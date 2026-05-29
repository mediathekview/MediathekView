/*
 * Copyright (c) 2026 derreisende77.
 * This code was developed as part of the MediathekView project https://github.com/mediathekview/MediathekView
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package mediathek.controller.starter

import mediathek.config.Config
import mediathek.tool.MVFilmSize
import mediathek.tool.ProcessCommandUtils
import org.apache.logging.log4j.LogManager
import java.io.BufferedReader
import java.io.IOException
import java.io.InputStream
import java.io.InputStreamReader
import java.time.Duration
import java.time.LocalDateTime
import java.util.*

/**
 * Responsible for the interaction with ffmpeg/avconv.
 */
class RuntimeExec(
    private val mVFilmSize: MVFilmSize?,
    private val start: DownloadRunState?,
    private val strProgCall: String,
    strProgCallArray: String,
) {
    private val progressTracker = ProgressTracker()
    private val arrProgCallArray = strProgCallArray
        .split(TRENNER_PROG_ARRAY)
        .takeIf { it.size > 1 }
        ?.toTypedArray()
    private val strProgCallArray = strProgCallArray

    constructor(programCall: String) : this(null, null, programCall, "")

    fun exec(log: Boolean): Process? =
        runCatching {
            val process = if (arrProgCallArray != null) {
                if (log) {
                    logOutput(strProgCallArray, isArray = true)
                }
                ProcessBuilder(*arrProgCallArray).start()
            } else {
                if (log) {
                    logOutput(strProgCall, isArray = false)
                }
                ProcessBuilder(*ProcessCommandUtils.tokenizeCommand(strProgCall)).start()
            }

            startStreamConsumer(process, IoType.INPUT)
            startStreamConsumer(process, IoType.ERROR)
            process
        }.getOrElse { ex ->
            logger.error("Fehler beim Starten", ex)
            null
        }

    private fun logOutput(command: String, isArray: Boolean) {
        logger.info("=====================")
        logger.info(if (isArray) "Starte Array: " else "Starte nicht als Array:")
        logger.info(" -> {}", command)
        logger.info("=====================")
    }

    private fun startStreamConsumer(process: Process, ioType: IoType) {
        Thread.ofVirtual()
            .name("RuntimeExec ffmpeg stream consumer type $ioType for pid ${process.pid()}")
            .start { consumeStream(process, ioType) }
    }

    private fun consumeStream(process: Process, ioType: IoType) {
        val streamContext = createStreamContext(process, ioType)

        try {
            BufferedReader(InputStreamReader(streamContext.stream)).use { reader ->
                reader.lineSequence().forEach { line ->
                    if (streamContext.parseProgress) {
                        progressTracker.acceptErrorLine(line)
                    }
                    if (Config.isEnhancedLoggingEnabled()) {
                        logger.trace("  >> {}: {}", streamContext.title, line)
                    }
                }
            }
        } catch (ex: IOException) {
            logger.error("Error while consuming {} for pid {}", ioType, process.pid(), ex)
        }
    }

    private fun createStreamContext(process: Process, ioType: IoType): StreamContext =
        when (ioType) {
            IoType.INPUT -> StreamContext("INPUTSTREAM", process.inputStream, parseProgress = false)
            IoType.ERROR -> StreamContext("ERRORSTREAM [${process.pid()}]", process.errorStream, parseProgress = true)
        }

    private fun canTrackProgress(): Boolean = start?.startTime != null && mVFilmSize != null

    private fun secondsSinceStart(): Long =
        Duration.between(start!!.startTime, LocalDateTime.now()).toSeconds()

    private inner class ProgressTracker {
        private var totalSecs = .0
        private var oldSizeBytes = 0L
        private var oldBandwidthSampleSecs = 0L
        private var percent = -1
        private var percentStart = -1

        fun acceptErrorLine(input: String) {
            if (!canTrackProgress()) {
                return
            }

            // ffmpeg progress and diagnostics are emitted on stderr.
            runCatching {
                parseDurationSeconds(input).ifPresent { duration -> totalSecs = duration }
                parseSizeBytes(input).ifPresent(::updateTransferredBytes)
                parseProgressTimeSeconds(input).ifPresent(::updateProgressSeconds)
            }.onFailure { ex ->
                DownloadProgressEventPublisher.publishThrottled()
                logger.error("Failed to parse ffmpeg output line: {}", input, ex)
            }
        }

        private fun updateTransferredBytes(sizeBytes: Long) {
            val currentFilmSize = mVFilmSize ?: return
            val currentStart = start ?: return
            currentFilmSize.setAktSize(sizeBytes)

            val elapsedSecs = secondsSinceStart()
            if (oldBandwidthSampleSecs < elapsedSecs - 5) {
                currentStart.updateBandwidth((sizeBytes - oldSizeBytes) / (elapsedSecs - oldBandwidthSampleSecs))
                oldBandwidthSampleSecs = elapsedSecs
                oldSizeBytes = sizeBytes
            }
        }

        private fun updateProgressSeconds(progressSeconds: Double) {
            if (totalSecs <= 0) {
                return
            }

            updatePercent(progressSeconds / totalSecs * 100)
        }

        private fun updatePercent(percentValue: Double) {
            val currentStart = start ?: return

            // nur ganze Int speichern, und 1000 Schritte
            val newPercent = (percentValue * 10).toInt()
            currentStart.updateProgress(newPercent)
            if (newPercent == percent) {
                return
            }

            percent = newPercent
            if (percentStart == -1) {
                // fuer wiedergestartete Downloads
                percentStart = percent
            }
            if (percent > percentStart + 5) {
                // sonst macht es noch keinen Sinn
                val elapsedSecs = secondsSinceStart()
                val progressed = percent - percentStart
                val remaining = 1000 - percent
                currentStart.updateRemainingSeconds(elapsedSecs * remaining / progressed)
            }
            DownloadProgressEventPublisher.publishThrottled()
        }
    }

    private enum class IoType { INPUT, ERROR }

    private data class StreamContext(
        val title: String,
        val stream: InputStream,
        val parseProgress: Boolean,
    )

    companion object {
        const val TRENNER_PROG_ARRAY = "<>"

        private val PATTERN_FFMPEG = Regex("(?<= {2}Duration: )[^,]*")
        private val PATTERN_TIME = Regex("(?<=time=)[^ ]*")
        private val PATTERN_SIZE = Regex("(?<=size=)\\s*\\d+(?:\\.\\d+)?\\s*[KMG]?i?B", RegexOption.IGNORE_CASE)
        private val logger = LogManager.getLogger()

        fun parseDurationSeconds(input: String): OptionalDouble =
            PATTERN_FFMPEG.find(input)
                ?.value
                ?.trim()
                ?.let(::parseTimeSeconds)
                ?: OptionalDouble.empty()

        fun parseTimeSeconds(timeToken: String?): OptionalDouble {
            if (timeToken.isNullOrBlank()) {
                return OptionalDouble.empty()
            }

            return runCatching {
                val trimmed = timeToken.trim()
                if (':' in trimmed) {
                    val hms = trimmed.split(':')
                    if (hms.size != 3) {
                        return OptionalDouble.empty()
                    }

                    OptionalDouble.of(
                        hms[0].toInt() * 3600 +
                            hms[1].toInt() * 60 +
                            hms[2].toDouble(),
                    )
                } else {
                    OptionalDouble.of(trimmed.toDouble())
                }
            }.getOrElse {
                OptionalDouble.empty()
            }
        }

        fun parseProgressTimeSeconds(input: String): OptionalDouble =
            PATTERN_TIME.find(input)
                ?.value
                ?.trim()
                ?.let(::parseTimeSeconds)
                ?: OptionalDouble.empty()

        fun parseSizeBytes(input: String): OptionalLong {
            val sizeToken = PATTERN_SIZE.find(input)?.value?.trim() ?: return OptionalLong.empty()
            val unitStart = sizeToken.indexOfFirst { !it.isDigit() && it != '.' }
            if (unitStart <= 0 || unitStart >= sizeToken.length) {
                return OptionalLong.empty()
            }

            return runCatching {
                val value = sizeToken.substring(0, unitStart).toDouble()
                val unit = sizeToken.substring(unitStart).trim().uppercase(Locale.ROOT)
                val multiplier = when (unit) {
                    "B" -> 1L
                    "KB" -> 1_000L
                    "KIB" -> 1_024L
                    "MB" -> 1_000_000L
                    "MIB" -> 1_048_576L
                    "GB" -> 1_000_000_000L
                    "GIB" -> 1_073_741_824L
                    else -> return OptionalLong.empty()
                }

                OptionalLong.of(kotlin.math.round(value * multiplier).toLong())
            }.getOrElse {
                OptionalLong.empty()
            }
        }
    }
}
