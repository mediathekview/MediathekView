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

import kotlinx.coroutines.*
import mediathek.daten.DatenDownload
import mediathek.tool.MVInfoFile
import mediathek.tool.subtitles.SubtitleExportResult
import mediathek.tool.subtitles.SubtitleExportService
import org.apache.logging.log4j.Logger
import java.io.IOException
import java.nio.file.Paths
import java.time.Duration

internal class DirectDownloadAncillaryFiles private constructor(
    private val jobs: List<Deferred<Unit>>,
    private val logger: Logger
) {

    suspend fun await() {
        try {
            for (job in jobs) {
                job.await()
            }
        } catch (_: CancellationException) {
            throw CancellationException()
        } catch (ex: Exception) {
            logger.error("awaitAncillaryDownloads().", ex)
        }
    }

    companion object {
        fun empty(logger: Logger): DirectDownloadAncillaryFiles {
            return DirectDownloadAncillaryFiles(emptyList(), logger)
        }

        fun start(scope: CoroutineScope, datenDownload: DatenDownload, logger: Logger): DirectDownloadAncillaryFiles {
            val infoJob = if (datenDownload.isInfoFile) {
                scope.async(Dispatchers.IO) {
                    try {
                        MVInfoFile().writeInfoFile(datenDownload)
                    } catch (ex: IOException) {
                        logger.error("Failed to write info file", ex)
                    }
                }
            } else {
                null
            }

            val subtitleJob = if (datenDownload.isSubtitle) {
                scope.async {
                    writeSubtitleFile(datenDownload, logger)
                }
            } else {
                null
            }

            return DirectDownloadAncillaryFiles(listOfNotNull(infoJob, subtitleJob), logger)
        }

        private suspend fun writeSubtitleFile(datenDownload: DatenDownload, logger: Logger) {
            val subtitleUrl = datenDownload.subtitleUrl
            if (subtitleUrl.isEmpty()) {
                return
            }

            val destinationPath = Paths.get(datenDownload.fileNameWithoutSuffix)
            val filmDuration = datenDownload.film?.filmLength
                ?.takeIf { it > 0 }
                ?.let { Duration.ofSeconds(it.toLong()) }
                ?: parseDuration(datenDownload.duration)
            when (val result = SubtitleExportService.downloadAndExport(subtitleUrl, destinationPath, filmDuration)) {
                SubtitleExportResult.InvalidFormat -> logger.error("Invalid subtitle format.")
                SubtitleExportResult.UnsupportedFormat -> logger.error("Unsupported subtitle format.")
                is SubtitleExportResult.Failure -> logger.error("Failed to write subtitle file", result.exception)
                is SubtitleExportResult.Success -> {
                    if (result.failures.isNotEmpty()) {
                        logger.warn(
                            "Subtitle export partially failed: {}",
                            result.failures.keys.joinToString(", ")
                        )
                    }
                }
            }
        }

        private fun parseDuration(value: String): Duration? {
            val parts = value.split(':')
            if (parts.size != 3) {
                return null
            }
            val hours = parts[0].toLongOrNull() ?: return null
            val minutes = parts[1].toLongOrNull() ?: return null
            val seconds = parts[2].toLongOrNull() ?: return null
            if (hours < 0 || minutes !in 0..59 || seconds !in 0..59) {
                return null
            }
            return Duration.ofHours(hours).plusMinutes(minutes).plusSeconds(seconds)
        }
    }
}
