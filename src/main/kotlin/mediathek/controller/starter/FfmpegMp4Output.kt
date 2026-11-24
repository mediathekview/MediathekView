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

import mediathek.daten.DatenDownload
import mediathek.tool.ProcessCommandUtils
import java.io.File
import java.io.IOException
import java.nio.file.Files
import java.nio.file.Path
import java.util.concurrent.ConcurrentHashMap
import java.util.concurrent.CountDownLatch
import java.util.concurrent.TimeUnit.SECONDS

internal class FfmpegMp4Output private constructor(
    private val target: File,
    private val arguments: List<String>,
    private val run: DownloadRunState,
) {
    val partFile: File = DirectDownloadPartFiles.partFileFor(target)
    var published: Boolean = false
        private set

    private var replaceExisting = false
    private var reservation: Path? = null
    private val released = CountDownLatch(1)

    fun approveOverwrite() {
        replaceExisting = true
    }

    fun prepareAttempt(preparedArguments: List<String>?): List<String>? {
        if (run.stoppen || (reservation == null && !reservePartFile())) return null
        val path = partFile.toPath()
        if (Files.isDirectory(path)) throw IOException("FFmpeg output is a directory: ${partFile.path}")
        if (run.stoppen) return null
        // FFmpeg starts a new MP4 on each attempt.
        Files.deleteIfExists(path)
        return (preparedArguments ?: arguments).dropLast(1) + listOf("-f", "mp4", partFile.path)
    }

    fun publish() {
        DirectDownloadPartFiles.moveCompletedPartToFinal(partFile, target, replaceExisting)
        published = true
    }

    fun releaseAfter(process: Process?) {
        val key = reservation ?: return
        val release = {
            activeParts.remove(key, this)
            released.countDown()
        }
        if (process != null && process.isAlive) {
            process.onExit().thenRun(release)
        } else {
            release()
        }
    }

    private fun reservePartFile(): Boolean {
        val path = partFile.toPath().toAbsolutePath()
        val key = path.parent.toRealPath().resolve(path.fileName)
        val previous = activeParts.putIfAbsent(key, this)
        if (previous != null) {
            if (!previous.run.stoppen || !previous.released.await(RELEASE_TIMEOUT_SECONDS, SECONDS)) {
                throw IOException("FFmpeg output already in use: ${partFile.path}")
            }
            if (run.stoppen) return false
            if (activeParts.putIfAbsent(key, this) != null) {
                throw IOException("FFmpeg output already in use: ${partFile.path}")
            }
        }
        reservation = key
        return true
    }

    companion object {
        private const val RELEASE_TIMEOUT_SECONDS = 5L
        private val programMapPattern = Regex("p:[0-9]+")
        private val copyOptions = listOf("-c", "copy", "-bsf:a", "aac_adtstoasc")
        private val activeParts = ConcurrentHashMap<Path, FfmpegMp4Output>()

        fun forDownload(download: DatenDownload, run: DownloadRunState): FfmpegMp4Output? {
            val target = File(download.targetPathFileName)
            if (download.isDownloadManager || !target.extension.equals("mp4", ignoreCase = true)) return null

            val args = download.programInvocationArray.split(RuntimeExec.TRENNER_PROG_ARRAY)
                .takeIf { it.size > 1 }
                ?: ProcessCommandUtils.tokenizeCommand(download.programInvocation).asList()
            if (args.size < 8 || args.last().removeSurrounding("\"") != download.targetPathFileName) return null
            val executable = args.first().substringAfterLast('/').substringAfterLast('\\')
            if (!executable.equals("ffmpeg", ignoreCase = true) && !executable.equals("ffmpeg.exe", ignoreCase = true)) return null

            val inputIndex = if (args.subList(1, 3) == listOf("-http_seekable", "0")) 3 else 1
            if (args[inputIndex] != "-i" || args[inputIndex + 1] != download.downloadUrl) return null
            val outputOptions = args.subList(inputIndex + 2, args.lastIndex)
            val automaticMap = download.automaticArteHlsProgramMap &&
                outputOptions.size == 8 && outputOptions[0] == "-map" &&
                outputOptions[1].matches(programMapPattern) &&
                outputOptions.subList(2, 4) == listOf("-map", "-0:s")
            val options = if (automaticMap) outputOptions.drop(4) else outputOptions
            return if (options == copyOptions) FfmpegMp4Output(target, args, run) else null
        }
    }
}
