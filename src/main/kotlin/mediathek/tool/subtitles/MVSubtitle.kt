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

package mediathek.tool.subtitles

import mediathek.daten.DatenDownload
import mediathek.tool.FileUtils
import mediathek.tool.PathExtensions
import mediathek.tool.subtitles.detector.TimedTextFormatDetector
import mediathek.tool.subtitles.ttml2.AssExporter
import mediathek.tool.subtitles.ttml2.SubRipHtmlExporter
import mediathek.tool.subtitles.ttml2.Ttml2Parser
import mediathek.tool.subtitles.vtt.WebVttToTtml2Converter
import org.apache.logging.log4j.LogManager
import java.io.IOException
import java.nio.file.Files
import java.nio.file.Path
import java.nio.file.Paths

class MVSubtitle {
    @Throws(Exception::class)
    private fun downloadAndConvertSubtitleFile(subtitleUrl: String, selectedFilePath: Path) {
        var tempSubtitleFile: Path? = null
        var currentPath = selectedFilePath

        try {
            tempSubtitleFile = FileUtils.downloadToTempFile(subtitleUrl)
            FileUtils.moveAtomicallyWithFallback(tempSubtitleFile, currentPath)

            val res = TimedTextFormatDetector.detect(currentPath, true)
            if (!res.valid) {
                throw IOException("Invalid subtitle format: ${res.format}")
            }

            if (res.format == TimedTextFormatDetector.Format.UNKNOWN) {
                throw IOException("Unknown subtitle format: ${res.format}")
            }

            currentPath = addFileExtension(currentPath, res.format)

            if (res.format == TimedTextFormatDetector.Format.WEBVTT) {
                val converter = WebVttToTtml2Converter()
                val newPath = PathExtensions.withExtension(currentPath, ".ttml")
                converter.convert(currentPath, newPath)
                currentPath = newPath
            }

            val parser = Ttml2Parser()
            val ttmlDoc = parser.parse(currentPath)

            val srtPath = PathExtensions.withExtension(currentPath, ".srt")
            val srtStr = SubRipHtmlExporter().export(ttmlDoc)
            Files.writeString(srtPath, srtStr)

            val assPath = PathExtensions.withExtension(currentPath, ".ass")
            val assOptions = AssExporter.Options(384, 288, false)
            val assStr = AssExporter(assOptions).export(ttmlDoc)
            Files.writeString(assPath, assStr)
        } finally {
            try {
                if (tempSubtitleFile != null) {
                    Files.deleteIfExists(tempSubtitleFile)
                }
            } catch (_: IOException) {
            }
        }
    }

    fun writeSubtitle(datenDownload: DatenDownload) {
        val urlSubtitle = datenDownload.arr[DatenDownload.DOWNLOAD_URL_SUBTITLE]
        if (urlSubtitle.isEmpty()) {
            return
        }

        val destinationPath = Paths.get(datenDownload.fileNameWithoutSuffix)
        try {
            downloadAndConvertSubtitleFile(urlSubtitle, destinationPath)
        } catch (e: Exception) {
            LogManager.getLogger().error("Error writing subtitle.", e)
        }
    }

    companion object {
        @Throws(IOException::class)
        fun addFileExtension(selectedFilePath: Path, format: TimedTextFormatDetector.Format): Path {
            return when (format) {
                TimedTextFormatDetector.Format.WEBVTT -> {
                    val path = PathExtensions.withExtension(selectedFilePath, ".vtt")
                    FileUtils.moveAtomicallyWithFallback(selectedFilePath, path)
                    path
                }

                TimedTextFormatDetector.Format.TTML1,
                TimedTextFormatDetector.Format.TTML2 -> {
                    val path = PathExtensions.withExtension(selectedFilePath, ".ttml")
                    FileUtils.moveAtomicallyWithFallback(selectedFilePath, path)
                    path
                }

                else -> throw IOException("Unknown subtitle format: $format")
            }
        }
    }
}
