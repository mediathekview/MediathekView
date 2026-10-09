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

import kotlinx.coroutines.Dispatchers
import kotlinx.coroutines.withContext
import mediathek.tool.FileUtils
import mediathek.tool.PathExtensions
import mediathek.tool.subtitles.detector.TimedTextFormatDetector
import mediathek.tool.subtitles.ttml2.AssExporter
import mediathek.tool.subtitles.ttml2.SubRipHtmlExporter
import mediathek.tool.subtitles.ttml2.Ttml2Parser
import mediathek.tool.subtitles.vtt.WebVttToTtml2Converter
import java.io.IOException
import java.nio.file.Files
import java.nio.file.Path
import java.time.Duration
import kotlin.coroutines.cancellation.CancellationException

object SubtitleExportService {
    suspend fun downloadAndExport(
        subtitleUrl: String,
        selectedFilePath: Path,
        filmDuration: Duration? = null,
    ): SubtitleExportResult =
        withContext(Dispatchers.IO) {
            try {
                doDownloadAndExport(subtitleUrl, selectedFilePath, filmDuration)
            } catch (ex: CancellationException) {
                throw ex
            } catch (ex: Exception) {
                SubtitleExportResult.Failure(ex)
            }
        }

    private fun doDownloadAndExport(
        subtitleUrl: String,
        selectedFilePath: Path,
        filmDuration: Duration?,
    ): SubtitleExportResult {
        var downloadedSubtitlePath: Path? = null
        val temporaryArtifacts = mutableListOf<Path>()
        val successes = mutableListOf<String>()
        val failures = LinkedHashMap<String, Throwable>()

        try {
            downloadedSubtitlePath = FileUtils.downloadToTempFile(subtitleUrl)

            val detection = TimedTextFormatDetector.detect(downloadedSubtitlePath, true)
            if (!detection.valid) {
                return SubtitleExportResult.InvalidFormat
            }

            val detectedFormat = detection.format
            if (detectedFormat == TimedTextFormatDetector.Format.UNKNOWN) {
                return SubtitleExportResult.UnsupportedFormat
            }

            val originalTargetPath = targetPathForDetectedFormat(selectedFilePath, detectedFormat)
            val originalTempPath = createSiblingTempFile(originalTargetPath)
            temporaryArtifacts.add(originalTempPath)
            FileUtils.moveAtomicallyWithFallback(downloadedSubtitlePath, originalTempPath)
            downloadedSubtitlePath = null

            val ttmlTargetPath = ttmlTargetPath(originalTargetPath, detectedFormat)
            val generatedTtmlTempPath = prepareGeneratedTtmlArtifact(
                originalTempPath = originalTempPath,
                ttmlTargetPath = ttmlTargetPath,
                detectedFormat = detectedFormat,
                temporaryArtifacts = temporaryArtifacts,
                failures = failures,
            )

            val subtitleDocument = parseSubtitleDocument(
                generatedTtmlArtifact = generatedTtmlTempPath,
                originalTempPath = originalTempPath,
                filmDuration = filmDuration,
                failures = failures,
            )

            subtitleDocument?.let { document ->
                exportDerivedArtifacts(
                    document = document,
                    ttmlTargetPath = ttmlTargetPath,
                    temporaryArtifacts = temporaryArtifacts,
                    successes = successes,
                    failures = failures,
                )
            }

            generatedTtmlTempPath?.let {
                publishArtifact(
                    temporaryPath = it.path,
                    targetPath = ttmlTargetPath,
                    label = SubtitleArtifact.TTML.displayName,
                    temporaryArtifacts = temporaryArtifacts,
                    successes = successes,
                    failures = failures,
                )
            }

            publishArtifact(
                temporaryPath = originalTempPath,
                targetPath = originalTargetPath,
                label = displayNameFor(detectedFormat),
                temporaryArtifacts = temporaryArtifacts,
                successes = successes,
                failures = failures,
            )

            return if (successes.isNotEmpty()) {
                SubtitleExportResult.Success(successes = successes.toList(), failures = failures.toMap())
            } else {
                SubtitleExportResult.Failure(failures.values.firstOrNull() ?: IOException("Subtitle export failed."))
            }
        } finally {
            downloadedSubtitlePath?.deleteQuietly()
            temporaryArtifacts.forEach { it.deleteQuietly() }
        }
    }

    private fun prepareGeneratedTtmlArtifact(
        originalTempPath: Path,
        ttmlTargetPath: Path,
        detectedFormat: TimedTextFormatDetector.Format,
        temporaryArtifacts: MutableList<Path>,
        failures: MutableMap<String, Throwable>,
    ): GeneratedTtmlArtifact? {
        if (detectedFormat != TimedTextFormatDetector.Format.WEBVTT) {
            return null
        }

        return runCatching {
            val ttmlTempPath = createSiblingTempFile(ttmlTargetPath)
            temporaryArtifacts.add(ttmlTempPath)
            val ttmlContent = WebVttToTtml2Converter().convertToString(originalTempPath)
            Files.writeString(ttmlTempPath, ttmlContent)
            GeneratedTtmlArtifact(ttmlTempPath, ttmlContent)
        }.getOrElse { conversionError ->
            failures[SubtitleArtifact.TTML.displayName] = conversionError
            failures.recordDerivedArtifactFailures(conversionError)
            null
        }
    }

    private fun parseSubtitleDocument(
        generatedTtmlArtifact: GeneratedTtmlArtifact?,
        originalTempPath: Path,
        filmDuration: Duration?,
        failures: MutableMap<String, Throwable>,
    ): SubtitleDocument? =
        runCatching {
            generatedTtmlArtifact?.content?.let { content ->
                filmDuration?.let { Ttml2Parser().parse(content, it) } ?: Ttml2Parser().parse(content)
            } ?: filmDuration?.let { Ttml2Parser().parseAndCorrect(originalTempPath, it) }
                ?: Ttml2Parser().parse(originalTempPath)
        }.getOrElse { parseError ->
            failures.recordDerivedArtifactFailures(parseError)
            null
        }

    private fun exportDerivedArtifacts(
        document: SubtitleDocument,
        ttmlTargetPath: Path,
        temporaryArtifacts: MutableList<Path>,
        successes: MutableList<String>,
        failures: MutableMap<String, Throwable>,
    ) {
        DerivedSubtitleArtifact.entries.forEach { artifact ->
            exportDerivedArtifact(
                artifact = artifact,
                targetPath = PathExtensions.withExtension(ttmlTargetPath, artifact.extension),
                temporaryArtifacts = temporaryArtifacts,
                successes = successes,
                failures = failures,
                document = document,
            )
        }
    }

    private fun exportDerivedArtifact(
        artifact: DerivedSubtitleArtifact,
        targetPath: Path,
        temporaryArtifacts: MutableList<Path>,
        successes: MutableList<String>,
        failures: MutableMap<String, Throwable>,
        document: SubtitleDocument,
    ) {
        runCatching {
            val tempPath = createSiblingTempFile(targetPath)
            temporaryArtifacts.add(tempPath)
            artifact.write(document, tempPath)
            publishArtifact(
                temporaryPath = tempPath,
                targetPath = targetPath,
                label = artifact.displayName,
                temporaryArtifacts = temporaryArtifacts,
                successes = successes,
                failures = failures,
            )
        }.onFailure { exportError ->
            failures[artifact.displayName] = exportError
        }
    }

    private fun publishArtifact(
        temporaryPath: Path,
        targetPath: Path,
        label: String,
        temporaryArtifacts: MutableList<Path>,
        successes: MutableList<String>,
        failures: MutableMap<String, Throwable>,
    ) {
        runCatching {
            FileUtils.moveAtomicallyWithFallback(temporaryPath, targetPath)
            temporaryArtifacts.remove(temporaryPath)
            successes += label
        }.onFailure { publishError ->
            failures[label] = publishError
        }
    }

    private fun displayNameFor(format: TimedTextFormatDetector.Format): String = when (format) {
        TimedTextFormatDetector.Format.WEBVTT -> SubtitleArtifact.WEBVTT.displayName
        TimedTextFormatDetector.Format.TTML1,
        TimedTextFormatDetector.Format.TTML2 -> SubtitleArtifact.TTML.displayName
        TimedTextFormatDetector.Format.UNKNOWN -> "Unbekannt"
    }

    private fun targetPathForDetectedFormat(
        selectedFilePath: Path,
        detectedFormat: TimedTextFormatDetector.Format,
    ): Path = when (detectedFormat) {
        TimedTextFormatDetector.Format.WEBVTT -> PathExtensions.withExtension(
            selectedFilePath,
            SubtitleArtifact.WEBVTT.extension,
        )
        TimedTextFormatDetector.Format.TTML1,
        TimedTextFormatDetector.Format.TTML2 -> PathExtensions.withExtension(selectedFilePath, SubtitleArtifact.TTML.extension)
        TimedTextFormatDetector.Format.UNKNOWN -> throw IOException("Unknown subtitle format: $detectedFormat")
    }

    private fun ttmlTargetPath(
        originalTargetPath: Path,
        detectedFormat: TimedTextFormatDetector.Format,
    ): Path = if (detectedFormat == TimedTextFormatDetector.Format.WEBVTT) {
        PathExtensions.withExtension(originalTargetPath, SubtitleArtifact.TTML.extension)
    } else {
        originalTargetPath
    }

    private fun createSiblingTempFile(targetPath: Path): Path {
        val absoluteTarget = targetPath.toAbsolutePath()
        val parent = absoluteTarget.parent ?: throw IOException("Path has no parent: $targetPath")
        val fileName = absoluteTarget.fileName?.toString() ?: throw IOException("Path has no filename: $targetPath")
        return Files.createTempFile(parent, "$fileName.", ".tmp")
    }

    private fun Path.deleteQuietly() {
        try {
            Files.deleteIfExists(this)
        } catch (_: IOException) {
        }
    }

    private fun MutableMap<String, Throwable>.recordDerivedArtifactFailures(error: Throwable) {
        DerivedSubtitleArtifact.entries.forEach { artifact ->
            putIfAbsent(artifact.displayName, error)
        }
    }

    private data class GeneratedTtmlArtifact(
        val path: Path,
        val content: String,
    )

    private enum class SubtitleArtifact(
        val displayName: String,
        val extension: String,
    ) {
        TTML("TTML", ".ttml"),
        WEBVTT("WebVTT", ".vtt"),
        SRT("SRT", ".srt"),
        ASS("ASS", ".ass"),
    }

    private enum class DerivedSubtitleArtifact(
        private val artifact: SubtitleArtifact,
    ) {
        SRT(SubtitleArtifact.SRT) {
            override fun write(document: SubtitleDocument, targetPath: Path) {
                Files.writeString(targetPath, SubRipHtmlExporter().export(document))
            }
        },
        ASS(SubtitleArtifact.ASS) {
            override fun write(document: SubtitleDocument, targetPath: Path) {
                val assOptions = AssExporter.Options(384, 288, false)
                Files.writeString(targetPath, AssExporter(assOptions).export(document))
            }
        };

        val displayName: String
            get() = artifact.displayName

        val extension: String
            get() = artifact.extension

        abstract fun write(document: SubtitleDocument, targetPath: Path)
    }
}

sealed interface SubtitleExportResult {
    data class Success(
        val successes: List<String>,
        val failures: Map<String, Throwable>,
    ) : SubtitleExportResult

    data object InvalidFormat : SubtitleExportResult
    data object UnsupportedFormat : SubtitleExportResult
    data class Failure(val exception: Throwable) : SubtitleExportResult
}
