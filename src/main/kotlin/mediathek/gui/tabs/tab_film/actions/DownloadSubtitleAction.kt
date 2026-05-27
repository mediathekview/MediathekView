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

package mediathek.gui.tabs.tab_film.actions

import kotlinx.coroutines.*
import kotlinx.coroutines.swing.Swing
import mediathek.config.Konstanten
import mediathek.daten.DatenFilm
import mediathek.mainwindow.MediathekGui
import mediathek.tool.FileDialogs
import mediathek.tool.FileUtils
import mediathek.tool.PathExtensions
import mediathek.tool.SwingErrorDialog
import mediathek.tool.subtitles.detector.TimedTextFormatDetector
import mediathek.tool.subtitles.ttml2.AssExporter
import mediathek.tool.subtitles.ttml2.SubRipHtmlExporter
import mediathek.tool.subtitles.ttml2.Ttml2Parser
import mediathek.tool.subtitles.vtt.WebVttToTtml2Converter
import java.awt.event.ActionEvent
import java.io.IOException
import java.nio.file.Files
import java.nio.file.Path
import java.util.Optional
import java.util.function.Supplier
import javax.swing.AbstractAction
import javax.swing.Action
import javax.swing.JOptionPane

class DownloadSubtitleAction(
    private val currentlySelectedFilm: Supplier<Optional<DatenFilm>>,
) : AbstractAction() {
    private val uiScope = CoroutineScope(SupervisorJob() + Dispatchers.Swing)

    init {
        putValue(Action.NAME, "Untertitel-Datei sofort laden...")
    }

    override fun actionPerformed(e: ActionEvent?) {
        val film = currentlySelectedFilm.get().orElse(null) ?: return
        val selectedFile = FileDialogs.chooseSaveFileLocation(MediathekGui.ui(), "Untertitel speichern", "")

        if (selectedFile == null) {
            JOptionPane.showMessageDialog(
                MediathekGui.ui(),
                "Vorgang wurde abgebrochen.",
                Konstanten.PROGRAMMNAME,
                JOptionPane.WARNING_MESSAGE
            )
            return
        }

        isEnabled = false
        uiScope.launch {
            val result = runCatching {
                withContext(Dispatchers.IO) {
                    exportSubtitleFiles(film.subtitleUrl, selectedFile.toPath())
                }
            }.getOrElse(SubtitleExportResult::Failure)

            when (result) {
                SubtitleExportResult.InvalidFormat -> {
                    JOptionPane.showMessageDialog(
                        MediathekGui.ui(),
                        "Untertitelformat konnte nicht erkannt werden.",
                        Konstanten.PROGRAMMNAME,
                        JOptionPane.ERROR_MESSAGE
                    )
                }

                SubtitleExportResult.UnsupportedFormat -> {
                    JOptionPane.showMessageDialog(
                        MediathekGui.ui(),
                        "Untertitelformat wird nicht unterstützt.",
                        Konstanten.PROGRAMMNAME,
                        JOptionPane.ERROR_MESSAGE
                    )
                }

                is SubtitleExportResult.Success -> {
                    JOptionPane.showMessageDialog(
                        MediathekGui.ui(),
                        buildCompletionMessage(result),
                        Konstanten.PROGRAMMNAME,
                        if (result.failures.isEmpty()) JOptionPane.INFORMATION_MESSAGE else JOptionPane.WARNING_MESSAGE
                    )
                }

                is SubtitleExportResult.Failure -> {
                    SwingErrorDialog.showExceptionMessage(
                        MediathekGui.ui(),
                        "Untertitel konnte nicht geladen werden.",
                        result.exception
                    )
                }
            }

            isEnabled = true
        }
    }

    private fun exportSubtitleFiles(subtitleUrl: String, selectedFilePath: Path): SubtitleExportResult {
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
            val generatedTtmlTempPath = if (detectedFormat == TimedTextFormatDetector.Format.WEBVTT) {
                runCatching {
                    val ttmlTempPath = createSiblingTempFile(ttmlTargetPath)
                    temporaryArtifacts.add(ttmlTempPath)
                    WebVttToTtml2Converter().convert(originalTempPath, ttmlTempPath)
                    ttmlTempPath
                }.getOrElse { conversionError ->
                    failures["TTML"] = conversionError
                    failures.putIfAbsent("SRT", conversionError)
                    failures.putIfAbsent("ASS", conversionError)
                    null
                }
            } else {
                null
            }
            val ttmlSourcePath = generatedTtmlTempPath ?: originalTempPath

            val subtitleDocument = runCatching {
                Ttml2Parser().parse(ttmlSourcePath)
            }.getOrElse { parseError ->
                failures.putIfAbsent("SRT", parseError)
                failures.putIfAbsent("ASS", parseError)
                null
            }

            subtitleDocument?.let {
                exportDerivedArtifact(
                    label = "SRT",
                    targetPath = PathExtensions.withExtension(ttmlTargetPath, ".srt"),
                    temporaryArtifacts = temporaryArtifacts,
                    successes = successes,
                    failures = failures
                ) { target ->
                    Files.writeString(target, SubRipHtmlExporter().export(it))
                }

                exportDerivedArtifact(
                    label = "ASS",
                    targetPath = PathExtensions.withExtension(ttmlTargetPath, ".ass"),
                    temporaryArtifacts = temporaryArtifacts,
                    successes = successes,
                    failures = failures
                ) { target ->
                    val assOptions = AssExporter.Options(384, 288, false)
                    Files.writeString(target, AssExporter(assOptions).export(it))
                }
            }

            generatedTtmlTempPath?.let {
                publishArtifact(
                    temporaryPath = it,
                    targetPath = ttmlTargetPath,
                    label = "TTML",
                    temporaryArtifacts = temporaryArtifacts,
                    successes = successes,
                    failures = failures
                )
            }

            publishArtifact(
                temporaryPath = originalTempPath,
                targetPath = originalTargetPath,
                label = displayNameFor(detectedFormat),
                temporaryArtifacts = temporaryArtifacts,
                successes = successes,
                failures = failures
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

    private fun exportDerivedArtifact(
        label: String,
        targetPath: Path,
        temporaryArtifacts: MutableList<Path>,
        successes: MutableList<String>,
        failures: MutableMap<String, Throwable>,
        writer: (Path) -> Unit
    ) {
        runCatching {
            val tempPath = createSiblingTempFile(targetPath)
            temporaryArtifacts.add(tempPath)
            writer(tempPath)
            publishArtifact(
                temporaryPath = tempPath,
                targetPath = targetPath,
                label = label,
                temporaryArtifacts = temporaryArtifacts,
                successes = successes,
                failures = failures
            )
        }.onFailure { exportError ->
            failures[label] = exportError
        }
    }

    private fun publishArtifact(
        temporaryPath: Path,
        targetPath: Path,
        label: String,
        temporaryArtifacts: MutableList<Path>,
        successes: MutableList<String>,
        failures: MutableMap<String, Throwable>
    ) {
        runCatching {
            FileUtils.moveAtomicallyWithFallback(temporaryPath, targetPath)
            temporaryArtifacts.remove(temporaryPath)
            successes += label
        }.onFailure { publishError ->
            failures[label] = publishError
        }
    }

    private fun buildCompletionMessage(result: SubtitleExportResult.Success): String {
        val successLine = "Erfolgreich erstellt: ${result.successes.joinToString(", ")}."
        if (result.failures.isEmpty()) {
            return successLine
        }

        val failureLine = result.failures.keys.joinToString(", ")
        return "$successLine\nFehlgeschlagen: $failureLine."
    }

    private fun displayNameFor(format: TimedTextFormatDetector.Format): String = when (format) {
        TimedTextFormatDetector.Format.WEBVTT -> "WebVTT"
        TimedTextFormatDetector.Format.TTML1,
        TimedTextFormatDetector.Format.TTML2 -> "TTML"
        TimedTextFormatDetector.Format.UNKNOWN -> "Unbekannt"
    }

    private fun targetPathForDetectedFormat(
        selectedFilePath: Path,
        detectedFormat: TimedTextFormatDetector.Format
    ): Path = when (detectedFormat) {
        TimedTextFormatDetector.Format.WEBVTT -> PathExtensions.withExtension(selectedFilePath, ".vtt")
        TimedTextFormatDetector.Format.TTML1,
        TimedTextFormatDetector.Format.TTML2 -> PathExtensions.withExtension(selectedFilePath, ".ttml")
        TimedTextFormatDetector.Format.UNKNOWN -> throw IOException("Unknown subtitle format: $detectedFormat")
    }

    private fun ttmlTargetPath(
        originalTargetPath: Path,
        detectedFormat: TimedTextFormatDetector.Format
    ): Path = if (detectedFormat == TimedTextFormatDetector.Format.WEBVTT) {
        PathExtensions.withExtension(originalTargetPath, ".ttml")
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

    private sealed interface SubtitleExportResult {
        data class Success(
            val successes: List<String>,
            val failures: Map<String, Throwable>
        ) : SubtitleExportResult
        data object InvalidFormat : SubtitleExportResult
        data object UnsupportedFormat : SubtitleExportResult
        data class Failure(val exception: Throwable) : SubtitleExportResult
    }
}
