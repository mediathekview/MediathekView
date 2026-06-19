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

import kotlinx.coroutines.CoroutineScope
import kotlinx.coroutines.Dispatchers
import kotlinx.coroutines.SupervisorJob
import kotlinx.coroutines.launch
import kotlinx.coroutines.swing.Swing
import mediathek.config.Konstanten
import mediathek.daten.DatenFilm
import mediathek.tool.FileDialogs
import mediathek.tool.SwingErrorDialog
import mediathek.tool.subtitles.SubtitleExportResult
import mediathek.tool.subtitles.SubtitleExportService
import java.awt.Frame
import java.awt.event.ActionEvent
import java.util.*
import java.util.function.Supplier
import javax.swing.AbstractAction
import javax.swing.Action
import javax.swing.JOptionPane

class DownloadSubtitleAction(
    private val owner: Frame,
    private val currentlySelectedFilm: Supplier<Optional<DatenFilm>>,
) : AbstractAction() {
    private val uiScope = CoroutineScope(SupervisorJob() + Dispatchers.Swing)

    init {
        putValue(Action.NAME, "Untertitel-Datei sofort laden...")
    }

    override fun actionPerformed(e: ActionEvent?) {
        val film = currentlySelectedFilm.get().orElse(null) ?: return
        val selectedFile = FileDialogs.chooseSaveFileLocation(owner, "Untertitel speichern", "")

        if (selectedFile == null) {
            JOptionPane.showMessageDialog(
                owner,
                "Vorgang wurde abgebrochen.",
                Konstanten.PROGRAMMNAME,
                JOptionPane.WARNING_MESSAGE
            )
            return
        }

        isEnabled = false
        uiScope.launch {
            try {
                val result = SubtitleExportService.downloadAndExport(film.subtitleUrl, selectedFile.toPath())

                when (result) {
                    SubtitleExportResult.InvalidFormat -> {
                        JOptionPane.showMessageDialog(
                            owner,
                            "Untertitelformat konnte nicht erkannt werden.",
                            Konstanten.PROGRAMMNAME,
                            JOptionPane.ERROR_MESSAGE
                        )
                    }

                    SubtitleExportResult.UnsupportedFormat -> {
                        JOptionPane.showMessageDialog(
                            owner,
                            "Untertitelformat wird nicht unterstützt.",
                            Konstanten.PROGRAMMNAME,
                            JOptionPane.ERROR_MESSAGE
                        )
                    }

                    is SubtitleExportResult.Success -> {
                        JOptionPane.showMessageDialog(
                            owner,
                            buildCompletionMessage(result),
                            Konstanten.PROGRAMMNAME,
                            if (result.failures.isEmpty()) JOptionPane.INFORMATION_MESSAGE else JOptionPane.WARNING_MESSAGE
                        )
                    }

                    is SubtitleExportResult.Failure -> {
                        SwingErrorDialog.showExceptionMessage(
                            owner,
                            "Untertitel konnte nicht geladen werden.",
                            result.exception
                        )
                    }
                }
            } finally {
                isEnabled = true
            }
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

}
