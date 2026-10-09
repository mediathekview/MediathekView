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

package mediathek.sqlite

import kotlinx.coroutines.*
import kotlinx.coroutines.swing.Swing
import mediathek.config.Konstanten
import mediathek.tool.sql.SqlDatabaseConfig
import java.awt.Desktop
import java.awt.Dimension
import java.awt.Frame
import java.awt.event.ActionEvent
import java.net.URISyntaxException
import java.nio.file.Files
import java.nio.file.Path
import javax.swing.AbstractAction
import javax.swing.JEditorPane
import javax.swing.JOptionPane
import javax.swing.event.HyperlinkEvent
import kotlin.io.path.name

class RecoverHistoryDbAction(private val owner: Frame) : AbstractAction() {
    private val recoverService = SqliteRecoverService()
    private val uiScope = CoroutineScope(SupervisorJob() + Dispatchers.Swing)

    init {
        putValue(NAME, "History-Datenbank wiederherstellen...")
    }

    override fun actionPerformed(e: ActionEvent?) {
        val sourceDatabase = SqlDatabaseConfig.historyDbPath.toAbsolutePath().normalize()
        if (Files.notExists(sourceDatabase)) {
            JOptionPane.showMessageDialog(
                owner,
                "Die Verlaufdatenbank existiert nicht:\n$sourceDatabase",
                Konstanten.PROGRAMMNAME,
                JOptionPane.ERROR_MESSAGE
            )
            return
        }

        val targetDatabase = sourceDatabase.resolveSibling(suggestedTarget(sourceDatabase))
        isEnabled = false
        val progressDialog = SqliteRecoveryProgressDialog(owner)
        val job = uiScope.launch {
            try {
                val summary = recoverService.recover(sourceDatabase, targetDatabase) { progress ->
                    withContext(Dispatchers.Swing) {
                        progressDialog.updateProgress(progress.message, progress.indeterminate)
                    }
                }
                JOptionPane.showMessageDialog(
                    owner,
                    "Wiederherstellung abgeschlossen.\n" +
                        "Quelle:\n${summary.sourceDatabase}\n\n" +
                        "Ziel:\n${summary.targetDatabase}\n\n" +
                        "Entfernte Duplikate:\n${summary.duplicateCount}\n\n" +
                        "Beenden Sie MediathekView, löschen Sie die alte Datenbank und benennen die wiederhergestellte Datenbank um.",
                    Konstanten.PROGRAMMNAME,
                    JOptionPane.INFORMATION_MESSAGE
                )
            } catch (_: CancellationException) {
                JOptionPane.showMessageDialog(
                    owner,
                    "SQLite-Recovery wurde abgebrochen.",
                    Konstanten.PROGRAMMNAME,
                    JOptionPane.INFORMATION_MESSAGE
                )
            } catch (ex: Exception) {
                JOptionPane.showMessageDialog(
                    owner,
                    buildDialogMessage(ex.message ?: "SQLite-Recovery ist fehlgeschlagen."),
                    Konstanten.PROGRAMMNAME,
                    JOptionPane.ERROR_MESSAGE
                )
            } finally {
                progressDialog.dispose()
                isEnabled = true
            }
        }

        progressDialog.onCancel = { job.cancel() }
        progressDialog.isVisible = true
    }

    private fun suggestedTarget(sourceDatabase: Path): String {
        val fileName = sourceDatabase.fileName.name
        val dotIndex = fileName.lastIndexOf('.')
        return if (dotIndex >= 0) {
            fileName.substring(0, dotIndex) + "-recovered" + fileName.substring(dotIndex)
        } else {
            "$fileName-recovered"
        }
    }

    private fun buildDialogMessage(message: String): Any {
        if (!message.trimStart().startsWith("<html>", ignoreCase = true)) {
            return message
        }

        return JEditorPane("text/html", message).apply {
            isEditable = false
            isOpaque = false
            border = null
            putClientProperty(JEditorPane.HONOR_DISPLAY_PROPERTIES, true)
            preferredSize = Dimension(460, preferredSize.height)
            addHyperlinkListener { event ->
                if (event.eventType != HyperlinkEvent.EventType.ACTIVATED || event.url == null) {
                    return@addHyperlinkListener
                }
                openLink(event.url.toString())
            }
        }
    }

    private fun openLink(url: String) {
        if (!Desktop.isDesktopSupported()) {
            return
        }

        val desktop = Desktop.getDesktop()
        if (!desktop.isSupported(Desktop.Action.BROWSE)) {
            return
        }

        try {
            desktop.browse(java.net.URI(url))
        } catch (_: URISyntaxException) {
        } catch (_: Exception) {
        }
    }
}
