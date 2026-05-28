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

package mediathek.gui.dialog

import kotlinx.coroutines.CancellationException
import kotlinx.coroutines.CoroutineScope
import kotlinx.coroutines.Dispatchers
import kotlinx.coroutines.SupervisorJob
import kotlinx.coroutines.cancel
import kotlinx.coroutines.launch
import kotlinx.coroutines.swing.Swing
import kotlinx.coroutines.withContext
import mediathek.config.Daten
import mediathek.config.Konstanten
import mediathek.config.MVConfig
import mediathek.config.StandardLocations
import mediathek.daten.DatenFilm
import mediathek.mainwindow.MediathekGui
import mediathek.tool.EscapeKeyHandler
import mediathek.tool.FileDialogs
import mediathek.tool.FilenameUtils
import mediathek.tool.GuiFunktionen
import mediathek.tool.MVInfoFile
import mediathek.tool.SVGIconUtilities
import mediathek.tool.SwingErrorDialog
import okhttp3.HttpUrl.Companion.toHttpUrlOrNull
import org.apache.logging.log4j.LogManager
import java.io.IOException
import java.nio.file.Path
import javax.swing.JFrame
import javax.swing.JOptionPane

class DialogFilmBeschreibung(
    parent: JFrame?,
    private val datenFilm: DatenFilm,
) : DialogFilmBeschreibungBase(parent) {
    private val dialogScope = CoroutineScope(SupervisorJob() + Dispatchers.Swing)

    init {
        title = TITLE
        if (parent != null) {
            setLocationRelativeTo(parent)
        }

        EscapeKeyHandler.installHandler(this) { dispose() }

        jTextArea1.text = datenFilm.description
        jTextFieldTitel.text = datenFilm.title

        jButtonOk.addActionListener {
            datenFilm.description = jTextArea1.text
            dispose()
        }

        jButtonHilfe.icon = SVGIconUtilities.createSVGIcon("icons/fontawesome/circle-question.svg")
        jButtonHilfe.addActionListener {
            JOptionPane.showMessageDialog(
                this,
                HELP_MESSAGE,
                Konstanten.PROGRAMMNAME,
                JOptionPane.INFORMATION_MESSAGE,
            )
        }

        jButtonSpeichern.addActionListener { saveInfoFile() }
    }

    override fun dispose() {
        dialogScope.cancel()
        super.dispose()
    }

    private fun saveInfoFile() {
        datenFilm.description = jTextArea1.text

        val destinationPath = buildDestinationPath()
        val destinationFile = FileDialogs.chooseSaveFileLocation(MediathekGui.ui(), "Infos speichern", destinationPath)
            ?: return
        val path = destinationFile.toPath()

        jButtonSpeichern.isEnabled = false
        dialogScope.launch {
            try {
                writeInfoFile(path)
                JOptionPane.showMessageDialog(
                    this@DialogFilmBeschreibung,
                    "Infodatei wurde erfolgreich geschrieben.",
                    Konstanten.PROGRAMMNAME,
                    JOptionPane.INFORMATION_MESSAGE,
                )
            } catch (ex: IOException) {
                SwingErrorDialog.showExceptionMessage(
                    this@DialogFilmBeschreibung,
                    "Ein unbekannter Fehler ist aufgetreten!",
                    ex,
                )
                logger.error("Ziel: {}", path.toAbsolutePath().toString(), ex)
            } catch (ex: CancellationException) {
                throw ex
            } finally {
                jButtonSpeichern.isEnabled = true
            }
        }
    }

    private suspend fun writeInfoFile(path: Path) = withContext(Dispatchers.IO) {
        MVInfoFile().writeInfoFile(datenFilm, path, datenFilm.urlNormalQuality.toHttpUrlOrNull())
    }

    private fun buildDestinationPath(): String {
        val title = FilenameUtils.replaceLeerDateiname(
            datenFilm.title,
            false,
            MVConfig.get(MVConfig.Configs.SYSTEM_USE_REPLACETABLE).toBoolean(),
            MVConfig.get(MVConfig.Configs.SYSTEM_ONLY_ASCII).toBoolean(),
        )
        val programSets = Daten.getInstance().listePset.listeSpeichern
        val targetPath = if (programSets.isEmpty()) {
            StandardLocations.getStandardDownloadPath()
        } else {
            programSets[0].zielPfad.ifEmpty { StandardLocations.getStandardDownloadPath() }
        }
        val fileName = if (title.isEmpty()) {
            "${datenFilm.sender.replace(" ", "-")}$SUFFIX"
        } else {
            "$title$SUFFIX"
        }

        return GuiFunktionen.addsPfad(targetPath, fileName)
    }

    private companion object {
        private val logger = LogManager.getLogger()
        private const val TITLE = "Beschreibung ändern"
        private const val SUFFIX = ".txt"
        private val HELP_MESSAGE = """
            Diese Funktion richtet sich z.B. an Benutzer,welche eine angepasste Beschreibung der Sendung in Form der Infodatei ("Filmname.txt") anlegen und durch Drittprogramme einlesen lassen wollen.

            Achtung: Diese Änderungen gehen nach dem Neuladen einer Filmliste verloren.
        """.trimIndent()
    }
}
