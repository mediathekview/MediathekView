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

package mediathek.gui.dialogEinstellungen

import mediathek.config.Konstanten
import mediathek.config.application.ApplicationConfiguration
import mediathek.gui.dialog.HelpTextDialog
import mediathek.tool.GetFile
import mediathek.tool.GuiFunktionenProgramme
import mediathek.tool.SVGIconUtilities
import org.apache.commons.lang3.SystemUtils
import org.apache.logging.log4j.LogManager
import java.awt.Color
import java.awt.FileDialog
import java.io.File
import javax.swing.JFileChooser
import javax.swing.JFrame
import javax.swing.JTextField
import javax.swing.UIManager
import javax.swing.event.DocumentEvent
import javax.swing.event.DocumentListener

class PanelProgrammPfade(
    private val parentComponent: JFrame?,
    private val showVlcSettings: Boolean,
    private val showFFmpegSettings: Boolean,
) : PanelProgrammPfadeBase() {
    init {
        configureComponentMetadata()
        initializePanel()
        installListeners()
    }

    private fun configureComponentMetadata() {
        jPanelVlc.name = PanelProgrammPfadeComponentNames.VLC_PANEL
        jTextFieldVlc.name = PanelProgrammPfadeComponentNames.VLC_PATH
        jButtonVlcPfad.name = PanelProgrammPfadeComponentNames.CHOOSE_VLC
        jButtonVlcSuchen.name = PanelProgrammPfadeComponentNames.SEARCH_VLC
        jButtonHilfe.name = PanelProgrammPfadeComponentNames.HELP
        jPanelFFmpeg.name = PanelProgrammPfadeComponentNames.FFMPEG_PANEL
        jTextFieldFFmpeg.name = PanelProgrammPfadeComponentNames.FFMPEG_PATH
        jButtonFFmpegSuchen.name = PanelProgrammPfadeComponentNames.SEARCH_FFMPEG
        jButtonFFmpegPfad.name = PanelProgrammPfadeComponentNames.CHOOSE_FFMPEG
    }

    private fun initializePanel() {
        jButtonVlcPfad.icon = SVGIconUtilities.createSVGIcon("icons/fontawesome/folder-open.svg")
        jButtonFFmpegPfad.icon = SVGIconUtilities.createSVGIcon("icons/fontawesome/folder-open.svg")
        jButtonHilfe.icon = SVGIconUtilities.createSVGIcon("icons/fontawesome/circle-question.svg")
        jPanelVlc.isVisible = showVlcSettings
        jPanelFFmpeg.isVisible = showFFmpegSettings

        val config = ApplicationConfiguration.getInstance()
        if (config.standardVlcPath.isEmpty()) {
            config.standardVlcPath = GuiFunktionenProgramme.getMusterPfadVlc()
        }
        if (config.standardFFmpegPath.isEmpty()) {
            config.standardFFmpegPath = GuiFunktionenProgramme.getMusterPfadFFmpeg()
        }
        jTextFieldVlc.text = config.standardVlcPath
        jTextFieldFFmpeg.text = config.standardFFmpegPath
    }

    private fun installListeners() {
        jTextFieldVlc.document.addDocumentListener(ProgramPathDocumentListener())
        jTextFieldFFmpeg.document.addDocumentListener(ProgramPathDocumentListener())

        jButtonVlcPfad.addActionListener { chooseProgramFile(jTextFieldVlc) }
        jButtonFFmpegPfad.addActionListener { chooseProgramFile(jTextFieldFFmpeg) }
        jButtonVlcSuchen.addActionListener {
            ApplicationConfiguration.getInstance().standardVlcPath = ""
            jTextFieldVlc.text = GuiFunktionenProgramme.getMusterPfadVlc()
        }
        jButtonFFmpegSuchen.addActionListener {
            ApplicationConfiguration.getInstance().standardFFmpegPath = ""
            jTextFieldFFmpeg.text = GuiFunktionenProgramme.getMusterPfadFFmpeg()
        }
        jButtonHilfe.addActionListener {
            HelpTextDialog.show(
                parentComponent,
                GetFile.getHilfeSuchen(Konstanten.PFAD_HILFETEXT_STANDARD_PSET),
            )
        }
    }

    private fun checkPaths() {
        val config = ApplicationConfiguration.getInstance()
        config.standardVlcPath = jTextFieldVlc.text
        config.standardFFmpegPath = jTextFieldFFmpeg.text
        updatePathValidation(jTextFieldVlc, config.standardVlcPath)
        updatePathValidation(jTextFieldFFmpeg, config.standardFFmpegPath)
    }

    private fun updatePathValidation(textField: JTextField, configuredPath: String) {
        textField.background = try {
            if (textField.text.isEmpty() || !File(configuredPath).exists()) {
                INVALID_PATH_COLOR
            } else {
                UIManager.getDefaults().getColor(TEXT_FIELD_BACKGROUND_KEY)
            }
        } catch (_: Exception) {
            INVALID_PATH_COLOR
        }
    }

    private fun chooseProgramFile(textField: JTextField) {
        if (SystemUtils.IS_OS_MAC_OSX) {
            chooseProgramFileWithNativeDialog(textField)
        } else {
            chooseProgramFileWithSwingDialog(textField)
        }
    }

    private fun chooseProgramFileWithNativeDialog(textField: JTextField) {
        val chooser = FileDialog(parentComponent, PROGRAM_FILE_DIALOG_TITLE).apply {
            mode = FileDialog.LOAD
            isVisible = true
        }
        val selectedFile = chooser.file ?: return
        try {
            textField.text = File(chooser.directory + selectedFile).absolutePath
        } catch (ex: Exception) {
            logger.error(ex)
        }
    }

    private fun chooseProgramFileWithSwingDialog(textField: JTextField) {
        val chooser = JFileChooser().apply {
            fileSelectionMode = JFileChooser.FILES_ONLY
            isFileHidingEnabled = false
            currentDirectory = if (textField.text.isEmpty()) {
                File(SystemUtils.USER_HOME)
            } else {
                File(textField.text)
            }
        }
        if (chooser.showOpenDialog(null) == JFileChooser.APPROVE_OPTION) {
            try {
                textField.text = chooser.selectedFile.absolutePath
            } catch (ex: Exception) {
                logger.error(ex)
            }
        }
    }

    private inner class ProgramPathDocumentListener : DocumentListener {
        override fun insertUpdate(event: DocumentEvent) = checkPaths()

        override fun removeUpdate(event: DocumentEvent) = checkPaths()

        override fun changedUpdate(event: DocumentEvent) = checkPaths()
    }

    private companion object {
        private val logger = LogManager.getLogger(PanelProgrammPfade::class.java)
        private val INVALID_PATH_COLOR = Color(255, 200, 200)
        private const val PROGRAM_FILE_DIALOG_TITLE = "Programmdatei auswählen"
        private const val TEXT_FIELD_BACKGROUND_KEY = "TextField.background"
    }
}

internal object PanelProgrammPfadeComponentNames {
    const val VLC_PANEL = "PanelProgrammPfade.vlcPanel"
    const val VLC_PATH = "PanelProgrammPfade.vlcPath"
    const val CHOOSE_VLC = "PanelProgrammPfade.chooseVlc"
    const val SEARCH_VLC = "PanelProgrammPfade.searchVlc"
    const val HELP = "PanelProgrammPfade.help"
    const val FFMPEG_PANEL = "PanelProgrammPfade.ffmpegPanel"
    const val FFMPEG_PATH = "PanelProgrammPfade.ffmpegPath"
    const val SEARCH_FFMPEG = "PanelProgrammPfade.searchFFmpeg"
    const val CHOOSE_FFMPEG = "PanelProgrammPfade.chooseFFmpeg"
}
