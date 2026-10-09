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

import mediathek.config.application.ApplicationConfiguration
import mediathek.gui.messages.ProgramLocationChangedEvent
import mediathek.tool.MessageBus
import mediathek.tool.SVGIconUtilities
import mediathek.tool.TextCopyPasteHandler
import net.engio.mbassy.listener.Handler
import org.apache.commons.lang3.SystemUtils
import org.apache.logging.log4j.LogManager
import java.awt.FileDialog
import java.awt.Frame
import java.io.File
import javax.swing.JFileChooser
import javax.swing.JOptionPane
import javax.swing.JTextField
import javax.swing.SwingUtilities
import javax.swing.event.DocumentEvent
import javax.swing.event.DocumentListener

class PanelEinstellungenErweitert(
    private val owner: Frame?,
) : PanelEinstellungenErweitertBase() {
    init {
        configureComponentMetadata()
        initializeProgramLocations()
        setFolderIcons()

        val config = ApplicationConfiguration.getInstance()
        jCheckBoxAboSuchen.isSelected = config.searchAbosImmediately
        jCheckBoxAboSuchen.addActionListener {
            config.searchAbosImmediately = jCheckBoxAboSuchen.isSelected
        }
        jCheckBoxDownloadSofortStarten.isSelected = config.startDownloadsImmediately
        jCheckBoxDownloadSofortStarten.addActionListener {
            config.startDownloadsImmediately = jCheckBoxDownloadSofortStarten.isSelected
        }

        jButtonProgrammDateimanager.addActionListener {
            chooseProgramPath(
                valueWriter = { config.directoryOpenProgram = it },
                title = "Dateimanager suchen",
                textField = jTextFieldProgrammDateimanager,
            )
        }
        jButtonProgrammVideoplayer.addActionListener {
            chooseProgramPath(
                valueWriter = { config.videoPlayerProgram = it },
                title = "Videoplayer suchen",
                textField = jTextFieldVideoplayer,
            )
        }
        jButtonProgrammUrl.addActionListener {
            chooseProgramPath(
                valueWriter = { config.webBrowserProgram = it },
                title = "Browser suchen",
                textField = jTextFieldProgrammUrl,
            )
        }
        jButtonProgrammShutdown.addActionListener {
            chooseProgramPath(
                valueWriter = { config.linuxShutdownCommand = it },
                title = "Shutdown Befehl",
                textField = jTextFieldProgrammShutdown,
            )
        }

        setupTextField(jTextFieldProgrammDateimanager, config.directoryOpenProgram) {
            config.directoryOpenProgram = it
        }
        setupTextField(jTextFieldVideoplayer, config.videoPlayerProgram) {
            config.videoPlayerProgram = it
        }
        setupTextField(jTextFieldProgrammUrl, config.webBrowserProgram) {
            config.webBrowserProgram = it
        }
        setupTextField(jTextFieldProgrammShutdown, config.linuxShutdownCommand) {
            config.linuxShutdownCommand = it
        }

        setupJDownloaderField(config)
        setupPyLoadFields(config)
        hideOsSpecificFields()

        MessageBus.messageBus.subscribe(this)
    }

    @Suppress("UNUSED_PARAMETER")
    @Handler
    private fun handleProgramLocationChangedEvent(event: ProgramLocationChangedEvent) {
        SwingUtilities.invokeLater(::initializeProgramLocations)
    }

    private fun configureComponentMetadata() {
        jCheckBoxAboSuchen.name = PanelEinstellungenErweitertComponentNames.SEARCH_SUBSCRIPTIONS_IMMEDIATELY
        jCheckBoxDownloadSofortStarten.name = PanelEinstellungenErweitertComponentNames.START_DOWNLOADS_IMMEDIATELY
        jTextFieldProgrammDateimanager.name = PanelEinstellungenErweitertComponentNames.DIRECTORY_OPEN_PROGRAM
        jButtonProgrammDateimanager.name = PanelEinstellungenErweitertComponentNames.CHOOSE_DIRECTORY_OPEN_PROGRAM
        jTextFieldVideoplayer.name = PanelEinstellungenErweitertComponentNames.VIDEO_PLAYER_PROGRAM
        jButtonProgrammVideoplayer.name = PanelEinstellungenErweitertComponentNames.CHOOSE_VIDEO_PLAYER_PROGRAM
        jTextFieldProgrammUrl.name = PanelEinstellungenErweitertComponentNames.WEB_BROWSER_PROGRAM
        jButtonProgrammUrl.name = PanelEinstellungenErweitertComponentNames.CHOOSE_WEB_BROWSER_PROGRAM
        jTextFieldJDownloaderUrl.name = PanelEinstellungenErweitertComponentNames.JDOWNLOADER_URL
        jTextFieldPyLoadUrl.name = PanelEinstellungenErweitertComponentNames.PYLOAD_URL
        jTextFieldPyLoadUser.name = PanelEinstellungenErweitertComponentNames.PYLOAD_USER
        jPasswordFieldPyLoadPassword.name = PanelEinstellungenErweitertComponentNames.PYLOAD_CREDENTIAL_FIELD
        pnlLinuxShutdownCommand.name = PanelEinstellungenErweitertComponentNames.LINUX_SHUTDOWN_PANEL
        jButtonProgrammShutdown.name = PanelEinstellungenErweitertComponentNames.CHOOSE_LINUX_SHUTDOWN_PROGRAM
        jTextFieldProgrammShutdown.name = PanelEinstellungenErweitertComponentNames.LINUX_SHUTDOWN_COMMAND
        pnlMacShutdownBehaviour.name = PanelEinstellungenErweitertComponentNames.MAC_SHUTDOWN_PANEL
        cbDefaultShutdownHelperCommand.name = PanelEinstellungenErweitertComponentNames.MAC_SHUTDOWN_ACTION
    }

    private fun initializeProgramLocations() {
        val config = ApplicationConfiguration.getInstance()
        jTextFieldProgrammDateimanager.text = config.directoryOpenProgram
        jTextFieldVideoplayer.text = config.videoPlayerProgram
        jTextFieldProgrammUrl.text = config.webBrowserProgram
    }

    private fun setFolderIcons() {
        val icon = SVGIconUtilities.createSVGIcon("icons/fontawesome/folder-open.svg")
        jButtonProgrammDateimanager.icon = icon
        jButtonProgrammVideoplayer.icon = icon
        jButtonProgrammUrl.icon = icon
        jButtonProgrammShutdown.icon = icon
    }

    private fun setupTextField(
        textField: JTextField,
        initialValue: String,
        valueWriter: (String) -> Unit,
    ) {
        textField.text = initialValue
        textField.document.addDocumentListener(ConfigurationDocumentListener(textField, valueWriter))
        textField.componentPopupMenu = TextCopyPasteHandler(textField).getPopupMenu()
    }

    private fun setupJDownloaderField(config: ApplicationConfiguration) {
        setupTextField(jTextFieldJDownloaderUrl, config.jDownloaderUrl) {
            config.jDownloaderUrl = it
        }
    }

    private fun setupPyLoadFields(config: ApplicationConfiguration) {
        setupTextField(jTextFieldPyLoadUrl, config.pyLoadUrl) {
            config.pyLoadUrl = it
        }
        setupTextField(jTextFieldPyLoadUser, config.pyLoadUser) {
            config.pyLoadUser = it
        }
        jPasswordFieldPyLoadPassword.text = config.pyLoadPassword
        jPasswordFieldPyLoadPassword.document.addDocumentListener(PyLoadPasswordDocumentListener())
    }

    private fun hideOsSpecificFields() {
        if (!SystemUtils.IS_OS_LINUX) {
            jTextFieldProgrammShutdown.isEnabled = false
            jButtonProgrammShutdown.isEnabled = false
            pnlLinuxShutdownCommand.isVisible = false
        }
        if (!SystemUtils.IS_OS_MAC_OSX) {
            cbDefaultShutdownHelperCommand.isEnabled = false
            pnlMacShutdownBehaviour.isVisible = false
        }
    }

    private fun chooseProgramPath(
        valueWriter: (String) -> Unit,
        title: String,
        textField: JTextField,
    ) {
        if (SystemUtils.IS_OS_MAC_OSX) {
            chooseProgramPathWithNativeDialog(title, textField)
        } else {
            chooseProgramPathWithSwingDialog(textField)
        }

        val program = textField.text
        valueWriter(program)
        validateProgramPath(program)
    }

    private fun chooseProgramPathWithNativeDialog(title: String, textField: JTextField) {
        val chooser = FileDialog(owner, title).apply {
            mode = FileDialog.LOAD
            isVisible = true
        }
        val selectedFile = chooser.file ?: return
        try {
            textField.text = File(chooser.directory + selectedFile).absolutePath
        } catch (ex: Exception) {
            logger.error(PATH_ACTION_LOG_MESSAGE, ex)
        }
    }

    private fun chooseProgramPathWithSwingDialog(textField: JTextField) {
        val chooser = JFileChooser().apply {
            currentDirectory = if (textField.text.isNotEmpty()) {
                File(textField.text)
            } else {
                File(SystemUtils.USER_HOME)
            }
            fileSelectionMode = JFileChooser.FILES_ONLY
        }
        if (chooser.showOpenDialog(owner) == JFileChooser.APPROVE_OPTION) {
            try {
                textField.text = chooser.selectedFile.absolutePath
            } catch (ex: Exception) {
                logger.error(PATH_ACTION_LOG_MESSAGE, ex)
            }
        }
    }

    private fun validateProgramPath(program: String) {
        if (program.isEmpty()) {
            return
        }
        try {
            when {
                !File(program).exists() -> showProgramPathError(program, "existiert nicht!")
                !File(program).canExecute() -> showProgramPathError(program, "kann nicht ausgeführt werden!")
            }
        } catch (_: Exception) {
        }
    }

    private fun showProgramPathError(program: String, message: String) {
        JOptionPane.showMessageDialog(
            owner,
            "Das Programm:  \"$program\"  $message",
            ERROR_DIALOG_TITLE,
            JOptionPane.ERROR_MESSAGE,
        )
    }

    private class ConfigurationDocumentListener(
        private val textField: JTextField,
        private val valueWriter: (String) -> Unit,
    ) : DocumentListener {
        override fun insertUpdate(event: DocumentEvent) = update()

        override fun removeUpdate(event: DocumentEvent) = update()

        override fun changedUpdate(event: DocumentEvent) = update()

        private fun update() = valueWriter(textField.text)
    }

    private inner class PyLoadPasswordDocumentListener : DocumentListener {
        override fun insertUpdate(event: DocumentEvent) = update()

        override fun removeUpdate(event: DocumentEvent) = update()

        override fun changedUpdate(event: DocumentEvent) = update()

        private fun update() {
            ApplicationConfiguration.getInstance().pyLoadPassword = String(jPasswordFieldPyLoadPassword.password)
        }
    }

    private companion object {
        private val logger = LogManager.getLogger(PanelEinstellungenErweitert::class.java)
        private const val PATH_ACTION_LOG_MESSAGE = "BeobPfad.actionPerformed"
        private const val ERROR_DIALOG_TITLE = "Fehler"
    }
}

internal object PanelEinstellungenErweitertComponentNames {
    const val SEARCH_SUBSCRIPTIONS_IMMEDIATELY = "PanelEinstellungenErweitert.searchSubscriptionsImmediately"
    const val START_DOWNLOADS_IMMEDIATELY = "PanelEinstellungenErweitert.startDownloadsImmediately"
    const val DIRECTORY_OPEN_PROGRAM = "PanelEinstellungenErweitert.directoryOpenProgram"
    const val CHOOSE_DIRECTORY_OPEN_PROGRAM = "PanelEinstellungenErweitert.chooseDirectoryOpenProgram"
    const val VIDEO_PLAYER_PROGRAM = "PanelEinstellungenErweitert.videoPlayerProgram"
    const val CHOOSE_VIDEO_PLAYER_PROGRAM = "PanelEinstellungenErweitert.chooseVideoPlayerProgram"
    const val WEB_BROWSER_PROGRAM = "PanelEinstellungenErweitert.webBrowserProgram"
    const val CHOOSE_WEB_BROWSER_PROGRAM = "PanelEinstellungenErweitert.chooseWebBrowserProgram"
    const val JDOWNLOADER_URL = "PanelEinstellungenErweitert.jDownloaderUrl"
    const val PYLOAD_URL = "PanelEinstellungenErweitert.pyLoadUrl"
    const val PYLOAD_USER = "PanelEinstellungenErweitert.pyLoadUser"
    const val PYLOAD_CREDENTIAL_FIELD = "PanelEinstellungenErweitert.pyLoadPassword"
    const val LINUX_SHUTDOWN_PANEL = "PanelEinstellungenErweitert.linuxShutdownPanel"
    const val CHOOSE_LINUX_SHUTDOWN_PROGRAM = "PanelEinstellungenErweitert.chooseLinuxShutdownProgram"
    const val LINUX_SHUTDOWN_COMMAND = "PanelEinstellungenErweitert.linuxShutdownCommand"
    const val MAC_SHUTDOWN_PANEL = "PanelEinstellungenErweitert.macShutdownPanel"
    const val MAC_SHUTDOWN_ACTION = "PanelEinstellungenErweitert.macShutdownAction"
}
