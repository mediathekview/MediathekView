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

import ca.odell.glazedlists.swing.GlazedListsSwing
import kotlinx.coroutines.CoroutineScope
import kotlinx.coroutines.Dispatchers
import kotlinx.coroutines.SupervisorJob
import kotlinx.coroutines.launch
import kotlinx.coroutines.swing.Swing
import mediathek.config.Daten
import mediathek.config.Konstanten
import mediathek.config.MVConfig
import mediathek.controller.SenderFilmlistLoadApprover
import mediathek.gui.messages.FilmListImportTypeChangedEvent
import mediathek.mainwindow.MediathekGui
import mediathek.swing.IconUtils
import mediathek.tool.*
import net.engio.mbassy.listener.Handler
import org.apache.commons.configuration2.Configuration
import org.kordamp.ikonli.fontawesome6.FontAwesomeSolid
import javax.swing.JCheckBox
import javax.swing.JOptionPane
import javax.swing.JTextField
import javax.swing.border.TitledBorder
import javax.swing.event.DocumentEvent
import javax.swing.event.DocumentListener

class PanelFilmlisteLaden(
    inSettingsDialog: Boolean,
) : PanelFilmlisteLadenBase() {
    private val config: Configuration = ApplicationConfiguration.getConfiguration()
    private val uiScope = CoroutineScope(SupervisorJob() + Dispatchers.Swing)
    private var warningDialogShown = false
    private var senderSelectionChanged = false

    init {
        MessageBus.messageBus.subscribe(this)

        initPanel()
        initReloadButton()
        setupCheckBoxes()

        btnReloadFilmlist.isVisible = inSettingsDialog
        if (inSettingsDialog) {
            prepareSettingsLayout()
        } else {
            panel1.toolTipText = "<html>Bei Änderungen wird eine komplette Filmliste vom Server geladen.<br>" +
                "Dies funktioniert <b>NICHT im Erweiterungsmodus</b>!!!</html>"
            panel1.border = TitledBorder("Ausgewählte Sender laden:")
        }

        setupSenderList()

        jRadioButtonManuell.addChangeListener {
            val selected = jRadioButtonManuell.isSelected
            jTextFieldUrl.isEnabled = selected
            jButtonDateiAuswaehlen.isEnabled = selected
            jCheckBoxUpdate.isEnabled = selected
        }

        cbEvaluateDuplicates.isSelected =
            config.getBoolean(ApplicationConfiguration.FILM_EVALUATE_DUPLICATES, true)
        cbEvaluateDuplicates.addActionListener {
            config.setProperty(
                ApplicationConfiguration.FILM_EVALUATE_DUPLICATES,
                cbEvaluateDuplicates.isSelected,
            )
        }
    }

    private fun initReloadButton() {
        btnReloadFilmlist.icon = IconUtils.of(FontAwesomeSolid.REDO_ALT)
        btnReloadFilmlist.addActionListener {
            val daten = Daten.getInstance()
            daten.listeFilme.clear()
            daten.filmeLaden.loadFilmlist("", hasSenderSelectionChanged())
        }
    }

    private fun setupSenderList() {
        val model = GlazedListsSwing.eventComboBoxModelWithThreadProxyList(SenderListBoxModel.providedSenderList)
        senderCheckBoxList.model = model

        val selectionModel = senderCheckBoxList.checkBoxListSelectionModel
        for (index in 0 until model.size) {
            val item = model.getElementAt(index)
            if (SenderFilmlistLoadApprover.isApproved(item)) {
                selectionModel.addSelectionInterval(index, index)
            } else {
                selectionModel.removeSelectionInterval(index, index)
            }
        }

        selectionModel.addListSelectionListener { event ->
            if (!event.valueIsAdjusting) {
                for (index in 0 until model.size) {
                    val item = model.getElementAt(index)
                    if (selectionModel.isSelectedIndex(index)) {
                        SenderFilmlistLoadApprover.approve(item)
                    } else {
                        SenderFilmlistLoadApprover.deny(item)
                    }
                }

                senderSelectionChanged = true
                showSenderRestartWarningOnce()
            }
        }
    }

    private fun showSenderRestartWarningOnce() {
        uiScope.launch {
            if (!warningDialogShown) {
                val message =
                    "<html>Bei Änderungen an den Sendern <b>muss</b> zwingend ein Neustart durchgeführt werden.</html>"
                JOptionPane.showMessageDialog(
                    this@PanelFilmlisteLaden,
                    message,
                    Konstanten.PROGRAMMNAME,
                    JOptionPane.WARNING_MESSAGE,
                )
                warningDialogShown = true
            }
        }
    }

    private fun prepareSettingsLayout() {
        lblUrl.isVisible = false
        jTextFieldUrl.isVisible = false
        jButtonDateiAuswaehlen.isVisible = false
    }

    private fun setupCheckBoxes() {
        cbSign.isSelected = config.getBoolean(ApplicationConfiguration.FilmList.LOAD_SIGN_LANGUAGE, true)
        cbSign.addActionListener {
            config.setProperty(ApplicationConfiguration.FilmList.LOAD_SIGN_LANGUAGE, cbSign.isSelected)
        }

        cbAudio.isSelected = config.getBoolean(ApplicationConfiguration.FilmList.LOAD_AUDIO_DESCRIPTION, true)
        cbAudio.addActionListener {
            config.setProperty(ApplicationConfiguration.FilmList.LOAD_AUDIO_DESCRIPTION, cbAudio.isSelected)
        }

        cbTrailer.isSelected = config.getBoolean(ApplicationConfiguration.FilmList.LOAD_TRAILER, true)
        cbTrailer.addActionListener {
            config.setProperty(ApplicationConfiguration.FilmList.LOAD_TRAILER, cbTrailer.isSelected)
        }

        cbLivestreams.isSelected = config.getBoolean(ApplicationConfiguration.FilmList.LOAD_LIVESTREAMS, true)
        cbLivestreams.addActionListener {
            config.setProperty(ApplicationConfiguration.FilmList.LOAD_LIVESTREAMS, cbLivestreams.isSelected)
        }

        jCheckBoxUpdate.isSelected = config.getBoolean(ApplicationConfiguration.FilmList.EXTEND_OLD_FILMLIST, false)
        jCheckBoxUpdate.addActionListener {
            config.setProperty(ApplicationConfiguration.FilmList.EXTEND_OLD_FILMLIST, jCheckBoxUpdate.isSelected)
        }
    }

    fun hasSenderSelectionChanged(): Boolean = senderSelectionChanged

    private fun initPanel() {
        initRadio()

        jButtonDateiAuswaehlen.icon = SVGIconUtilities.createSVGIcon("icons/fontawesome/folder-open.svg")
        jButtonDateiAuswaehlen.addActionListener {
            val loadFile = FileDialogs.chooseLoadFileLocation(MediathekGui.ui(), "Filmliste laden", "")
            if (loadFile != null) {
                jTextFieldUrl.text = loadFile.absolutePath
            }
        }

        val listener = {
            if (jRadioButtonManuell.isSelected) {
                FilmListUpdateType.MANUAL.writeToConfig()
            } else {
                FilmListUpdateType.AUTOMATIC.writeToConfig()
            }

            MessageBus.messageBus.publishAsync(FilmListImportTypeChangedEvent())
        }
        jRadioButtonManuell.addActionListener { listener() }
        jRadioButtonAuto.addActionListener { listener() }

        jTextFieldUrl.document.addDocumentListener(ImportUrlDocumentListener())
        val handler = TextCopyPasteHandler(jTextFieldUrl)
        jTextFieldUrl.componentPopupMenu = handler.getPopupMenu()
    }

    @Suppress("UNUSED_PARAMETER")
    @Handler
    private fun handleFilmListImportTypeChanged(event: FilmListImportTypeChangedEvent) {
        uiScope.launch {
            initRadio()
        }
    }

    private fun initRadio() {
        when (FilmListUpdateType.fromConfig()) {
            FilmListUpdateType.MANUAL -> jRadioButtonManuell.isSelected = true
            FilmListUpdateType.AUTOMATIC -> jRadioButtonAuto.isSelected = true
        }

        jTextFieldUrl.text = MVConfig.get(MVConfig.Configs.SYSTEM_IMPORT_URL_MANUELL)
    }

    val updateCheckBox: JCheckBox
        get() = jCheckBoxUpdate

    val urlTextField: JTextField
        get() = jTextFieldUrl

    private inner class ImportUrlDocumentListener : DocumentListener {
        override fun insertUpdate(event: DocumentEvent) {
            updateManualImportUrl()
        }

        override fun removeUpdate(event: DocumentEvent) {
            updateManualImportUrl()
        }

        override fun changedUpdate(event: DocumentEvent) {
            updateManualImportUrl()
        }

        private fun updateManualImportUrl() {
            MVConfig.add(MVConfig.Configs.SYSTEM_IMPORT_URL_MANUELL, jTextFieldUrl.text)
        }
    }
}
