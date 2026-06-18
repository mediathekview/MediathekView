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

package mediathek.gui.dialog.add_download

import mediathek.config.MVColor
import mediathek.config.application.ApplicationConfiguration
import mediathek.daten.DatenPset
import mediathek.tool.EscapeKeyHandler
import mediathek.tool.FileDialogs
import mediathek.tool.FilenameUtils
import mediathek.tool.SVGIconUtilities
import javax.swing.DefaultComboBoxModel
import javax.swing.JFrame
import javax.swing.UIManager
import javax.swing.event.DocumentEvent
import javax.swing.event.DocumentListener
import javax.swing.text.JTextComponent

class DialogAddMoreDownload(
    private val parent: JFrame,
    private val pSet: DatenPset,
) : DialogAddMoreDownloadBase(parent) {
    private val orgPfad: String
    private var addAll = false
    private var cancel = false
    private var info = false
    private var subtitle = false
    private var startImmediately = false

    init {
        chkSubtitle.configureDownloadSubtitleCheckBox()
        chkSubtitle.isSelected = pSet.shouldDownloadSubtitle()
        subtitle = chkSubtitle.isSelected
        chkSubtitle.addActionListener { subtitle = chkSubtitle.isSelected }

        chkInfo.isSelected = pSet.shouldCreateInfofile()
        info = chkInfo.isSelected
        chkInfo.addActionListener { info = chkInfo.isSelected }

        jCheckBoxPfadSpeichern.isSelected = ApplicationConfiguration.getInstance().showLastUsedDownloadPath
        jCheckBoxPfadSpeichern.addActionListener {
            ApplicationConfiguration.getInstance().showLastUsedDownloadPath = jCheckBoxPfadSpeichern.isSelected
        }

        btnChange.addActionListener { dispose() }
        btnStartImmediately.addActionListener {
            addAll = true
            startImmediately = true
            dispose()
        }
        btnQueueDownloads.addActionListener {
            addAll = true
            startImmediately = false
            dispose()
        }
        btnCancel.addActionListener {
            cancel = true
            dispose()
        }

        jButtonPath.icon = SVGIconUtilities.createSVGIcon("icons/fontawesome/folder-open.svg")
        jButtonPath.addActionListener {
            val initialDirectory = jComboBoxPath.selectedItem?.toString().orEmpty()
            val selectedDirectory = FileDialogs.chooseDirectoryLocation(
                parent,
                "Film speichern",
                initialDirectory,
            )
            if (selectedDirectory != null) {
                val absolutePath = selectedDirectory.absolutePath
                jComboBoxPath.addItem(absolutePath)
                jComboBoxPath.selectedItem = absolutePath
            }
        }

        jButtonDelPath.icon = SVGIconUtilities.createSVGIcon("icons/fontawesome/trash-can.svg")
        jButtonDelPath.addActionListener {
            ApplicationConfiguration.getInstance().savedDownloadTargetPaths = ""
            jComboBoxPath.model = DefaultComboBoxModel(arrayOf(pSet.zielPfad))
        }

        DialogAddDownload.setModelPfad(pSet.zielPfad, jComboBoxPath)
        orgPfad = pSet.zielPfad
        pathEditor.isOpaque = true
        pathEditor.document.addDocumentListener(IllegalFilenameListener())

        EscapeKeyHandler.installHandler(this) {
            cancel = true
            dispose()
        }

        rootPane.defaultButton = btnStartImmediately
        pack()
    }

    fun wasCancelled(): Boolean = cancel

    fun showDialog(): DialogResult {
        isVisible = true
        return DialogResult(addAll, info, subtitle, path, startImmediately)
    }

    private val pathEditor: JTextComponent
        get() = jComboBoxPath.editor.editorComponent as JTextComponent

    private val path: String
        get() = jComboBoxPath.model.selectedItem?.toString().orEmpty().ifEmpty { pSet.zielPfad }

    override fun dispose() {
        DialogAddDownload.saveComboPfad(jComboBoxPath, orgPfad)
        super.dispose()
    }

    class DialogResult(
        private val addAllWithDefaults: Boolean,
        private val info: Boolean,
        private val subtitle: Boolean,
        private val path: String,
        private val startImmediately: Boolean,
    ) {
        fun addAllWithDefaults(): Boolean = addAllWithDefaults

        fun info(): Boolean = info

        fun subtitle(): Boolean = subtitle

        fun path(): String = path

        fun startImmediately(): Boolean = startImmediately
    }

    private inner class IllegalFilenameListener : DocumentListener {
        override fun insertUpdate(e: DocumentEvent) {
            updatePathEditorBackground()
        }

        override fun removeUpdate(e: DocumentEvent) {
            updatePathEditorBackground()
        }

        override fun changedUpdate(e: DocumentEvent) {
            updatePathEditorBackground()
        }

        private fun updatePathEditorBackground() {
            val text = pathEditor.text
            pathEditor.background = if (text != FilenameUtils.checkFilenameForIllegalCharacters(text, true)) {
                MVColor.DOWNLOAD_FEHLER.color
            } else {
                UIManager.getDefaults().getColor("TextField.background")
            }
        }
    }
}
