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

package mediathek.gui.dialog.subripmerge

import kotlinx.coroutines.*
import kotlinx.coroutines.swing.Swing
import mediathek.config.Konstanten
import mediathek.tool.FileDialogs
import mediathek.tool.LanguageCode
import mediathek.tool.SwingErrorDialog
import org.apache.logging.log4j.LogManager
import java.awt.Window
import javax.swing.DefaultComboBoxModel
import javax.swing.JOptionPane
import javax.swing.event.DocumentEvent
import javax.swing.event.DocumentListener
import kotlin.coroutines.cancellation.CancellationException

class MergeSubripVideoDialog(owner: Window) : MergeSubripVideoDialogBase(owner) {
    private val dialogScope = CoroutineScope(SupervisorJob() + Dispatchers.Swing)

    init {
        rootPane.defaultButton = btnMerge
        busyLabel.isVisible = false
        btnMerge.isEnabled = false

        fillLanguageComboBox()
        cbLanguage.selectedItem = getLanguageText(LanguageCode.de)

        btnCancel.addActionListener { dispose() }
        setupTextFieldListener()

        btnSelectInputSubrip.addActionListener { chooseSubripFile() }
        btnSelectInputVideo.addActionListener { chooseVideoFile() }
        btnSelectVideoOutputPath.addActionListener { chooseVideoOutputPath() }
        btnMerge.addActionListener { mergeSubripWithVideo() }
    }

    override fun dispose() {
        dialogScope.cancel()
        super.dispose()
    }

    private fun chooseSubripFile() {
        val file = FileDialogs.chooseLoadFileLocation(this, "Untertitel wählen", "") ?: return
        val filePath = file.absolutePath
        if (!filePath.lowercase().endsWith(".srt")) {
            JOptionPane.showMessageDialog(
                this,
                "Untertiteldatei muss auf .srt enden.",
                Konstanten.PROGRAMMNAME,
                JOptionPane.ERROR_MESSAGE,
            )
            tfSubripFilePath.text = ""
        } else {
            tfSubripFilePath.text = filePath
        }
    }

    private fun chooseVideoFile() {
        tfVideoFilePath.text = FileDialogs.chooseLoadFileLocation(this, "Video wählen", "")?.absolutePath.orEmpty()
    }

    private fun chooseVideoOutputPath() {
        tfVideoOutputPath.text = FileDialogs.chooseSaveFileLocation(this, "Videospeicherort wählen", "")?.absolutePath.orEmpty()
    }

    private fun mergeSubripWithVideo() {
        val language = runCatching(::selectedLanguageCode).getOrElse { ex ->
            logger.error("Error occurred while reading selected subtitle language", ex)
            SwingErrorDialog.showExceptionMessage(this, "Es ist ein Fehler aufgetreten.", ex)
            return
        }

        startMergeProcess()
        val subripFilePath = tfSubripFilePath.text
        val videoFilePath = tfVideoFilePath.text
        val videoOutputPath = tfVideoOutputPath.text

        dialogScope.launch {
            try {
                SubripVideoMergeService.merge(
                    subripFilePath = subripFilePath,
                    videoFilePath = videoFilePath,
                    videoOutputPath = videoOutputPath,
                    languageCode = language,
                )
                shutdownMergeProcess()
                JOptionPane.showMessageDialog(
                    this@MergeSubripVideoDialog,
                    "Das Zusammenführen war erfolgreich",
                    Konstanten.PROGRAMMNAME,
                    JOptionPane.INFORMATION_MESSAGE,
                )
                dispose()
            } catch (ex: CancellationException) {
                throw ex
            } catch (ex: Exception) {
                logger.error("Error occurred while merging video", ex)
                shutdownMergeProcess()
                SwingErrorDialog.showExceptionMessage(this@MergeSubripVideoDialog, "Der Vorgang war fehlerhaft", ex)
                dispose()
            }
        }
    }

    private fun setupTextFieldListener() {
        val documentListener = object : DocumentListener {
            override fun insertUpdate(e: DocumentEvent) = updateButtonState()

            override fun removeUpdate(e: DocumentEvent) = updateButtonState()

            override fun changedUpdate(e: DocumentEvent) = updateButtonState()
        }

        tfSubripFilePath.document.addDocumentListener(documentListener)
        tfVideoFilePath.document.addDocumentListener(documentListener)
        tfVideoOutputPath.document.addDocumentListener(documentListener)
    }

    private fun updateButtonState() {
        btnMerge.isEnabled = tfSubripFilePath.text.isNotBlank() &&
            tfVideoFilePath.text.isNotBlank() &&
            tfVideoOutputPath.text.isNotBlank()
    }

    private fun startMergeProcess() {
        busyLabel.isVisible = true
        busyLabel.isBusy = true
        btnMerge.isEnabled = false
        btnCancel.isEnabled = false
    }

    private fun shutdownMergeProcess() {
        busyLabel.isBusy = false
        busyLabel.isVisible = false
        btnCancel.isEnabled = true
    }

    private fun selectedLanguageCode(): String {
        val selectedLanguage = checkNotNull(cbLanguage.selectedItem as? String) {
            "Native language selected is null"
        }

        return checkNotNull(languagePattern.find(selectedLanguage)?.groupValues?.get(1)) {
            "Could not get ISO 639 3 letter code"
        }
    }

    private fun getLanguageText(code: LanguageCode): String = "${code.nativeName()} [${code.getISO3Language()}]"

    private fun fillLanguageComboBox() {
        cbLanguage.model = DefaultComboBoxModel(LanguageCode.entries.map(::getLanguageText).toTypedArray())
    }

    private companion object {
        private val logger = LogManager.getLogger(MergeSubripVideoDialog::class.java)
        private val languagePattern = """\[(.*?)]""".toRegex()
    }
}
