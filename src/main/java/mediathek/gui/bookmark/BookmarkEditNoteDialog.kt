/*
 * Copyright (c) 2025 derreisende77.
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

package mediathek.gui.bookmark

import kotlinx.coroutines.*
import kotlinx.coroutines.swing.Swing
import mediathek.config.Konstanten
import mediathek.gui.bookmark.expiration.ArdMediathekExpiryHelper
import mediathek.gui.bookmark.expiration.ArteExpiryHelper
import mediathek.gui.bookmark.expiration.OrfExpiryHelper
import mediathek.gui.bookmark.expiration.ThreeSatExpiryHelper
import mediathek.swing.IconUtils
import mediathek.swing.JIkonliSafeButton
import mediathek.tool.EscapeKeyHandler
import mediathek.tool.datum.DateUtil
import net.miginfocom.layout.AC
import net.miginfocom.layout.CC
import net.miginfocom.layout.LC
import net.miginfocom.swing.MigLayout
import org.jdesktop.swingx.JXDatePicker
import org.kordamp.ikonli.materialdesign2.MaterialDesignC
import java.awt.BorderLayout
import java.awt.Dialog
import java.awt.Dimension
import java.time.LocalDate
import javax.swing.*

class BookmarkEditNoteDialog(
    owner: Dialog,
    private val bm: BookmarkData
) : JDialog(owner, true) {

    private var okPressed = false
    private val datePicker = JXDatePicker()
    private val btnSearch = JIkonliSafeButton()
    private val textArea = JTextArea()
    private val okButton = JButton("OK")
    private val cancelButton = JButton("Abbrechen")

    private val job = SupervisorJob()
    private val coroutineScope = CoroutineScope(job + Dispatchers.Swing)

    companion object {
        private val ARD_SENDERS = setOf(
            "ard", "ardalpha", "br", "funknet", "hr", "mdr", "ndr",
            "one", "phoenix", "radiobremntv", "rbb", "sr", "swr",
            "tagesschau24", "wdr"
        )
    }

    init {
        title = "Notiz hinzufügen"
        defaultCloseOperation = DISPOSE_ON_CLOSE

        initComponents()
        rootPane.defaultButton = okButton
        EscapeKeyHandler.installHandler(this) { dispose() }

        setupButtonBar()
        setupNotizArea()
        setupDatePicker()

        btnSearch.icon = IconUtils.of(MaterialDesignC.CLOUD_SEARCH_OUTLINE, 20)
        btnSearch.toolTipText = "Auf Website suchen"
        btnSearch.addActionListener {
            coroutineScope.launch {
                btnSearch.isEnabled = false
                try {
                    val result = withContext(Dispatchers.IO) {
                        fetchExpiryDate(bm.datenFilm.sender, bm.datenFilm.websiteUrl)
                    }
                    if (result != null) {
                        datePicker.date = DateUtil.convertToDate(result)
                    } else {
                        showNotFoundMessage()
                    }
                } finally {
                    btnSearch.isEnabled = true
                }
            }
        }

        SwingUtilities.invokeLater { textArea.requestFocusInWindow() }
    }

    override fun dispose() {
        job.cancel()
        super.dispose()
    }

    private fun setupButtonBar() {
        okButton.addActionListener {
            okPressed = true
            dispose()
        }
        cancelButton.addActionListener { dispose() }
    }

    private fun setupNotizArea() {
        textArea.text = bm.note ?: ""
        textArea.caretPosition = 0
    }

    private fun setupDatePicker() {
        bm.availableUntil?.let {
            datePicker.date = DateUtil.convertToDate(it)
        }
    }

    fun getNotiz(): String = textArea.text

    fun getAvailableUntilDate(): LocalDate? = DateUtil.convertToLocalDate(datePicker.date)

    fun isOkPressed(): Boolean = okPressed

    private fun fetchExpiryDate(sender: String, websiteUrl: String): LocalDate? {
        val normalizedSender = normalizeSender(sender)
        return when {
            "arte" in normalizedSender -> ArteExpiryHelper.getExpiryInfo(websiteUrl).map { it.expiryDate() }.orElse(null)
            normalizedSender == "3sat" -> ThreeSatExpiryHelper.getExpiryInfo(websiteUrl).map { it.expiryDate() }.orElse(null)
            normalizedSender in ARD_SENDERS -> ArdMediathekExpiryHelper.getExpiryInfo(websiteUrl).map { it.expiryDate() }.orElse(null)
            normalizedSender == "orf" -> OrfExpiryHelper.getExpiryInfo(websiteUrl).map { it.expiryDate() }.orElse(null)
            else -> null
        }
    }

    private fun showNotFoundMessage() {
        JOptionPane.showMessageDialog(this,
            "Das Ablaufdatum wurde nicht gefunden.",
            Konstanten.PROGRAMMNAME,
            JOptionPane.WARNING_MESSAGE
        )
    }

    private fun normalizeSender(sender: String) =
        sender.trim().lowercase().replace(Regex("[^a-z0-9]"), "")

    private fun initComponents() {
        val dialogPane = JPanel(BorderLayout())
        val contentPanel = JPanel(MigLayout(
            LC().insets("dialog").hideMode(3),
            AC().fill().gap().fill().gap().fill().gap().grow().fill(),
            AC().gap().grow().align("top")
        ))

        contentPanel.add(JLabel("Verfügbar bis:"), CC().cell(0, 0))
        contentPanel.add(datePicker, CC().cell(1, 0))
        contentPanel.add(btnSearch, CC().cell(2, 0))

        contentPanel.add(JLabel("Notiz:"), CC().cell(0, 1))

        val scrollPane = JScrollPane(textArea).apply {
            minimumSize = Dimension(400, 150)
            preferredSize = Dimension(400, 150)
        }
        textArea.lineWrap = true
        textArea.wrapStyleWord = true
        contentPanel.add(scrollPane, CC().cell(1, 1, 3, 1).grow())

        val buttonBar = JPanel(MigLayout(
            LC().insets("dialog").alignX("right"),
            AC().size("button").fill().gap().size("button").fill(),
            AC()
        ))

        buttonBar.add(okButton, CC().cell(0, 0))
        buttonBar.add(cancelButton, CC().cell(1, 0))

        dialogPane.add(contentPanel, BorderLayout.CENTER)
        dialogPane.add(buttonBar, BorderLayout.SOUTH)

        contentPane.add(dialogPane, BorderLayout.CENTER)
        minimumSize = Dimension(450, 300)
        pack()
        setLocationRelativeTo(owner)
    }
}
