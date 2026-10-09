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

import mediathek.config.Konstanten
import net.miginfocom.swing.MigLayout
import java.awt.BorderLayout
import java.awt.Dimension
import java.awt.Frame
import java.awt.event.KeyEvent
import java.awt.event.WindowAdapter
import java.awt.event.WindowEvent
import javax.swing.*

class SqliteRecoveryProgressDialog(owner: Frame?) : JDialog(owner, "DB-Wiederherstellung", true) {
    private val messageLabel = JLabel("Starte DB-Wiederherstellung...")
    private val progressBar = JProgressBar()
    private val cancelButton = JButton("Abbrechen")

    var onCancel: (() -> Unit)? = null

    init {
        defaultCloseOperation = DO_NOTHING_ON_CLOSE
        minimumSize = Dimension(480, 160)
        layout = BorderLayout()

        val content = JPanel(MigLayout("insets 16, fillx", "[grow]", "[]12[]20[]"))
        content.add(
            JLabel(
                "<html>Die DB-Wiederherstellung kann je nach Datenbankgröße eine Weile dauern.<br>" +
                    "Das Fenster bleibt geöffnet, bis der Vorgang abgeschlossen oder abgebrochen wurde.</html>"
            ),
            "growx, wrap"
        )
        content.add(messageLabel, "growx, wrap")

        progressBar.isIndeterminate = true
        content.add(progressBar, "growx, wrap")

        val buttonPanel = JPanel(MigLayout("insets 0, fillx", "[right]"))
        cancelButton.addActionListener { cancelRecovery() }
        buttonPanel.add(cancelButton)
        content.add(buttonPanel, "growx")

        add(content, BorderLayout.CENTER)

        rootPane.registerKeyboardAction(
            { cancelRecovery() },
            KeyStroke.getKeyStroke(KeyEvent.VK_ESCAPE, 0),
            JComponent.WHEN_IN_FOCUSED_WINDOW
        )
        addWindowListener(object : WindowAdapter() {
            override fun windowClosing(e: WindowEvent) {
                cancelRecovery()
            }
        })

        pack()
        setLocationRelativeTo(owner)
    }

    fun updateProgress(message: String, indeterminate: Boolean = true) {
        messageLabel.text = message
        progressBar.isIndeterminate = indeterminate
    }

    fun markCancelling() {
        messageLabel.text = "Breche DB-Wiederherstellung ab..."
        cancelButton.isEnabled = false
    }

    private fun cancelRecovery() {
        val decision = JOptionPane.showConfirmDialog(
            this,
            "DB-Wiederherstellung wirklich abbrechen?",
            Konstanten.PROGRAMMNAME,
            JOptionPane.OK_CANCEL_OPTION,
            JOptionPane.WARNING_MESSAGE
        )
        if (decision == JOptionPane.OK_OPTION) {
            markCancelling()
            onCancel?.invoke()
        }
    }
}
