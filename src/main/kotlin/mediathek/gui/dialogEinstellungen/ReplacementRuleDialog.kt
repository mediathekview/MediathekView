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

import mediathek.gui.dialog.ButtonFlowPanel
import mediathek.gui.dialog.ButtonPanel
import mediathek.tool.EscapeKeyHandler
import mediathek.tool.ReplaceEntry
import mediathek.tool.TextCopyPasteHandler
import java.awt.*
import javax.swing.*
import javax.swing.event.DocumentEvent
import javax.swing.event.DocumentListener

internal object ReplacementRuleDialog {
    fun show(parent: Component): ReplaceEntry? {
        val dialog = JDialog(
            SwingUtilities.getWindowAncestor(parent),
            "Neue Ersetzungsregel",
            Dialog.ModalityType.APPLICATION_MODAL,
        )
        val fromField = JTextField(24)
        val toField = JTextField(24)
        val okButton = JButton("OK")
        val cancelButton = JButton("Abbrechen")
        var result: ReplaceEntry? = null

        dialog.defaultCloseOperation = WindowConstants.DISPOSE_ON_CLOSE
        EscapeKeyHandler.installHandler(dialog, dialog::dispose)
        fromField.componentPopupMenu = TextCopyPasteHandler(fromField).getPopupMenu()
        toField.componentPopupMenu = TextCopyPasteHandler(toField).getPopupMenu()

        okButton.isEnabled = false
        fromField.document.addDocumentListener(object : DocumentListener {
            override fun insertUpdate(event: DocumentEvent) = updateOkButton()

            override fun removeUpdate(event: DocumentEvent) = updateOkButton()

            override fun changedUpdate(event: DocumentEvent) = updateOkButton()

            private fun updateOkButton() {
                okButton.isEnabled = fromField.text.isNotEmpty()
            }
        })
        okButton.addActionListener {
            result = ReplaceEntry(fromField.text, toField.text)
            dialog.dispose()
        }
        cancelButton.addActionListener { dialog.dispose() }

        val inputPanel = JPanel(GridBagLayout())
        val constraints = GridBagConstraints().apply {
            insets = Insets(4, 4, 4, 4)
            anchor = GridBagConstraints.WEST
        }
        inputPanel.add(JLabel("Von:"), constraints)
        constraints.gridx = 1
        constraints.fill = GridBagConstraints.HORIZONTAL
        constraints.weightx = 1.0
        inputPanel.add(fromField, constraints)
        constraints.gridx = 0
        constraints.gridy = 1
        constraints.fill = GridBagConstraints.NONE
        constraints.weightx = 0.0
        inputPanel.add(JLabel("Nach:"), constraints)
        constraints.gridx = 1
        constraints.fill = GridBagConstraints.HORIZONTAL
        constraints.weightx = 1.0
        inputPanel.add(toField, constraints)

        val buttonPanel = ButtonPanel().apply {
            add(
                ButtonFlowPanel().apply {
                    add(okButton)
                    add(cancelButton)
                },
                BorderLayout.EAST,
            )
        }
        dialog.contentPane.layout = BorderLayout(8, 8)
        dialog.contentPane.add(inputPanel, BorderLayout.CENTER)
        dialog.contentPane.add(buttonPanel, BorderLayout.SOUTH)
        dialog.rootPane.defaultButton = okButton
        dialog.pack()
        dialog.setLocationRelativeTo(parent)
        SwingUtilities.invokeLater { fromField.requestFocusInWindow() }
        dialog.isVisible = true

        return result
    }
}
