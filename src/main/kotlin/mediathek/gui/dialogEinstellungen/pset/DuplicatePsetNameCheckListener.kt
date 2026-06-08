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

package mediathek.gui.dialogEinstellungen.pset

import mediathek.config.Daten
import java.awt.Color
import javax.swing.JTextField
import javax.swing.UIManager
import javax.swing.event.DocumentEvent
import javax.swing.event.DocumentListener

class DuplicatePsetNameCheckListener(
    private val textField: JTextField,
) : DocumentListener {
    override fun insertUpdate(e: DocumentEvent) {
        duplicateNameCheck()
    }

    override fun removeUpdate(e: DocumentEvent) {
        duplicateNameCheck()
    }

    override fun changedUpdate(e: DocumentEvent) {
        duplicateNameCheck()
    }

    private fun markDuplicate() {
        textField.background = Color.ORANGE
        textField.requestFocusInWindow()
    }

    private fun resetDuplicate() {
        textField.background = UIManager.getDefaults().getColor("TextField.background")
    }

    private fun duplicateNameCheck() {
        val count = Daten.getInstance().listePset
            .map { pset -> pset.name }
            .count { name -> name == textField.text }

        if (count > 1) {
            markDuplicate()
        } else {
            resetDuplicate()
        }
    }
}
