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

import java.awt.Component
import javax.swing.*

object HelpTextDialog {
    private const val ROWS = 20
    private const val COLUMNS = 60

    @JvmStatic
    fun show(parent: Component?, text: String) {
        if (SwingUtilities.isEventDispatchThread()) {
            showOnEventDispatchThread(parent, text)
        } else {
            SwingUtilities.invokeLater { showOnEventDispatchThread(parent, text) }
        }
    }

    private fun showOnEventDispatchThread(parent: Component?, text: String) {
        val textArea = JTextArea(text).apply {
            rows = ROWS
            columns = COLUMNS
            lineWrap = true
            wrapStyleWord = true
            isEditable = false
            border = BorderFactory.createEmptyBorder(5, 5, 5, 5)
            caretPosition = 0
        }

        JOptionPane.showMessageDialog(parent, JScrollPane(textArea), "Hilfe", JOptionPane.PLAIN_MESSAGE)
    }
}