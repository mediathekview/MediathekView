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

package mediathek.tool

import mediathek.config.Konstanten
import java.awt.BorderLayout
import java.awt.Component
import java.awt.Dimension
import java.awt.HeadlessException
import java.io.PrintWriter
import java.io.StringWriter
import javax.swing.*

object SwingErrorDialog {
    @Throws(HeadlessException::class)
    fun showExceptionMessage(parentComponent: Component?, messageText: String, exception: Throwable) {
        val stringWriter = StringWriter()
        exception.printStackTrace(PrintWriter(stringWriter))

        val message = JLabel(messageText).apply {
            border = BorderFactory.createEmptyBorder(3, 0, 10, 0)
        }

        val text = JTextArea().apply {
            isEditable = false
            font = UIManager.getFont("Label.font")
            text = stringWriter.toString()
            caretPosition = 0
        }

        val scroller = JScrollPane(text).apply {
            preferredSize = Dimension(640, 350)
        }

        val panel = JPanel().apply {
            layout = BorderLayout()
            add(message, BorderLayout.NORTH)
            add(scroller, BorderLayout.SOUTH)
        }

        JOptionPane.showMessageDialog(parentComponent, panel, Konstanten.PROGRAMMNAME, JOptionPane.ERROR_MESSAGE)
    }
}
