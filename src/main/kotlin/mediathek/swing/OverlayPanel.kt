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

package mediathek.swing

import java.awt.Color
import java.awt.Font
import java.awt.Graphics
import java.awt.event.MouseAdapter
import java.awt.event.MouseWheelEvent
import java.awt.event.MouseWheelListener
import javax.swing.JComponent

class OverlayPanel(private val message: String) : JComponent() {
    private val backgroundColor = Color(192, 192, 192, 128)

    init {
        isOpaque = false
        val mouseBlocker = object : MouseAdapter() {}
        addMouseListener(mouseBlocker)
        addMouseMotionListener(mouseBlocker)
        addMouseWheelListener(MouseWheelListener { event: MouseWheelEvent -> event.consume() })
    }

    override fun paintComponent(g: Graphics) {
        super.paintComponent(g)
        g.color = backgroundColor
        g.fillRect(0, 0, width, height)

        g.color = Color.BLACK
        g.font = g.font.deriveFont(Font.BOLD, 16f)

        val fm = g.fontMetrics
        val textWidth = fm.stringWidth(message)
        val textHeight = fm.height

        g.drawString(
            message,
            (width - textWidth) / 2,
            (height + textHeight) / 2 - fm.descent
        )
    }
}
