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

package mediathek.tool.cellrenderer

import java.awt.Color
import java.awt.Component
import java.awt.Graphics
import java.awt.Graphics2D
import javax.swing.Icon

class SelectionContrastIcon(private val delegate: Icon) : Icon {
    override fun paintIcon(c: Component, g: Graphics, x: Int, y: Int) {
        val graphics = g.create() as Graphics2D
        try {
            val width = iconWidth
            val height = iconHeight
            graphics.color = Color(255, 255, 255, 210)
            graphics.fillRoundRect(x, y, width, height, 6, 6)
            graphics.color = Color(255, 255, 255, 235)
            graphics.drawRoundRect(x, y, width - 1, height - 1, 6, 6)
            delegate.paintIcon(c, graphics, x + PADDING_X, y + PADDING_Y)
        } finally {
            graphics.dispose()
        }
    }

    override fun getIconWidth(): Int = delegate.iconWidth + (PADDING_X * 2)

    override fun getIconHeight(): Int = delegate.iconHeight + (PADDING_Y * 2)

    private companion object {
        const val PADDING_X = 3
        const val PADDING_Y = 1
    }
}
