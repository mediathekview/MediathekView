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

package mediathek.audiothek.ui.download

import java.awt.*
import java.awt.geom.Arc2D
import java.awt.geom.Ellipse2D
import javax.swing.Icon
import javax.swing.UIManager

class CircularProgressIcon(
    private val size: Int = 14,
    private val strokeWidth: Float = 2f
) : Icon {
    var progress: Double = 0.0
        set(value) {
            field = value.coerceIn(0.0, 1.0)
        }

    override fun getIconWidth(): Int = size

    override fun getIconHeight(): Int = size

    override fun paintIcon(c: Component?, g: Graphics, x: Int, y: Int) {
        val g2 = g.create() as Graphics2D
        try {
            g2.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)
            g2.setRenderingHint(RenderingHints.KEY_STROKE_CONTROL, RenderingHints.VALUE_STROKE_PURE)

            val ringColor = UIManager.getColor("Component.borderColor")
                ?: UIManager.getColor("Button.disabledBorderColor")
                ?: c?.foreground
            val progressColor = UIManager.getColor("ProgressBar.foreground")
                ?: UIManager.getColor("Component.accentColor")
                ?: c?.foreground

            val inset = strokeWidth / 2f + 1f
            val diameter = size - inset * 2
            val shape = Ellipse2D.Float(x + inset, y + inset, diameter, diameter)

            g2.stroke = BasicStroke(strokeWidth, BasicStroke.CAP_ROUND, BasicStroke.JOIN_ROUND)
            g2.color = ringColor
            g2.draw(shape)

            g2.color = progressColor
            g2.draw(Arc2D.Float(x + inset, y + inset, diameter, diameter, 90f, (-360.0 * progress).toFloat(), Arc2D.OPEN))
        } finally {
            g2.dispose()
        }
    }
}
