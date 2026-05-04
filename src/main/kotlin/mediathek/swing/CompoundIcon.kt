/*
 * Copyright (c) 2025-2026 derreisende77.
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

import java.awt.Component
import java.awt.Graphics
import javax.swing.Icon

/**
 * Paints two or more icons as a single icon.
 */
class CompoundIcon(
    val axis: Axis = Axis.X_AXIS,
    val gap: Int = 0,
    private val alignmentX: Float = CENTER,
    private val alignmentY: Float = CENTER,
    vararg icons: Icon,
) : Icon {
    enum class Axis {
        X_AXIS,
        Y_AXIS,
        Z_AXIS,
    }

    private val icons: Array<out Icon> = icons.also { iconArray ->
        iconArray.forEachIndexed { index, icon ->
            requireNotNull(icon) { "Icon ($index) cannot be null" }
        }
    }
    private val normalizedAlignmentX = alignmentX.coerceIn(0.0f, 1.0f)
    private val normalizedAlignmentY = alignmentY.coerceIn(0.0f, 1.0f)

    constructor(vararg icons: Icon) : this(Axis.X_AXIS, 0, CENTER, CENTER, *icons)

    constructor(axis: Axis, vararg icons: Icon) : this(axis, 0, CENTER, CENTER, *icons)

    constructor(axis: Axis, gap: Int, vararg icons: Icon) : this(axis, gap, CENTER, CENTER, *icons)

    fun getAlignmentX(): Float = normalizedAlignmentX

    fun getAlignmentY(): Float = normalizedAlignmentY

    fun getIconCount(): Int = icons.size

    fun getIcon(index: Int): Icon = icons[index]

    override fun getIconWidth(): Int {
        return if (axis == Axis.X_AXIS) {
            icons.sumOf { it.iconWidth } + (icons.size - 1) * gap
        } else {
            icons.maxOfOrNull { it.iconWidth } ?: 0
        }
    }

    override fun getIconHeight(): Int {
        return if (axis == Axis.Y_AXIS) {
            icons.sumOf { it.iconHeight } + (icons.size - 1) * gap
        } else {
            icons.maxOfOrNull { it.iconHeight } ?: 0
        }
    }

    override fun paintIcon(component: Component?, graphics: Graphics, x: Int, y: Int) {
        when (axis) {
            Axis.X_AXIS -> {
                var iconX = x
                val height = iconHeight
                icons.forEach { icon ->
                    val iconY = getOffset(height, icon.iconHeight, normalizedAlignmentY)
                    icon.paintIcon(component, graphics, iconX, y + iconY)
                    iconX += icon.iconWidth + gap
                }
            }

            Axis.Y_AXIS -> {
                var iconY = y
                val width = iconWidth
                icons.forEach { icon ->
                    val iconX = getOffset(width, icon.iconWidth, normalizedAlignmentX)
                    icon.paintIcon(component, graphics, x + iconX, iconY)
                    iconY += icon.iconHeight + gap
                }
            }

            Axis.Z_AXIS -> {
                val width = iconWidth
                val height = iconHeight
                icons.forEach { icon ->
                    val iconX = getOffset(width, icon.iconWidth, normalizedAlignmentX)
                    val iconY = getOffset(height, icon.iconHeight, normalizedAlignmentY)
                    icon.paintIcon(component, graphics, x + iconX, y + iconY)
                }
            }
        }
    }

    private fun getOffset(maxValue: Int, iconValue: Int, alignment: Float): Int =
        Math.round((maxValue - iconValue) * alignment)

    companion object {
        const val TOP: Float = 0.0f
        const val LEFT: Float = 0.0f
        const val CENTER: Float = 0.5f
        const val BOTTOM: Float = 1.0f
        const val RIGHT: Float = 1.0f
    }
}
