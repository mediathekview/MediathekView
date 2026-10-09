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

package mediathek.swing.table

import java.awt.Component
import javax.swing.Icon
import javax.swing.JLabel
import javax.swing.JTable
import javax.swing.border.Border
import javax.swing.table.DefaultTableCellRenderer

class IconHeaderCellRenderer(
    private val headerIcon: Icon,
    private val tooltipText: String
) : DefaultTableCellRenderer() {
    override fun getTableCellRendererComponent(
        table: JTable,
        value: Any?,
        isSelected: Boolean,
        hasFocus: Boolean,
        row: Int,
        column: Int
    ): Component {
        val defaultRenderer = table.tableHeader.defaultRenderer
        val styledComponent = defaultRenderer.getTableCellRendererComponent(
            table,
            value,
            isSelected,
            hasFocus,
            row,
            column
        )

        if (styledComponent is JLabel) {
            copyHeaderStyleFrom(styledComponent)
        } else {
            resetHeaderStyle()
        }

        horizontalTextPosition = LEFT
        horizontalAlignment = CENTER
        icon = headerIcon
        text = ""
        toolTipText = this@IconHeaderCellRenderer.tooltipText

        return this
    }

    private fun copyHeaderStyleFrom(source: JLabel) {
        foreground = source.foreground
        background = source.background
        font = source.font
        border = source.border
        isOpaque = source.isOpaque
        horizontalAlignment = source.horizontalAlignment
        verticalAlignment = source.verticalAlignment
        iconTextGap = source.iconTextGap
    }

    private fun resetHeaderStyle() {
        border = null as Border?
        isOpaque = false
    }
}
