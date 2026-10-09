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

package mediathek.gui.bookmark.renderer

import java.awt.BorderLayout
import java.awt.Component
import javax.swing.*
import javax.swing.plaf.UIResource
import javax.swing.table.TableCellRenderer

open class NoteCellRenderer : JPanel(), TableCellRenderer {
    protected val checkBox = JCheckBox()

    init {
        layout = BorderLayout()
        checkBox.horizontalAlignment = SwingConstants.CENTER
        add(checkBox, BorderLayout.CENTER)
    }

    protected fun performSelectionDrawing(table: JTable, isSelected: Boolean, row: Int) {
        if (isSelected) {
            foreground = table.selectionForeground
            background = table.selectionBackground
        } else {
            var background = table.background
            if (background == null || background is UIResource) {
                val alternateColor = UIManager.getColor("Table.alternateRowColor")
                if (alternateColor != null && row % 2 != 0) {
                    background = alternateColor
                }
            }
            foreground = table.foreground
            this.background = background
        }
    }

    override fun getTableCellRendererComponent(
        table: JTable?,
        value: Any?,
        isSelected: Boolean,
        hasFocus: Boolean,
        row: Int,
        column: Int,
    ): Component {
        if (table == null) {
            return this
        }

        performSelectionDrawing(table, isSelected, row)

        checkBox.isSelected = value != null
        toolTipText = value as? String

        return this
    }
}
