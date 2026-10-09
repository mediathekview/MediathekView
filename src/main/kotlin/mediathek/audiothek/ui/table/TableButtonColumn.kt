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

package mediathek.audiothek.ui.table

import org.kordamp.ikonli.swing.FontIcon
import java.awt.Color
import java.awt.Component
import java.awt.FlowLayout
import javax.swing.*
import javax.swing.plaf.UIResource
import javax.swing.table.TableCellEditor
import javax.swing.table.TableCellRenderer

class TableButtonColumn(
    private val label: String,
    private val icon: Icon? = null,
    private val onClick: (modelRow: Int) -> Unit
) : AbstractCellEditor(), TableCellRenderer, TableCellEditor {
    private val renderButton = JButton(label)
    private val editorButton = JButton(label)
    private val renderPanel = JPanel(FlowLayout(FlowLayout.CENTER, 0, 0))
    private val editorPanel = JPanel(FlowLayout(FlowLayout.CENTER, 0, 0))
    private val normalIcon = icon
    private val selectedIcon = (icon as? FontIcon)?.let {
        FontIcon.of(it.ikon, it.iconSize, Color.WHITE)
    } ?: icon
    private var currentModelRow = -1

    init {
        renderPanel.add(renderButton)
        editorPanel.add(editorButton)
        renderButton.icon = normalIcon
        editorButton.icon = selectedIcon
        if (icon != null) {
            renderButton.text = ""
            editorButton.text = ""
            configureIconButton(renderButton)
            configureIconButton(editorButton)
        }

        editorButton.addActionListener {
            val row = currentModelRow
            fireEditingStopped()
            if (row >= 0) {
                onClick(row)
            }
        }
    }

    override fun getTableCellRendererComponent(
        table: JTable,
        value: Any?,
        isSelected: Boolean,
        hasFocus: Boolean,
        row: Int,
        column: Int
    ): Component {
        renderButton.text = if (icon == null) value?.toString() ?: label else ""
        if (isSelected) {
            renderButton.icon = selectedIcon
            renderButton.foreground = Color.WHITE
            renderPanel.foreground = table.selectionForeground
            renderPanel.background = table.selectionBackground
        } else {
            renderButton.icon = normalIcon
            renderButton.foreground = table.foreground
            renderPanel.foreground = table.foreground
            renderPanel.background = resolveRowBackground(table, row)
        }
        return renderPanel
    }

    override fun getTableCellEditorComponent(
        table: JTable,
        value: Any?,
        isSelected: Boolean,
        row: Int,
        column: Int
    ): Component {
        currentModelRow = table.convertRowIndexToModel(row)
        editorButton.text = if (icon == null) value?.toString() ?: label else ""
        editorButton.icon = selectedIcon
        editorButton.foreground = Color.WHITE
        editorPanel.foreground = table.selectionForeground
        editorPanel.background = table.selectionBackground
        return editorPanel
    }

    override fun getCellEditorValue(): Any = label

    private fun configureIconButton(button: JButton) {
        button.isBorderPainted = false
        button.isContentAreaFilled = false
        button.isFocusPainted = false
        button.isOpaque = false
    }

    private fun resolveRowBackground(table: JTable, row: Int): Color? {
        val tableBackground = table.background
        if (tableBackground == null || tableBackground is UIResource) {
            val alternateColor = UIManager.getColor("Table.alternateRowColor")
            if (alternateColor != null && row % 2 != 0) {
                return alternateColor
            }
        }
        return tableBackground
    }
}
