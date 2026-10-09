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
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package ca.odell.glazedlists.swing

import ca.odell.glazedlists.SortedList
import ca.odell.glazedlists.gui.AbstractTableComparatorChooser
import ca.odell.glazedlists.gui.TableFormat
import ca.odell.glazedlists.impl.SortIconFactory
import ca.odell.glazedlists.impl.gui.SortingStrategy
import java.awt.AWTEventMulticaster
import java.awt.Component
import java.awt.Cursor
import java.awt.event.ActionEvent
import java.awt.event.ActionListener
import java.awt.event.MouseAdapter
import java.awt.event.MouseEvent
import java.beans.PropertyChangeEvent
import java.beans.PropertyChangeListener
import javax.swing.Icon
import javax.swing.JLabel
import javax.swing.JTable
import javax.swing.SwingConstants
import javax.swing.event.TableModelEvent
import javax.swing.event.TableModelListener
import javax.swing.plaf.UIResource
import javax.swing.table.DefaultTableCellRenderer
import javax.swing.table.TableCellRenderer
import javax.swing.table.TableModel

/** Lets users sort a Swing table by clicking its column headers. */
class TableComparatorChooser<E : Any> private constructor(
    private val table: JTable,
    sortedList: SortedList<E>,
    strategy: SortingStrategy,
    tableFormat: TableFormat<in E>,
) : AbstractTableComparatorChooser<E>(sortedList, tableFormat, strategy) {
    private var sortArrowHeaderRenderer: SortArrowHeaderRenderer? = null
    private val tableHeaderUIHandler = TableHeaderUIHandler()
    private val tableModelHandler = TableModelHandler()
    private var sortListener: ActionListener? = null
    private val headerClickHandler: HeaderClickHandler

    init {
        table.addPropertyChangeListener("model", tableModelHandler)
        table.tableHeader.addPropertyChangeListener("UI", tableHeaderUIHandler)
        wrapDefaultTableHeaderRenderer()
        table.model.addTableModelListener(tableModelHandler)
        headerClickHandler = HeaderClickHandler(table, strategy)
    }

    private fun wrapDefaultTableHeaderRenderer() {
        val defaultRenderer = table.tableHeader.defaultRenderer
        if (defaultRenderer != null && defaultRenderer !== sortArrowHeaderRenderer) {
            sortArrowHeaderRenderer = SortArrowHeaderRenderer(defaultRenderer)
            table.tableHeader.defaultRenderer = sortArrowHeaderRenderer
        }
    }

    fun addSortActionListener(sortActionListener: ActionListener) {
        sortListener = AWTEventMulticaster.add(sortListener, sortActionListener)
    }

    override fun redetectComparator(currentComparator: Comparator<in E>?) {
        super.redetectComparator(currentComparator)
        table.tableHeader.revalidate()
        table.tableHeader.repaint()
    }

    override fun rebuildComparator() {
        super.rebuildComparator()
        table.tableHeader.revalidate()
        table.tableHeader.repaint()
        sortListener?.actionPerformed(ActionEvent(this, 0, "sort"))
    }

    override fun getSortingStyle(column: Int): Int =
        super.getSortingStyle(table.convertColumnIndexToModel(column))

    private fun isSortingMouseEvent(event: MouseEvent): Boolean = event.button == MouseEvent.BUTTON1

    override fun disposeInternal() {
        headerClickHandler.dispose()

        val currentRenderer = sortArrowHeaderRenderer
        if (currentRenderer != null && table.tableHeader.defaultRenderer === currentRenderer) {
            table.tableHeader.defaultRenderer = currentRenderer.delegateRenderer
        }

        table.model.removeTableModelListener(tableModelHandler)
        table.removePropertyChangeListener("model", tableModelHandler)
        table.tableHeader.removePropertyChangeListener("UI", tableHeaderUIHandler)
        sortListener = null
    }

    private inner class TableHeaderUIHandler : PropertyChangeListener {
        override fun propertyChange(event: PropertyChangeEvent) {
            wrapDefaultTableHeaderRenderer()
        }
    }

    private inner class TableModelHandler : TableModelListener, PropertyChangeListener {
        override fun propertyChange(event: PropertyChangeEvent) {
            val oldModel = event.oldValue as TableModel
            val newModel = event.newValue as TableModel

            oldModel.removeTableModelListener(this)
            newModel.addTableModelListener(this)

            if (newModel is AdvancedTableModel<*>) {
                setTableFormat(getTableFormat(newModel))
            }
        }

        override fun tableChanged(event: TableModelEvent) {
            if (event.firstRow == TableModelEvent.HEADER_ROW && event.column == TableModelEvent.ALL_COLUMNS) {
                if (table.model is AdvancedTableModel<*>) {
                    setTableFormat(getTableFormat(table.model))
                }
            }

            val currentComparator = getSortedList().comparator
            if (currentComparator !== sortedListComparator) {
                redetectComparator(currentComparator)
            }
        }
    }

    private inner class SortArrowHeaderRenderer(
        var delegateRenderer: TableCellRenderer,
    ) : TableCellRenderer, UIResource {
        override fun getTableCellRendererComponent(
            table: JTable,
            value: Any?,
            isSelected: Boolean,
            hasFocus: Boolean,
            row: Int,
            column: Int,
        ): Component {
            if (column < 0) {
                return getDelegateTableCellRendererComponent(table, value, isSelected, hasFocus, row, column)
            }

            val sortIcon = icons[getSortingStyle(column)]
            val rendered: Component
            if (delegateRenderer is SortableRenderer) {
                (delegateRenderer as SortableRenderer).setSortIcon(sortIcon)
                rendered = getDelegateTableCellRendererComponent(table, value, isSelected, hasFocus, row, column)
            } else {
                rendered = getDelegateTableCellRendererComponent(table, value, isSelected, hasFocus, row, column)
                if (rendered is JLabel) {
                    rendered.icon = sortIcon
                    rendered.horizontalTextPosition = SwingConstants.LEADING
                }
            }
            return rendered
        }

        private fun getDelegateTableCellRendererComponent(
            table: JTable,
            value: Any?,
            isSelected: Boolean,
            hasFocus: Boolean,
            row: Int,
            column: Int,
        ): Component = try {
            delegateRenderer.getTableCellRendererComponent(table, value, isSelected, hasFocus, row, column)
        } catch (_: RuntimeException) {
            delegateRenderer = DefaultTableCellRenderer()
            delegateRenderer.getTableCellRendererComponent(table, value, isSelected, hasFocus, row, column)
        }
    }

    private inner class HeaderClickHandler(
        private val table: JTable,
        private val delegate: SortingStrategy,
    ) : MouseAdapter() {
        private var mouseEventIsPerformingPopupTrigger = false

        init {
            table.tableHeader.addMouseListener(this)
        }

        override fun mouseClicked(event: MouseEvent) {
            if (mouseEventIsPerformingPopupTrigger) return
            if (table.tableHeader.cursor == Cursor.getPredefinedCursor(Cursor.E_RESIZE_CURSOR)) return
            if (!isSortingMouseEvent(event)) return

            val viewColumn = table.columnModel.getColumnIndexAtX(event.x)
            val column = table.convertColumnIndexToModel(viewColumn)
            val clicks = event.clickCount
            if (clicks >= 1 && column != -1) {
                delegate.columnClicked(
                    sortingState,
                    column,
                    clicks,
                    event.isShiftDown,
                    event.isControlDown || event.isMetaDown,
                )
            }
        }

        override fun mousePressed(event: MouseEvent) {
            mouseEventIsPerformingPopupTrigger = event.isPopupTrigger
        }

        fun dispose() {
            table.tableHeader.removeMouseListener(this)
        }
    }

    companion object {
        private val icons: Array<Icon?> = SortIconFactory.loadIcons()

        fun <E : Any> install(
            table: JTable,
            sortedList: SortedList<E>,
            strategy: SortingStrategy,
        ): TableComparatorChooser<E> = install(table, sortedList, strategy, getTableFormat(table.model))

        fun <E : Any> install(
            table: JTable,
            sortedList: SortedList<E>,
            strategy: SortingStrategy,
            tableFormat: TableFormat<in E>,
        ): TableComparatorChooser<E> = TableComparatorChooser(table, sortedList, strategy, tableFormat)

        @Suppress("UNCHECKED_CAST")
        private fun <E : Any> getTableFormat(tableModel: TableModel): TableFormat<in E> =
            (tableModel as AdvancedTableModel<E>).tableFormat
    }
}
