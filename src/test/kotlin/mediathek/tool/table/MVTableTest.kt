package mediathek.tool.table

import mediathek.controller.DownloadColumn
import mediathek.daten.DatenDownload
import mediathek.swing.SwingDispatch
import mediathek.tool.MessageBus
import mediathek.tool.models.TModelDownload
import org.junit.jupiter.api.Assertions.assertArrayEquals
import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Test
import javax.swing.RowFilter
import javax.swing.table.DefaultTableModel
import javax.swing.table.TableModel
import javax.swing.table.TableRowSorter

class MVTableTest {
    @Test
    fun countNumberOfColumns() {
        var count = PersistentColumnConfigurationTable.countNumberOfColumns("a,b,c,d")
        assertEquals(4, count)

        count = PersistentColumnConfigurationTable.countNumberOfColumns("a,b,c,d,e")
        assertEquals(5, count)
    }

    @Test
    fun downloadModelReturnsLiveValuesForColumnsLoadedWhileHidden() {
        val download = DatenDownload()
        download.aboName = "Daily Abo"
        download.sender = "ZDF"

        val row = Array<Any>(DownloadColumn.COUNT) { "" }
        row[DownloadColumn.REF.index] = download

        val model = TModelDownload()
        model.addRow(row)

        assertEquals(DownloadColumn.COUNT, model.columnCount)
        assertEquals("Abo", model.getColumnName(DownloadColumn.ABO.index))
        assertEquals("Größe [MB]", model.getColumnName(DownloadColumn.SIZE.index))
        assertEquals(String::class.java, model.getColumnClass(DownloadColumn.ABO.index))
        assertEquals(Boolean::class.javaObjectType, model.getColumnClass(DownloadColumn.DEFERRED.index))

        assertEquals("Daily Abo", model.getValueAt(0, DownloadColumn.ABO.index))
        assertEquals("ZDF", model.getValueAt(0, DownloadColumn.SENDER.index))
    }

    @Test
    fun sortedTableRestoresSelectionWithinTheVisibleRowCount() {
        SwingDispatch.runAndWait {
            val table = object : MVTable(1, ColumnVisibilityStore.create(1), null, null) {
                override fun spaltenAusschalten() = Unit
            }
            try {
                val model = DefaultTableModel(arrayOf("Value"), 0)
                for (value in listOf(4, 1, 3, 2)) {
                    model.addRow(arrayOf(value))
                }
                table.model = model
                val sorter = TableRowSorter<TableModel>(model)
                sorter.toggleSortOrder(0)
                table.rowSorter = sorter
                table.setRowSelectionInterval(0, 0)
                table.addRowSelectionInterval(3, 3)

                table.fireTableDataChanged(true)
                assertArrayEquals(intArrayOf(0, 3), table.selectedRows)

                table.getSpalten()
                sorter.rowFilter = RowFilter.numberFilter(RowFilter.ComparisonType.BEFORE, 3, 0)
                table.clearSelection()
                table.setSpalten()
                assertEquals(4, model.rowCount)
                assertEquals(2, table.rowCount)
                assertArrayEquals(intArrayOf(0), table.selectedRows)

                table.setRowSelectionInterval(1, 1)
                table.getSpalten()
                sorter.rowFilter = RowFilter.numberFilter(RowFilter.ComparisonType.BEFORE, 2, 0)
                table.setSpalten()
                assertEquals(1, table.rowCount)
                assertEquals(-1, table.selectedRow)
            } finally {
                MessageBus.messageBus.unsubscribe(table)
            }
        }
    }
}
