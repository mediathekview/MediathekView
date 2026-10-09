package mediathek.tool.models

import mediathek.controller.DownloadColumn
import mediathek.daten.DatenDownload
import javax.swing.table.DefaultTableModel

class TModelDownload : DefaultTableModel() {
    override fun getColumnClass(columnIndex: Int): Class<*> =
        DownloadColumn.fromIndex(columnIndex).valueType

    override fun isCellEditable(row: Int, column: Int): Boolean = false

    override fun getColumnName(column: Int): String =
        DownloadColumn.fromIndex(column).title

    override fun getColumnCount(): Int = DownloadColumn.COUNT

    override fun getValueAt(row: Int, column: Int): Any? {
        val download = dataVector[row][DownloadColumn.REF.index] as DatenDownload
        val descriptor = DownloadColumn.fromIndex(column)
        return descriptor.valueFrom(download) ?: super.getValueAt(row, column)
    }
}
