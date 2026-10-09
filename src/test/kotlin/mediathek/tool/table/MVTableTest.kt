package mediathek.tool.table

import mediathek.controller.DownloadColumn
import mediathek.daten.DatenDownload
import mediathek.tool.models.TModelDownload
import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Test

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
}
