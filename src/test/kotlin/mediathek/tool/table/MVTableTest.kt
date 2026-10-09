package mediathek.tool.table

import mediathek.config.application.ApplicationConfiguration
import mediathek.controller.DownloadColumn
import mediathek.daten.DatenDownload
import mediathek.tool.models.TModelDownload
import mediathek.tool.models.FilmColumn
import mediathek.tool.models.TModelFilm
import org.junit.jupiter.api.Assertions.*
import org.junit.jupiter.api.Test
import javax.swing.DefaultRowSorter
import javax.swing.SortOrder

class MVTableTest {
    @Test
    fun countNumberOfColumns() {
        var count = PersistentColumnConfigurationTable.countNumberOfColumns("a,b,c,d")
        assertEquals(4, count)

        count = PersistentColumnConfigurationTable.countNumberOfColumns("a,b,c,d,e")
        assertEquals(5, count)
    }

    @Test
    fun filmColumnConfigurationAcceptsPersistedTrailingPlaceholderColumns() {
        val config = ApplicationConfiguration.getInstance()
        val originalConfig = config.filmTableColumnConfiguration
        config.filmTableColumnConfiguration =
            "0,85,352,521,23,23,20,84,53,66,77,0,0,0,1389,0,0|0,1,2,3,4,5,6,13,7,8,9,10,11,12,14,0,0|7|DESCENDING"

        try {
            val table = MVFilmTable()
            table.model = TModelFilm()

            table.readColumnConfigurationData()

            assertEquals(FilmColumn.GEO.index, table.convertColumnIndexToModel(7))
        } finally {
            config.filmTableColumnConfiguration = originalConfig
        }
    }

    @Test
    fun filmTableUsesSingleSortKeyRowSorterWhenModelChanges() {
        val table = MVFilmTable()
        table.model = TModelFilm()

        val sorter = assertInstanceOf(DefaultRowSorter::class.java, table.rowSorter)
        sorter.sortKeys = listOf(
            javax.swing.RowSorter.SortKey(FilmColumn.TITLE.index, SortOrder.ASCENDING),
            javax.swing.RowSorter.SortKey(FilmColumn.SENDER.index, SortOrder.DESCENDING),
        )

        assertEquals(1, sorter.sortKeys.size)
        assertEquals(FilmColumn.TITLE.index, sorter.sortKeys.first().column)
        assertEquals(SortOrder.ASCENDING, sorter.sortKeys.first().sortOrder)
        assertTrue(!sorter.isSortable(FilmColumn.PLAY.index))
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
