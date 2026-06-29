package mediathek.tool.table

import mediathek.config.application.ApplicationConfiguration
import mediathek.controller.DownloadColumns
import mediathek.daten.DatenDownload
import mediathek.daten.DatenFilm
import mediathek.tool.models.TModelDownload
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

            assertEquals(DatenFilm.FILM_GEO, table.convertColumnIndexToModel(7))
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
            javax.swing.RowSorter.SortKey(DatenFilm.FILM_TITEL, SortOrder.ASCENDING),
            javax.swing.RowSorter.SortKey(DatenFilm.FILM_SENDER, SortOrder.DESCENDING),
        )

        assertEquals(1, sorter.sortKeys.size)
        assertEquals(DatenFilm.FILM_TITEL, sorter.sortKeys.first().column)
        assertEquals(SortOrder.ASCENDING, sorter.sortKeys.first().sortOrder)
        assertTrue(!sorter.isSortable(DatenFilm.FILM_ABSPIELEN))
    }

    @Test
    fun downloadModelReturnsLiveValuesForColumnsLoadedWhileHidden() {
        val download = DatenDownload()
        download.aboName = "Daily Abo"

        val row = Array<Any>(DownloadColumns.COUNT) { "" }
        row[DownloadColumns.REF] = download

        val model = TModelDownload()
        model.addRow(row)

        assertEquals("Daily Abo", model.getValueAt(0, DownloadColumns.ABO))
    }
}
