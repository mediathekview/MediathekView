package mediathek.tool.table

import mediathek.audiothek.ui.table.TriStateTableRowSorter
import mediathek.config.MVConfig
import mediathek.daten.DatenFilm
import mediathek.daten.ListeAbo
import mediathek.daten.abo.DatenAbo
import mediathek.tool.models.TModelAbo
import mediathek.tool.models.TModelFilm
import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Assertions.assertInstanceOf
import org.junit.jupiter.api.Test
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
        val key = MVConfig.Configs.SYSTEM_EIGENSCHAFTEN_TABELLE_FILME
        val originalConfig = MVConfig.get(key)
        MVConfig.add(
            key,
            "0,85,352,521,23,23,20,84,53,66,77,0,0,0,1389,0,0|0,1,2,3,4,5,6,13,7,8,9,10,11,12,14,0,0|7|DESCENDING",
        )

        try {
            val table = MVFilmTable()
            table.model = TModelFilm()

            table.readColumnConfigurationData()

            assertEquals(DatenFilm.FILM_GEO, table.reihe[7])
        } finally {
            MVConfig.add(key, originalConfig)
        }
    }

    @Test
    fun abosTableUsesTriStateRowSorterWhenModelChanges() {
        val table = MVAbosTable()
        table.model = TModelAbo(ListeAbo())

        val sorter = assertInstanceOf(TriStateTableRowSorter::class.java, table.rowSorter)

        sorter.toggleSortOrder(DatenAbo.ABO_NAME)
        assertEquals(SortOrder.ASCENDING, sorter.sortKeys.first().sortOrder)

        sorter.toggleSortOrder(DatenAbo.ABO_NAME)
        assertEquals(SortOrder.DESCENDING, sorter.sortKeys.first().sortOrder)

        sorter.toggleSortOrder(DatenAbo.ABO_NAME)
        assertEquals(0, sorter.sortKeys.size)
    }
}
