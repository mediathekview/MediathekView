package mediathek.gui.duplicates

import ca.odell.glazedlists.BasicEventList
import ca.odell.glazedlists.TransactionList
import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Assertions.assertThrows
import org.junit.jupiter.api.Test
import java.util.AbstractMap.SimpleImmutableEntry

class FilmStatisticsListUpdaterTest {
    @Test
    fun `replacement publishes one aggregate event`() {
        val statistics = TransactionList<FilmStatistics>(BasicEventList()).apply {
            add(FilmStatistics("old", 1))
        }
        var eventCount = 0
        statistics.addListEventListener { eventCount++ }

        replaceFilmStatistics(statistics, linkedMapOf("ARD" to 2L, "ZDF" to 3L))

        assertEquals(
            listOf(FilmStatistics("ARD", 2), FilmStatistics("ZDF", 3)),
            statistics,
        )
        assertEquals(1, eventCount)
    }

    @Test
    fun `replacement rolls back when reading statistics fails`() {
        val original = FilmStatistics("old", 1)
        val statistics = TransactionList<FilmStatistics>(BasicEventList()).apply { add(original) }
        var eventCount = 0
        statistics.addListEventListener { eventCount++ }

        assertThrows(IllegalStateException::class.java) {
            replaceFilmStatistics(statistics, failingStatistics())
        }

        assertEquals(listOf(original), statistics)
        assertEquals(0, eventCount)
    }

    private fun failingStatistics(): Map<String, Long> = object : AbstractMap<String, Long>() {
        override val entries: Set<Map.Entry<String, Long>> = object : AbstractSet<Map.Entry<String, Long>>() {
            override val size: Int = 2

            override fun iterator(): Iterator<Map.Entry<String, Long>> = object : Iterator<Map.Entry<String, Long>> {
                private var index = 0

                override fun hasNext(): Boolean = index < 2

                override fun next(): Map.Entry<String, Long> = when (index++) {
                    0 -> SimpleImmutableEntry("ARD", 2L)
                    else -> throw IllegalStateException("statistics unavailable")
                }
            }
        }
    }
}
