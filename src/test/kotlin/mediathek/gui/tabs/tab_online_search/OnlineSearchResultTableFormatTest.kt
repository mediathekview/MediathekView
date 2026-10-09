package mediathek.gui.tabs.tab_online_search

import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Assertions.assertTrue
import org.junit.jupiter.api.Test
import java.time.Duration
import java.time.LocalDateTime

class OnlineSearchResultTableFormatTest {
    @Test
    fun `format exposes expected columns`() {
        val format = OnlineSearchResultTableFormat()

        assertEquals(6, format.getColumnCount())
        assertEquals("Sender", format.getColumnName(OnlineSearchResultTableFormat.SENDER))
        assertEquals("Thema", format.getColumnName(OnlineSearchResultTableFormat.TOPIC))
        assertEquals("Titel", format.getColumnName(OnlineSearchResultTableFormat.TITLE))
        assertEquals("Datum", format.getColumnName(OnlineSearchResultTableFormat.DATE))
        assertEquals("Dauer", format.getColumnName(OnlineSearchResultTableFormat.DURATION))
        assertEquals("Website", format.getColumnName(OnlineSearchResultTableFormat.WEBSITE))
        assertEquals(String::class.java, format.getColumnClass(OnlineSearchResultTableFormat.TITLE))
        assertEquals(String::class.java, format.getColumnClass(OnlineSearchResultTableFormat.DATE))
        assertEquals(String::class.java, format.getColumnClass(OnlineSearchResultTableFormat.DURATION))
    }

    @Test
    fun `format reads result values without DatenFilm table constants`() {
        val format = OnlineSearchResultTableFormat()
        val result = OnlineSearchResult(
            provider = OnlineSearchProvider.ZDF,
            sender = "ZDF",
            topic = "heute journal",
            title = "Sendung vom 02.01.2026",
            normalQualityUrl = "https://cdn.example/zdf.mp4",
            highQualityUrl = "https://cdn.example/zdf-hd.mp4",
            broadcastTime = LocalDateTime.of(2026, 1, 2, 21, 45),
            duration = Duration.ofMinutes(30),
        )

        assertEquals("ZDF", format.getColumnValue(result, OnlineSearchResultTableFormat.SENDER))
        assertEquals("heute journal", format.getColumnValue(result, OnlineSearchResultTableFormat.TOPIC))
        assertEquals("02.01.2026", format.getColumnValue(result, OnlineSearchResultTableFormat.DATE))
        assertEquals("00:30:00", format.getColumnValue(result, OnlineSearchResultTableFormat.DURATION))
    }

    @Test
    fun `format displays missing duration as empty text`() {
        val format = OnlineSearchResultTableFormat()
        val result = OnlineSearchResult(
            provider = OnlineSearchProvider.ZDF,
            sender = "ZDF",
            topic = "heute journal",
            title = "Sendung vom 02.01.2026",
            normalQualityUrl = "https://cdn.example/zdf.mp4",
        )

        assertEquals("", format.getColumnValue(result, OnlineSearchResultTableFormat.DURATION))
        assertEquals("", format.getColumnValue(result, OnlineSearchResultTableFormat.DATE))
    }

    @Test
    fun `date comparator sorts chronologically instead of formatted text lexicographically`() {
        val format = OnlineSearchResultTableFormat()
        @Suppress("UNCHECKED_CAST")
        val comparator = format.getColumnComparator(OnlineSearchResultTableFormat.DATE) as Comparator<Any>

        val newerDayOlderYear = format.getColumnValue(
            result(broadcastTime = LocalDateTime.of(2025, 1, 31, 20, 15)),
            OnlineSearchResultTableFormat.DATE,
        )
        val olderDayNewerYear = format.getColumnValue(
            result(broadcastTime = LocalDateTime.of(2026, 1, 1, 20, 15)),
            OnlineSearchResultTableFormat.DATE,
        )

        assertTrue(comparator.compare(newerDayOlderYear, olderDayNewerYear) < 0)
    }

    @Test
    fun `duration comparator sorts by length instead of formatted text lexicographically`() {
        val format = OnlineSearchResultTableFormat()
        @Suppress("UNCHECKED_CAST")
        val comparator = format.getColumnComparator(OnlineSearchResultTableFormat.DURATION) as Comparator<Any>

        val ninetyNineHours = format.getColumnValue(
            result(duration = Duration.ofHours(99)),
            OnlineSearchResultTableFormat.DURATION,
        )
        val hundredHours = format.getColumnValue(
            result(duration = Duration.ofHours(100)),
            OnlineSearchResultTableFormat.DURATION,
        )

        assertTrue(comparator.compare(ninetyNineHours, hundredHours) < 0)
    }

    @Test
    fun `topic comparator sorts display topics`() {
        val format = OnlineSearchResultTableFormat()
        @Suppress("UNCHECKED_CAST")
        val comparator = format.getColumnComparator(OnlineSearchResultTableFormat.TOPIC) as Comparator<Any>

        assertTrue(comparator.compare("Alpha", "Beta") < 0)
    }

    @Test
    fun `title comparator sorts titles`() {
        val format = OnlineSearchResultTableFormat()
        @Suppress("UNCHECKED_CAST")
        val comparator = format.getColumnComparator(OnlineSearchResultTableFormat.TITLE) as Comparator<Any>

        assertTrue(comparator.compare("Alpha", "Beta") < 0)
    }

    private fun result(
        broadcastTime: LocalDateTime? = null,
        duration: Duration? = null,
    ) = OnlineSearchResult(
        provider = OnlineSearchProvider.ZDF,
        sender = "ZDF",
        topic = "heute journal",
        title = "Sendung",
        normalQualityUrl = "https://cdn.example/zdf.mp4",
        broadcastTime = broadcastTime,
        duration = duration,
    )
}
