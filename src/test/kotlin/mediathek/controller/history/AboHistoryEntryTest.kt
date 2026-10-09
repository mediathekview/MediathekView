package mediathek.controller.history

import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Assertions.assertNull
import org.junit.jupiter.api.Test
import java.time.LocalDate

internal class AboHistoryEntryTest {

    @Test
    fun parseCreatesEntryFromLegacyDateFormat() {
        val entry = AboHistoryEntry.parse("02.11.2020", "Thema", "Titel", "https://example.org/a.mp4")

        requireNotNull(entry)
        assertEquals(LocalDate.of(2020, 11, 2), entry.date)
        assertEquals("02.11.2020", entry.formattedDate)
    }

    @Test
    fun parseRejectsInvalidDate() {
        val entry = AboHistoryEntry.parse("not-a-date", "Thema", "Titel", "https://example.org/a.mp4")

        assertNull(entry)
    }
}
