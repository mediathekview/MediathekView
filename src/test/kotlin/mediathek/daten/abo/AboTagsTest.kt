package mediathek.daten.abo

import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Assertions.assertTrue
import org.junit.jupiter.api.Test

class AboTagsTest {

    @Test
    fun fromXmlTag() {
        val r = AboTags.fromXmlTag("Sender")
        assertTrue(r.isPresent)
        assertEquals(r.get(), AboTags.SENDER)
    }

    @Test
    fun fromIndex() {
        val r = AboTags.fromIndex(DatenAbo.ABO_NR)
        assertTrue(r.isPresent)
        assertEquals(r.get(), AboTags.NR)
    }
}