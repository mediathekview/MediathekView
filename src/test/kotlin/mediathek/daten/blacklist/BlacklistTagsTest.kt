package mediathek.daten.blacklist

import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Assertions.assertTrue
import org.junit.jupiter.api.Test

class BlacklistTagsTest {

    @Test
    fun fromXmlTag() {
        val r = BlacklistTags.fromXmlTag("black-sender")
        assertTrue(r.isPresent)
        assertEquals(r.get(), BlacklistTags.SENDER)
    }
}