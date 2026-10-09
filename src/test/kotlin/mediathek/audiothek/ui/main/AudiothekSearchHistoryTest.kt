package mediathek.audiothek.ui.main

import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Test

class AudiothekSearchHistoryTest {
    @Test
    fun `codec round trips JSON string array`() {
        val encoded = AudiothekSearchHistoryCodec.encode(listOf("thema:hörspiel", "titel:\"die drei ???\""))

        assertEquals("""["thema:hörspiel","titel:\"die drei ???\""]""", encoded)
        assertEquals(
            listOf("thema:hörspiel", "titel:\"die drei ???\""),
            AudiothekSearchHistoryCodec.decode(encoded)
        )
    }

    @Test
    fun `normalization trims blanks and preserves first occurrence order`() {
        assertEquals(
            listOf("alpha", "beta", "gamma"),
            AudiothekSearchHistoryCodec.normalize(listOf(" alpha ", "", "beta", "alpha", " gamma "))
        )
    }

    @Test
    fun `codec rejects non array or malformed history`() {
        assertEquals(emptyList<String>(), AudiothekSearchHistoryCodec.decode("alpha"))
        assertEquals(emptyList<String>(), AudiothekSearchHistoryCodec.decode("""["alpha",123]"""))
        assertEquals(emptyList<String>(), AudiothekSearchHistoryCodec.decode("""["alpha" "beta"]"""))
        assertEquals(emptyList<String>(), AudiothekSearchHistoryCodec.decode("""["alpha",]"""))
        assertEquals(emptyList<String>(), AudiothekSearchHistoryCodec.decode("  "))
    }

    @Test
    fun `history is capped at fifty entries`() {
        val entries = (1..60).map { "query-$it" }

        assertEquals((1..50).map { "query-$it" }, AudiothekSearchHistoryCodec.normalize(entries))
    }
}
