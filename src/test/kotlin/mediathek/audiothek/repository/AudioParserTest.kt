package mediathek.audiothek.repository

import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Assertions.assertNotNull
import org.junit.jupiter.api.Test
import java.io.ByteArrayInputStream

class AudioParserTest {
    @Test
    fun `parse handles nested audio row arrays`() {
        val dataset = AudioParser().parse(
            ByteArrayInputStream(NESTED_REMOTE_AUDIO_PAYLOAD.toByteArray()),
            "https://example.invalid/audios.json",
        )

        assertNotNull(dataset.metaLocal)
        assertEquals(1, dataset.entries.size)
        assertEquals("Remote Sender", dataset.entries[0].channel)
        assertEquals("Remote Thema", dataset.entries[0].genre)
        assertEquals("Remote Genre", dataset.entries[0].theme)
        assertEquals("Remote Titel", dataset.entries[0].title)
    }

    @Test
    fun `parse handles repeated audios fields with flat row arrays`() {
        val dataset = AudioParser().parse(
            ByteArrayInputStream(REPEATED_FIELD_AUDIO_PAYLOAD.toByteArray()),
            "https://example.invalid/audios.xz",
        )

        assertNotNull(dataset.metaLocal)
        assertEquals(1, dataset.entries.size)
        assertEquals("Remote Sender", dataset.entries[0].channel)
        assertEquals("Remote Thema", dataset.entries[0].genre)
        assertEquals("Remote Genre", dataset.entries[0].theme)
        assertEquals("Remote Titel", dataset.entries[0].title)
    }

    companion object {
        private val NESTED_REMOTE_AUDIO_PAYLOAD = """
            {
              "AudioList": ["AudioList", "22.04.2026 10:00:00"],
              "Audios": [
                ["Sender", "Genre", "Thema", "Titel", "Datum", "Zeit", "Dauer", "Größe", "Beschreibung", "Url", "Website", "Neu", "Podcast", "Doppelt"],
                ["Remote Sender", "Remote Genre", "Remote Thema", "Remote Titel", "22.04.2026", "10:15", "120", "2", "Remote Beschreibung", "https://example.invalid/remote.mp3", "https://example.invalid/remote", "false", "true", "false"]
              ]
            }
        """.trimIndent()

        private val REPEATED_FIELD_AUDIO_PAYLOAD = """
            {
              "AudioList" : [ "22.04.2026 13:03:45", "22.04.2026 15:03:45" ],
              "Audios" : [ "Sender", "Genre", "Thema", "Titel", "Datum", "Zeit", "Dauer", "Größe", "Beschreibung", "Url", "Website", "Neu", "Podcast", "Doppelt" ],
              "Audios" : [ "Remote Sender", "Remote Genre", "Remote Thema", "Remote Titel", "22.04.2026", "10:15", "120", "2", "Remote Beschreibung", "https://example.invalid/remote.mp3", "https://example.invalid/remote", "false", "true", "false" ]
            }
        """.trimIndent()
    }
}
