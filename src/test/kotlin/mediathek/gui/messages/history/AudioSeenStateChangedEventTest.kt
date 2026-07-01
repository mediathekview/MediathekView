package mediathek.gui.messages.history

import mediathek.audiothek.model.AudioEntry
import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Assertions.assertSame
import org.junit.jupiter.api.Test
import java.net.URI

class AudioSeenStateChangedEventTest {
    @Test
    fun carriesSeenStateAndChangedAudioEntries() {
        val entry = AudioEntry(
            channel = "Channel",
            genre = "Genre",
            theme = "Theme",
            title = "Title",
            durationMinutes = 12,
            sizeMb = 34,
            description = "Description",
            audioUrl = URI("https://example.org/audio.mp3"),
            websiteUrl = URI("https://example.org/audio"),
            isNew = false,
            isPodcast = false,
            isDuplicate = false,
            publishedAt = null
        )

        val event = AudioSeenStateChangedEvent(seen = true, entries = listOf(entry))

        assertEquals(true, event.seen)
        assertEquals(1, event.entries.size)
        assertSame(entry, event.entries.single())
    }
}
