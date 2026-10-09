package mediathek.audiothek.ui.main

import mediathek.audiothek.model.AudioEntry
import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Assertions.assertNull
import org.junit.jupiter.api.Test

class AudiothekOnlineSearchQueryTest {
    @Test
    fun `free search text is used unchanged for online search`() {
        assertEquals("krimi podcast", AudiothekOnlineSearchQuery.from("  krimi podcast  ")?.query)
    }

    @Test
    fun `thema field is converted to plain online query`() {
        assertEquals("hörspiel", AudiothekOnlineSearchQuery.from("thema:hörspiel")?.query)
        assertEquals("krimi", AudiothekOnlineSearchQuery.from("theme:krimi")?.query)
    }

    @Test
    fun `titel field is converted to plain online query`() {
        assertEquals("die drei ???", AudiothekOnlineSearchQuery.from("titel:\"die drei ???\"")?.query)
        assertEquals("wissen", AudiothekOnlineSearchQuery.from("title:*wissen?")?.query)
    }

    @Test
    fun `genre field alone does not create an online query`() {
        assertNull(AudiothekOnlineSearchQuery.from("genre:comedy"))
        assertNull(AudiothekOnlineSearchQuery.from("genre:\"true crime\""))
    }

    @Test
    fun `genre thema and titel fields can be combined with free text`() {
        assertEquals(
            "radiowissen geschichte bayern",
            AudiothekOnlineSearchQuery.from("genre:comedy thema:radiowissen titel:geschichte bayern")?.query
        )
    }

    @Test
    fun `genre field filters online results by genre`() {
        val request = AudiothekOnlineSearchQuery.from("genre:comedy science")!!
        assertEquals("science", request.query)
        val entries = listOf(
            audioEntry(genre = "Comedy", theme = "Other", title = "One"),
            audioEntry(genre = "Wissen", theme = "Comedy", title = "Two"),
            audioEntry(genre = "Wissen", theme = "Other", title = "Comedy Spezial")
        )

        assertEquals(listOf("One"), request.filter(entries).map { it.title })
    }

    @Test
    fun `thema and titel fields filter online results by theme and title`() {
        val request = AudiothekOnlineSearchQuery.from("thema:radiowissen titel:geschichte")!!
        val entries = listOf(
            audioEntry(genre = "Wissen", theme = "Radiowissen", title = "Geschichte Bayerns"),
            audioEntry(genre = "Wissen", theme = "Radiowissen", title = "Physik"),
            audioEntry(genre = "Wissen", theme = "Andere Reihe", title = "Geschichte Bayerns")
        )

        assertEquals(listOf("Geschichte Bayerns"), request.filter(entries).map { it.title })
    }

    @Test
    fun `local only lucene fields disable online search`() {
        assertNull(AudiothekOnlineSearchQuery.from("sender:ard"))
        assertNull(AudiothekOnlineSearchQuery.from("sender:ard thema:krimi"))
        assertNull(AudiothekOnlineSearchQuery.from("datum:12.03.2026"))
    }

    @Test
    fun `blank search text disables online search`() {
        assertNull(AudiothekOnlineSearchQuery.from("  "))
    }

    private fun audioEntry(genre: String, theme: String, title: String): AudioEntry =
        AudioEntry(
            channel = "Podcastindex",
            genre = genre,
            theme = theme,
            title = title,
            durationMinutes = null,
            sizeMb = null,
            description = "",
            audioUrl = null,
            websiteUrl = null,
            isNew = false,
            isPodcast = true,
            isDuplicate = false,
            publishedAt = null
        )
}
