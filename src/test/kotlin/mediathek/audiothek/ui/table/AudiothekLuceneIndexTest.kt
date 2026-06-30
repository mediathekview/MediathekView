package mediathek.audiothek.ui.table

import mediathek.audiothek.model.AudioEntry
import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Test

internal class AudiothekLuceneIndexTest {

    @Test
    fun `search maps lucene hits back to original entries by doc values row index`() {
        AudiothekLuceneIndex.build(
            listOf(
                audioEntry(title = "Morning News"),
                audioEntry(title = "Science Weekly"),
                audioEntry(title = "Evening News"),
            )
        ).use { index ->
            val titles = index.search("news", listOf(AudiothekLuceneIndex.FIELD_TITLE))
                .map { it.title }

            assertEquals(listOf("Morning News", "Evening News"), titles)
        }
    }

    @Test
    fun `replace entries rebuilds row index mapping`() {
        AudiothekLuceneIndex.build(
            listOf(
                audioEntry(title = "Old Entry"),
                audioEntry(title = "Another Old Entry"),
            )
        ).use { index ->
            index.replaceEntries(
                listOf(
                    audioEntry(title = "Unrelated Entry"),
                    audioEntry(title = "Replacement Entry"),
                )
            )

            val titles = index.search("replacement", listOf(AudiothekLuceneIndex.FIELD_TITLE))
                .map { it.title }

            assertEquals(listOf("Replacement Entry"), titles)
        }
    }

    private fun audioEntry(title: String): AudioEntry =
        AudioEntry(
            channel = "Test Channel",
            genre = "Podcast",
            theme = "Science",
            title = title,
            durationMinutes = null,
            sizeMb = null,
            description = "",
            audioUrl = null,
            websiteUrl = null,
            isNew = false,
            isPodcast = true,
            isDuplicate = false,
            publishedAt = null,
        )
}
