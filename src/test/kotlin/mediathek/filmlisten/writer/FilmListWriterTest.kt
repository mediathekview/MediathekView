package mediathek.filmlisten.writer

import mediathek.daten.DatenFilm
import mediathek.daten.ListeFilme
import org.junit.jupiter.api.Assertions.*
import org.junit.jupiter.api.Test
import org.junit.jupiter.api.io.TempDir
import java.io.IOException
import java.nio.file.Files
import java.nio.file.Path

class FilmListWriterTest {
    @TempDir
    lateinit var tempDir: Path

    @Test
    fun `writeFilmList does not reorder input list when compressing sender tags`() {
        val first = film("ZDF", "Z Thema")
        val second = film("ARD", "A Thema")
        val films = filmList(first, second)

        FilmListWriter(false).writeFilmList(tempDir.resolve("filmlist.json").toString(), films)

        assertSame(first, films[0])
        assertSame(second, films[1])
    }

    @Test
    fun `writeFilmList keeps existing target when replacement fails`() {
        val target = tempDir.resolve("filmlist.json")
        Files.createDirectory(target)
        Files.writeString(target.resolve("existing-entry"), "existing filmlist")

        assertThrows(IOException::class.java) {
            FilmListWriter(false).writeFilmList(target.toString(), filmList(film("ARD", "Thema")))
        }

        assertTrue(Files.isDirectory(target))
        assertEquals("existing filmlist", Files.readString(target.resolve("existing-entry")))
        Files.list(tempDir).use { paths ->
            assertEquals(listOf(target), paths.toList())
        }
    }

    private fun filmList(vararg films: DatenFilm) = ListeFilme().apply {
        metaData.datum = "15.05.2026, 12:00"
        metaData.id = "test-list"
        addAll(films)
    }

    private fun film(sender: String, thema: String) = DatenFilm().apply {
        this.sender = sender
        this.thema = thema
        title = "Titel"
        sendeDatum = "15.05.2026"
        sendeZeit = "12:00:00"
        description = "Beschreibung"
        urlNormalQuality = "https://example.test/$sender/$thema.mp4"
        websiteUrl = "https://example.test/$sender/$thema"
        subtitleUrl = ""
        lowQualityUrl = ""
        highQualityUrl = ""
    }
}
