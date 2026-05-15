package mediathek.filmlisten.writer

import mediathek.daten.DatenFilm
import mediathek.daten.ListeFilme
import org.junit.jupiter.api.Assertions.*
import org.junit.jupiter.api.Assumptions.assumeTrue
import org.junit.jupiter.api.Test
import org.junit.jupiter.api.io.TempDir
import java.io.IOException
import java.nio.file.FileSystems
import java.nio.file.Files
import java.nio.file.Path
import java.nio.file.attribute.PosixFilePermissions

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
    fun `writeFilmList keeps existing file when replacement cannot be staged`() {
        assumeTrue(FileSystems.getDefault().supportedFileAttributeViews().contains("posix"))
        val target = tempDir.resolve("filmlist.json")
        Files.writeString(target, "existing filmlist")
        val originalPermissions = Files.getPosixFilePermissions(tempDir)

        try {
            Files.setPosixFilePermissions(tempDir, PosixFilePermissions.fromString("r-x------"))

            assertThrows(IOException::class.java) {
                FilmListWriter(false).writeFilmList(target.toString(), filmList(film("ARD", "Thema")))
            }

            assertEquals("existing filmlist", Files.readString(target))
        } finally {
            Files.setPosixFilePermissions(tempDir, originalPermissions)
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
        setNormalQualityUrl("https://example.test/$sender/$thema.mp4")
        websiteUrl = "https://example.test/$sender/$thema"
        subtitleUrl = ""
        lowQualityUrl = ""
        highQualityUrl = ""
    }
}
