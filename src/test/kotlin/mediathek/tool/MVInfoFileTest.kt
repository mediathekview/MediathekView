package mediathek.tool

import mediathek.daten.DatenFilm
import okhttp3.HttpUrl.Companion.toHttpUrl
import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Assertions.assertFalse
import org.junit.jupiter.api.Assertions.assertThrows
import org.junit.jupiter.api.Assertions.assertTrue
import org.junit.jupiter.api.Test
import org.junit.jupiter.api.io.TempDir
import java.io.IOException
import java.nio.file.Files
import java.nio.file.Path

internal class MVInfoFileTest {

    @TempDir
    lateinit var tempDir: Path

    @Test
    fun splitDescriptionTextIntoOneLine() {
        val infoFile = MVInfoFile()
        assertEquals(
            "The Big Brown Fox Jumps over the Lazy Dog",
            infoFile.splitStringIntoMaxFixedLengthLines("The Big Brown Fox Jumps over the Lazy Dog", 50),
        )
    }

    @Test
    fun splitDescriptionTextIntoMore() {
        val result =
            "Weit hinten, hinter den Wortbergen, fern der Länder Vokalien${System.lineSeparator()}" +
                "und Konsonantien leben die Blindtexte. Abgeschieden wohnen sie${System.lineSeparator()}" +
                "in Buchstabhausen an der Küste des Semantik, eines großen${System.lineSeparator()}" +
                "Sprachozeans. Ein kleines Bächlein namens Duden fließt durch${System.lineSeparator()}" +
                "ihren Ort und versorgt sie mit den nötigen Regelialien. Es ist${System.lineSeparator()}" +
                "ein paradiesmatisches Land, in dem einem gebratene Satzteile${System.lineSeparator()}" +
                "in den Mund fliegen."
        val infoFile = MVInfoFile()
        assertEquals(result, infoFile.splitStringIntoMaxFixedLengthLines(DESCRIPTION_TEXT, 62))
    }

    @Test
    fun appendFormatedTableStringToEmptyStringBuilder() {
        val infoFile = MVInfoFile()
        val result = infoFile.appendFormattedTableLine(StringBuilder(), "%-12s %s", "Größe [MB]", "194")
        assertEquals("Größe [MB]:  194${System.lineSeparator()}", result.toString())
    }

    @Test
    fun writeInfoFileWritesExpectedContent() {
        val target = tempDir.resolve("nested").resolve("info.txt")
        val infoFile = MVInfoFile { 2_500_000 }
        val film = film().apply {
            subtitleUrl = TEST_SUBTITLE_URL
        }

        infoFile.writeInfoFile(film, target, TEST_URL)

        val content = Files.readString(target)
        assertTrue(content.contains("Sender:      Sender"))
        assertTrue(content.contains("Thema:       Thema"))
        assertTrue(content.contains("Titel:       Titel"))
        assertTrue(content.contains("Größe:"))
        assertTrue(content.contains("MiB"))
        assertTrue(content.contains(TEST_URL.toString()))
        assertTrue(content.contains("Subtitle-URL${System.lineSeparator()}$TEST_SUBTITLE_URL"))
    }

    @Test
    fun writeInfoFileOmitsSubtitleSectionWhenNoSubtitleUrlIsAvailable() {
        val target = tempDir.resolve("without-subtitle.txt")
        val infoFile = MVInfoFile { 2_500_000 }

        infoFile.writeInfoFile(film(), target, TEST_URL)

        val content = Files.readString(target)
        assertFalse(content.contains("Subtitle-URL${System.lineSeparator()}"))
    }

    @Test
    fun writeInfoFileRejectsMissingUrl() {
        val target = tempDir.resolve("info.txt")
        val infoFile = MVInfoFile { 2_500_000 }

        assertThrows(IOException::class.java) {
            infoFile.writeInfoFile(film(), target, null)
        }

        assertFalse(Files.exists(target))
    }

    @Test
    fun writeManualInfoFileRejectsInvalidFilmUrl() {
        val target = tempDir.resolve("manual-info.txt")
        val infoFile = MVInfoFile { 2_500_000 }
        val film = film().apply { urlNormalQuality = "not a url" }

        assertThrows(IOException::class.java) {
            infoFile.writeManualInfoFile(film, target)
        }

        assertFalse(Files.exists(target))
    }

    private companion object {
        private val TEST_URL = "https://example.org/video.mp4".toHttpUrl()
        private const val TEST_SUBTITLE_URL = "https://example.org/subtitle.ttml"
        private const val DESCRIPTION_TEXT =
            "Weit hinten, hinter den Wortbergen, fern der Länder Vokalien und Konsonantien leben die Blindtexte. Abgeschieden wohnen sie in Buchstabhausen an der Küste des Semantik, eines großen Sprachozeans. Ein kleines Bächlein namens Duden fließt durch ihren Ort und versorgt sie mit den nötigen Regelialien. Es ist ein paradiesmatisches Land, in dem einem gebratene Satzteile in den Mund fliegen."

        private fun film(): DatenFilm =
            DatenFilm().apply {
                sender = "Sender"
                thema = "Thema"
                title = "Titel"
                sendeDatum = "01.01.2026"
                sendeZeit = "20:15"
                setFilmLengthSeconds(3600)
                description = "Beschreibung"
                websiteUrl = "https://example.org/film"
                urlNormalQuality = TEST_URL.toString()
                init()
            }
    }
}
