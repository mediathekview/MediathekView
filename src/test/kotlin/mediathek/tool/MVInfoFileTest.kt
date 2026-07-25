package mediathek.tool

import mediathek.daten.DatenDownload
import mediathek.daten.DatenFilm
import okhttp3.HttpUrl.Companion.toHttpUrl
import org.junit.jupiter.api.Assertions.*
import org.junit.jupiter.api.Test
import org.junit.jupiter.api.io.TempDir
import java.io.IOException
import java.nio.file.Files
import java.nio.file.Path

internal class MVInfoFileTest {

    @TempDir
    lateinit var tempDir: Path

    @Test
    fun formatterWritesShortDescriptionOnOneLine() {
        assertEquals(
            "The Big Brown Fox Jumps over the Lazy Dog${System.lineSeparator()}${System.lineSeparator()}",
            MVInfoFileFormatter.format(infoFileData(description = "The Big Brown Fox Jumps over the Lazy Dog"))
                .descriptionSection(),
        )
    }

    @Test
    fun formatterWrapsLongDescription() {
        val result =
            "Weit hinten, hinter den Wortbergen, fern der Länder Vokalien${System.lineSeparator()}" +
                "und Konsonantien leben die Blindtexte. Abgeschieden wohnen sie${System.lineSeparator()}" +
                "in Buchstabhausen an der Küste des Semantik, eines großen${System.lineSeparator()}" +
                "Sprachozeans. Ein kleines Bächlein namens Duden fließt durch${System.lineSeparator()}" +
                "ihren Ort und versorgt sie mit den nötigen Regelialien. Es ist${System.lineSeparator()}" +
                "ein paradiesmatisches Land, in dem einem gebratene Satzteile${System.lineSeparator()}" +
                "in den Mund fliegen.${System.lineSeparator()}${System.lineSeparator()}"

        assertEquals(
            result,
            MVInfoFileFormatter.format(infoFileData(description = DESCRIPTION_TEXT)).descriptionSection(),
        )
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
    fun writeInfoFileReplacesExistingContent() {
        val target = tempDir.resolve("info.txt")
        Files.writeString(target, "old content")
        val infoFile = MVInfoFile { 2_500_000 }

        infoFile.writeInfoFile(film(), target, TEST_URL)

        val content = Files.readString(target)
        assertFalse(content.contains("old content"))
        assertTrue(content.contains("Sender:      Sender"))
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
    fun writeManualInfoFileWritesExtendedUrls() {
        val target = tempDir.resolve("manual-info.txt")
        val infoFile = MVInfoFile { 2_500_000 }
        val film = film().apply {
            highQualityUrl = TEST_HIGH_QUALITY_URL
            lowQualityUrl = TEST_LOW_QUALITY_URL
        }

        infoFile.writeManualInfoFile(film, target)

        val content = Files.readString(target)
        assertTrue(content.contains("URL${System.lineSeparator()}"))
        assertTrue(content.contains("HQ: $TEST_HIGH_QUALITY_URL${System.lineSeparator()}"))
        assertTrue(content.contains("Normal: $TEST_URL${System.lineSeparator()}"))
        assertTrue(content.contains("LQ: $TEST_LOW_QUALITY_URL${System.lineSeparator()}"))
    }

    @Test
    fun writeInfoFileRejectsDownloadWithoutUrl() {
        val target = tempDir.resolve("info.txt")
        val infoFile = MVInfoFile { 2_500_000 }
        val download = DatenDownload().apply {
            film = film()
            downloadUrl = "not a url"
            targetPathFileName = target.toString()
        }

        assertThrows(IOException::class.java) {
            infoFile.writeInfoFile(download)
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
        private const val TEST_HIGH_QUALITY_URL = "https://example.org/video-hd.mp4"
        private const val TEST_LOW_QUALITY_URL = "https://example.org/video-low.mp4"
        private const val TEST_SUBTITLE_URL = "https://example.org/subtitle.ttml"
        private const val DESCRIPTION_TEXT =
            "Weit hinten, hinter den Wortbergen, fern der Länder Vokalien und Konsonantien leben die Blindtexte. Abgeschieden wohnen sie in Buchstabhausen an der Küste des Semantik, eines großen Sprachozeans. Ein kleines Bächlein namens Duden fließt durch ihren Ort und versorgt sie mit den nötigen Regelialien. Es ist ein paradiesmatisches Land, in dem einem gebratene Satzteile in den Mund fliegen."

        private fun String.descriptionSection(): String =
            substringAfter("${TEST_URL}${System.lineSeparator()}${System.lineSeparator()}")

        private fun infoFileData(description: String): MVInfoFileData =
            MVInfoFileData(
                sender = "Sender",
                thema = "Thema",
                title = "Titel",
                sendeDatum = "01.01.2026",
                sendeZeit = "20:15",
                filmLength = "01:00:00",
                fileSize = FileSize.INVALID_SIZE.toLong(),
                websiteUrl = "https://example.org/film",
                urlLines = listOf(TEST_URL.toString()),
                subtitleUrl = null,
                description = description,
            )

        private fun film(): DatenFilm =
            DatenFilm().apply {
                sender = "Sender"
                thema = "Thema"
                title = "Titel"
                setSendeDatumFromFilmlistValue("01.01.2026")
                setSendeZeitFromFilmlistValue("20:15")
                setFilmLengthSeconds(3600)
                description = "Beschreibung"
                websiteUrl = "https://example.org/film"
                urlNormalQuality = TEST_URL.toString()
                init()
            }
    }
}
