package mediathek.controller.starter

import mediathek.daten.DatenDownload
import mediathek.daten.DatenFilm
import org.junit.jupiter.api.Assertions.*
import org.junit.jupiter.api.Test
import org.junit.jupiter.api.io.TempDir
import java.nio.file.Files
import java.nio.file.Path
import java.time.LocalDate
import java.time.LocalDateTime

internal class Mp4MetadataTest {
    @TempDir
    lateinit var tempDir: Path

    @Test
    fun buildsDefaultMetadataFromFilmAndDownloadFieldsWithoutCustomTagsOrWebsiteUrl() {
        val film = DatenFilm().apply {
            sender = "ARD"
            thema = "Tatort"
            title = "Borowski und das Meer"
            description = "Ein Fall an der Küste."
            websiteUrl = "https://example.invalid/film"
            subtitleUrl = "https://example.invalid/subtitle.vtt"
            availableUntil = LocalDate.of(2026, 12, 31)
            setSendeDateTime(LocalDateTime.of(2026, 7, 8, 20, 15, 0))
            urlNormalQuality = "https://example.invalid/video.mp4"
        }
        val download = DatenDownload().apply {
            this.film = film
            sender = film.sender
            topic = film.thema
            title = film.title
            date = film.sendeDatum
            time = film.sendeZeit
            subtitleUrl = film.subtitleUrl
        }

        val metadata = Mp4Metadata.defaultFor(download)

        assertEquals("Borowski und das Meer", metadata["title"])
        assertEquals("ARD", metadata["artist"])
        assertEquals("Tatort", metadata["album"])
        assertEquals("2026-07-08", metadata["date"])
        assertEquals("Ein Fall an der Küste.", metadata["description"])
        assertEquals("Ein Fall an der Küste.", metadata["synopsis"])
        assertEquals("Ein Fall an der Küste.", metadata["comment"])
        assertFalse(metadata.keys.any { it.startsWith("mediathekview_") })
        assertFalse(metadata.values.any { it.contains("example.invalid") })
        assertFalse(metadata.values.any { it == "MediathekView" })
    }

    @Test
    fun omitsBlankValuesAndConvertsGermanDateWhenFilmReferenceIsMissing() {
        val download = DatenDownload().apply {
            sender = "ZDF"
            topic = "Doku"
            title = "Titel"
            date = "08.07.2026"
            time = "20:15:00"
        }

        val metadata = Mp4Metadata.defaultFor(download)

        assertEquals("Titel", metadata["title"])
        assertEquals("ZDF", metadata["artist"])
        assertEquals("Doku", metadata["album"])
        assertEquals("2026-07-08", metadata["date"])
        assertTrue("description" !in metadata)
        assertTrue("comment" !in metadata)
    }

    @Test
    fun writerPassesOnlyStandardMetadataKeysToRemuxer() {
        val target = tempDir.resolve("video.mp4")
        Files.writeString(target, "not a real mp4")
        val download = DatenDownload().apply {
            title = "Titel"
            sender = "Sender"
            topic = "Thema"
            date = "08.07.2026"
            targetPathFileName = target.toString()
            isMp4Metadata = true
        }
        val captured = mutableMapOf<String, String>()

        Mp4Metadata.writeDefaultTags(
            download = download,
            ffmpegExecutable = Path.of("/usr/bin/ffmpeg"),
            remux = { _, _, temporaryTarget, metadata ->
                captured.putAll(metadata)
                Files.writeString(temporaryTarget, "remuxed")
                true
            },
        )

        assertEquals(setOf("title", "artist", "album", "date"), captured.keys)
    }

    @Test
    fun remuxOutputMapsInputFileByIndexInsteadOfFilterLabel() {
        val output = Mp4Metadata.remuxOutput(
            tempDir.resolve("out.mp4"),
            mapOf("title" to "Smoke Title"),
        )

        val arguments = output.buildArguments()

        assertEquals("0", arguments[arguments.indexOf("-map") + 1])
        assertFalse("[0]" in arguments)
        assertEquals("copy", arguments[arguments.indexOf("-c") + 1])
        assertEquals("title=Smoke Title", arguments[arguments.indexOf("-metadata") + 1])
    }
}
