package mediathek.daten

import mediathek.tool.FileSize
import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Assertions.assertTrue
import org.junit.jupiter.api.Test

internal class DatenDownloadTest {

    @Test
    fun formatTimeRemaining() {
        assertEquals("6 Min.", DownloadRuntimeText.formatTimeRemaining(360))
        assertEquals("5 Min.", DownloadRuntimeText.formatTimeRemaining(240))
        assertEquals("4 Min.", DownloadRuntimeText.formatTimeRemaining(180))
        assertEquals("3 Min.", DownloadRuntimeText.formatTimeRemaining(120))
        assertEquals("2 Min.", DownloadRuntimeText.formatTimeRemaining(70))
        assertEquals("1 Min.", DownloadRuntimeText.formatTimeRemaining(40))
        assertEquals("30 s", DownloadRuntimeText.formatTimeRemaining(25))
        assertEquals("20 s", DownloadRuntimeText.formatTimeRemaining(15))
        assertEquals("10 s", DownloadRuntimeText.formatTimeRemaining(8))
    }

    @Test
    fun convertsDownloadConfigAtModelBoundary() {
        val config = DownloadConfig(
            aboName = "Abo",
            sender = "Sender",
            topic = "Topic",
            title = "Title",
            sizeInMiB = 42,
            date = "01.06.2026",
            time = "20:15:00",
            duration = "00:45:00",
            interrupted = true,
            filmUrl = "https://example.invalid/film",
            historyUrl = "https://example.invalid/history",
            url = "https://example.invalid/download.mp4",
            rtmpUrl = "rtmp://example.invalid/download",
            subtitleUrl = "https://example.invalid/subtitle.vtt",
            programSet = "Set",
            program = "Program",
            programInvocation = "program invocation",
            programInvocationArray = "program|invocation",
            restart = true,
            targetFileName = "target.mp4",
            targetPath = "/tmp",
            targetPathFileName = "/tmp/target.mp4",
            type = DownloadType.PROGRAM,
            source = DownloadSource.DOWNLOAD,
            deferred = true,
            infoFile = true,
            spotlight = true,
            subtitle = true,
            downloadManager = true,
        )

        val download = DatenDownload.fromConfig(config)
        val roundTripConfig = download.toConfig()

        assertEquals(config, roundTripConfig)
        assertEquals(DownloadType.PROGRAM, download.art)
        assertEquals(DownloadSource.DOWNLOAD, download.quelle)
        assertEquals(42L * FileSize.ONE_MiB, download.runtime.filmSize.size)
    }

    @Test
    fun buildsProgramInvocationFromDownloadContext() {
        val program = DatenProg(
            "Program",
            "program",
            "--target ** --url %f --rtmp %F --path %a --name %b --web %w",
            false.toString(),
            false.toString(),
        )

        val invocation = DownloadProgramInvocationBuilder.build(
            downloadType = DownloadType.PROGRAM,
            program = program,
            request = DownloadInvocationRequest(
                downloadUrl = "https://example.invalid/download.mp4",
                rtmpUrl = "rtmp://example.invalid/download",
                targetPath = "/tmp",
                targetFileName = "download.mp4",
                targetPathFileName = "/tmp/download.mp4",
                websiteUrl = "https://example.invalid/film",
            ),
        )

        assertEquals(
            "program --target /tmp/download.mp4 --url https://example.invalid/download.mp4 " +
                "--rtmp rtmp://example.invalid/download --path /tmp --name download.mp4 " +
                "--web https://example.invalid/film",
            invocation.command,
        )
    }

    @Test
    fun buildsTargetFromProgramSetAndFilmFields() {
        val film = DatenFilm().apply {
            sender = "Sender One"
            thema = "Topic One"
            title = "Title One"
            sendeDatum = "01.06.2026"
            sendeZeit = "20:15:00"
            setNormalQualityUrl("https://example.invalid/video.mp4")
        }
        val programSet = DatenPset("Set").apply {
            zielDateiname = "%s-%t-%T.%S"
            zielPfad = "/downloads/%s"
            addProg(DatenProg("Program", "program", "--target **", false.toString(), false.toString()))
        }

        val target = DownloadTargetBuilder.build(
            DownloadTargetRequest(
                pSet = programSet,
                film = film,
                abo = null,
                requestedFileName = "",
                requestedPath = "",
                downloadUrl = film.urlNormalQuality,
                topic = "Topic One",
                title = "Title One",
            ),
        )

        assertEquals("Sender One-Topic One-Title One.mp4", target.fileName)
        assertEquals("/downloads/Sender One/Topic One", target.path)
        assertEquals("/downloads/Sender One/Topic One/Sender One-Topic One-Title One.mp4", target.pathFileName)
    }

    @Test
    fun constructorSeedsNormalQualitySizeFromFilmList() {
        val film = DatenFilm().apply {
            sender = "Sender One"
            thema = "Topic One"
            title = "Title One"
            sendeDatum = "01.06.2026"
            sendeZeit = "20:15:00"
            setNormalQualityUrl("https://example.invalid/video.mp4")
            fileSize.setSize("123")
        }
        val programSet = createProgramSet()

        val download = DatenDownload(programSet, film, DownloadSource.ABO, null, "", "", "")

        assertEquals(123L * FileSize.ONE_MiB, download.runtime.filmSize.size)
    }

    @Test
    fun constructorSeedsNormalQualitySizeWhenDownloadUrlParametersAreRemoved() {
        val film = DatenFilm().apply {
            sender = "Sender One"
            thema = "Topic One"
            title = "Title One"
            sendeDatum = "01.06.2026"
            sendeZeit = "20:15:00"
            setNormalQualityUrl("https://example.invalid/video.mp4?token=temporary")
            fileSize.setSize("456")
        }
        val programSet = createProgramSet()

        val download = DatenDownload(programSet, film, DownloadSource.ABO, null, "", "", "")

        assertEquals("https://example.invalid/video.mp4", download.downloadUrl)
        assertEquals(456L * FileSize.ONE_MiB, download.runtime.filmSize.size)
    }

    @Test
    fun setGroesseFromFilmSeedsNormalQualitySizeWhenDownloadUrlHasParameters() {
        val film = DatenFilm().apply {
            setNormalQualityUrl("https://example.invalid/video.mp4")
            fileSize.setSize("789")
        }
        val download = DatenDownload().apply {
            this.film = film
            downloadUrl = "https://example.invalid/video.mp4?token=temporary"
        }

        download.setGroesseFromFilm()

        assertEquals(789L * FileSize.ONE_MiB, download.runtime.filmSize.size)
    }

    @Test
    fun copyPreservesWebsiteUrlForProgramInvocationRebuilds() {
        val film = DatenFilm().apply {
            sender = "Sender One"
            thema = "Topic One"
            title = "Title One"
            sendeDatum = "01.06.2026"
            sendeZeit = "20:15:00"
            setNormalQualityUrl("https://example.invalid/video.mp4")
            websiteUrl = "https://example.invalid/film-page"
        }
        val programSet = DatenPset("Set").apply {
            zielDateiname = "%s-%t-%T.%S"
            zielPfad = "/downloads/%s"
            addProg(DatenProg("Program", "program", "--web %w --target **", false.toString(), false.toString()))
        }

        val copiedDownload = DatenDownload(
            programSet,
            film,
            DownloadSource.DOWNLOAD,
            null,
            "",
            "",
            "",
        ).copy

        copiedDownload.programInvocation = ""
        copiedDownload.programInvocationArray = ""
        copiedDownload.aufrufBauen()

        assertTrue(copiedDownload.programInvocation.contains("--web https://example.invalid/film-page"))
    }

    private fun createProgramSet(): DatenPset =
        DatenPset("Set").apply {
            aufloesung = FilmResolution.Enum.NORMAL
            zielDateiname = "%t.%S"
            zielPfad = "/downloads"
            addProg(DatenProg("Program", "program", "--target **", false.toString(), false.toString()))
        }
}
