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
            selectedResolution = FilmResolution.Enum.LOW,
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
        assertEquals(FilmResolution.Enum.LOW, download.selectedResolution)
        assertEquals(42L * FileSize.ONE_MIB, download.runtime.filmSize.size)
    }

    @Test
    fun fileNameWithoutSuffixStripsLikelyWebExtension() {
        val download = DatenDownload().apply {
            targetPathFileName = "https://ios-ondemand.swr.de/i/swr-fernsehen/bw-extra/20130202/601676.,m,s,l,.mp4.csmil/index_2_av.m3u8?e=b471643725c47acd"
        }

        assertEquals(
            "https://ios-ondemand.swr.de/i/swr-fernsehen/bw-extra/20130202/601676.,m,s,l,.mp4.csmil/index_2_av",
            download.fileNameWithoutSuffix,
        )
    }

    @Test
    fun fileNameWithoutSuffixStripsFileExtension() {
        val download = DatenDownload().apply {
            targetPathFileName = "/Users/derreisende/file1.mp4"
        }

        assertEquals("/Users/derreisende/file1", download.fileNameWithoutSuffix)
    }

    @Test
    fun fileNameWithoutSuffixKeepsQuestionMarksInPath() {
        val download = DatenDownload().apply {
            targetPathFileName =
                "/Users/derreisende/Downloads/mediathek/Die Nordreportage/Die Nordreportage-Wie geht das? Fertigung eines Windrades-0143177029.mp4"
        }

        assertEquals(
            "/Users/derreisende/Downloads/mediathek/Die Nordreportage/Die Nordreportage-Wie geht das? Fertigung eines Windrades-0143177029",
            download.fileNameWithoutSuffix,
        )
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
    fun buildsArteHlsProgramMappingForSelectedQuality() {
        val program = DatenProg(
            "ffmpeg",
            "ffmpeg",
            "-i %f -c copy -bsf:a aac_adtstoasc **",
            false.toString(),
            false.toString(),
        )

        val invocation = DownloadProgramInvocationBuilder.build(
            downloadType = DownloadType.PROGRAM,
            program = program,
            request = DownloadInvocationRequest(
                downloadUrl = "https://manifest-arte.akamaized.net/api/manifest/v1/Generate/id/de/XQ+KS+CHEV1/video.m3u8",
                rtmpUrl = "",
                targetPath = "/tmp",
                targetFileName = "video.mp4",
                targetPathFileName = "/tmp/video.mp4",
                websiteUrl = "https://www.arte.tv/de/videos/123456-000-A/video/",
                selectedResolution = FilmResolution.Enum.HIGH_QUALITY,
            ),
        )

        assertEquals(
            "ffmpeg -i https://manifest-arte.akamaized.net/api/manifest/v1/Generate/id/de/XQ+KS+CHEV1/video.m3u8 " +
                "-map p:1 -map -0:s -c copy -bsf:a aac_adtstoasc /tmp/video.mp4",
            invocation.command,
        )
        assertEquals(
            "ffmpeg<>-i<>https://manifest-arte.akamaized.net/api/manifest/v1/Generate/id/de/XQ+KS+CHEV1/video.m3u8" +
                "<>-map<>p:1<>-map<>-0:s<>-c<>copy<>-bsf:a<>aac_adtstoasc<>/tmp/video.mp4",
            invocation.commandArray,
        )
    }

    @Test
    fun buildsTargetFromProgramSetAndFilmFields() {
        val film = DatenFilm().apply {
            sender = "Sender One"
            thema = "Topic One"
            title = "Title One"
            setSendeDatumFromString("01.06.2026")
            setSendeZeitFromString("20:15:00")
            urlNormalQuality = "https://example.invalid/video.mp4"
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
    fun buildsTargetFromFilmDateTimePlaceholders() {
        val film = DatenFilm().apply {
            setSendeDatumFromString("01.06.2026")
            setSendeZeitFromString("20:15:00")
            urlNormalQuality = "https://example.invalid/video.mp4"
        }
        val programSet = DatenPset("Set").apply {
            zielDateiname = "%D-%d-%1-%2-%3-%4-%5-%6.%S"
            zielPfad = "/downloads"
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

        assertEquals("20260601-201500-01-06-2026-20-15-00.mp4", target.fileName)
    }

    @Test
    fun buildsTargetFromTwoDigitYearPlaceholder() {
        val film = DatenFilm().apply {
            setSendeDatumFromString("01.06.2026")
            urlNormalQuality = "https://example.invalid/video.mp4"
        }
        val programSet = DatenPset("Set").apply {
            zielDateiname = "%3_2.%S"
            zielPfad = "/downloads"
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

        assertEquals("26.mp4", target.fileName)
    }

    @Test
    fun buildsTargetSuffixFromDownloadUrlWithoutQueryParameters() {
        val film = DatenFilm().apply {
            sender = "Sender One"
            thema = "Topic One"
            title = "Title One"
            urlNormalQuality =
                "https://ios-ondemand.swr.de/i/swr-fernsehen/bw-extra/20130202/601676.,m,s,l,.mp4.csmil/index_2_av.m3u8?e=b471643725c47acd"
        }
        val programSet = DatenPset("Set").apply {
            zielDateiname = "%S"
            zielPfad = "/downloads"
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

        assertEquals("m3u8", target.fileName)
    }

    @Test
    fun constructorSeedsNormalQualitySizeFromFilmList() {
        val film = DatenFilm().apply {
            sender = "Sender One"
            thema = "Topic One"
            title = "Title One"
            setSendeDatumFromString("01.06.2026")
            setSendeZeitFromString("20:15:00")
            urlNormalQuality = "https://example.invalid/video.mp4"
            setFileSize("123")
        }
        val programSet = createProgramSet()

        val download = DatenDownload(programSet, film, DownloadSource.ABO, null, "", "", "")

        assertEquals(123L * FileSize.ONE_MIB, download.runtime.filmSize.size)
    }

    @Test
    fun constructorSeedsNormalQualitySizeWhenDownloadUrlParametersAreRemoved() {
        val film = DatenFilm().apply {
            sender = "Sender One"
            thema = "Topic One"
            title = "Title One"
            setSendeDatumFromString("01.06.2026")
            setSendeZeitFromString("20:15:00")
            urlNormalQuality = "https://example.invalid/video.mp4?token=temporary"
            setFileSize("456")
        }
        val programSet = createProgramSet()

        val download = DatenDownload(programSet, film, DownloadSource.ABO, null, "", "", "")

        assertEquals("https://example.invalid/video.mp4", download.downloadUrl)
        assertEquals(456L * FileSize.ONE_MIB, download.runtime.filmSize.size)
    }

    @Test
    fun setGroesseFromFilmSeedsNormalQualitySizeWhenDownloadUrlHasParameters() {
        val film = DatenFilm().apply {
            urlNormalQuality = "https://example.invalid/video.mp4"
            setFileSize("789")
        }
        val download = DatenDownload().apply {
            this.film = film
            downloadUrl = "https://example.invalid/video.mp4?token=temporary"
        }

        download.setGroesseFromFilm()

        assertEquals(789L * FileSize.ONE_MIB, download.runtime.filmSize.size)
    }

    @Test
    fun copyPreservesWebsiteUrlForProgramInvocationRebuilds() {
        val film = DatenFilm().apply {
            sender = "Sender One"
            thema = "Topic One"
            title = "Title One"
            setSendeDatumFromString("01.06.2026")
            setSendeZeitFromString("20:15:00")
            urlNormalQuality = "https://example.invalid/video.mp4"
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
