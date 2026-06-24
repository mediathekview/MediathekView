package mediathek.filmlisten

import mediathek.controller.SenderFilmlistLoadApprover
import mediathek.daten.DatenFilm
import mediathek.daten.ListeFilme
import mediathek.filmeSuchen.ListenerFilmeLaden
import com.sun.net.httpserver.HttpServer
import org.junit.jupiter.api.AfterEach
import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Assertions.assertFalse
import org.junit.jupiter.api.Assertions.assertTrue
import org.junit.jupiter.api.BeforeEach
import org.junit.jupiter.api.Test
import org.junit.jupiter.api.io.TempDir
import org.junit.jupiter.api.parallel.ResourceLock
import java.net.InetAddress
import java.net.InetSocketAddress
import java.nio.file.Files
import java.nio.file.Path
import java.util.concurrent.atomic.AtomicInteger

@ResourceLock("SenderFilmlistLoadApprover")
class FilmListImportServiceTest {
    @TempDir
    lateinit var tempDir: Path

    private lateinit var approvedSenders: Set<String>

    @BeforeEach
    fun rememberApprovedSenders() {
        approvedSenders = SenderFilmlistLoadApprover.senderSet.toSet()
        approveOnly("APPROVED")
    }

    @AfterEach
    fun restoreApprovedSenders() {
        SenderFilmlistLoadApprover.senderSet.clear()
        SenderFilmlistLoadApprover.senderSet.addAll(approvedSenders)
    }

    @Test
    fun `importFromFile captures old urls before replacing list`() {
        val oldFilm = film("APPROVED", "Old", "Old title")
        val films = ListeFilme().apply { add(oldFilm) }
        val service = service()

        val outcome = service.importFromFile(
            writeFilmList("replacement.json", filmEntry("APPROVED", "New", "New title")).toString(),
            films,
            0,
        ) {
            films.mapTo(HashSet()) { film -> film.urlNormalQuality }
        }

        assertEquals(FilmListImportResult.SUCCESS, outcome.result)
        assertEquals(setOf(oldFilm.urlNormalQuality), outcome.oldFilmUrls)
        assertTrue(outcome.importedDiffList.isEmpty())
        assertEquals(1, films.size)
        assertEquals("New title", films[0].title)
    }

    @Test
    fun `importAdditionalFromFile reads into separate imported list`() {
        val currentFilm = film("APPROVED", "Current", "Current title")
        val currentFilms = ListeFilme().apply { add(currentFilm) }
        val oldFilmUrls = setOf(currentFilm.urlNormalQuality)
        val service = service()

        val outcome = service.importAdditionalFromFile(
            writeFilmList("additional.json", filmEntry("APPROVED", "Additional", "Additional title")).toString(),
            0,
            oldFilmUrls,
        )

        assertEquals(FilmListImportResult.SUCCESS, outcome.result)
        assertEquals(oldFilmUrls, outcome.oldFilmUrls)
        assertEquals(1, currentFilms.size)
        assertEquals("Current title", currentFilms[0].title)
        assertEquals(1, outcome.importedDiffList.size)
        assertEquals("Additional title", outcome.importedDiffList[0].title)
    }

    @Test
    fun `importFromFile reports failure for unreadable source after preparing import`() {
        val oldFilm = film("APPROVED", "Old", "Old title")
        val films = ListeFilme().apply { add(oldFilm) }
        var prepareCalled = false
        val service = service()

        val outcome = service.importFromFile(
            tempDir.resolve("missing.json").toString(),
            films,
            0,
        ) {
            prepareCalled = true
            films.mapTo(HashSet()) { film -> film.urlNormalQuality }
        }

        assertEquals(FilmListImportResult.FAILURE, outcome.result)
        assertTrue(prepareCalled)
        assertEquals(setOf(oldFilm.urlNormalQuality), outcome.oldFilmUrls)
        assertTrue(films.isEmpty())
    }

    @Test
    fun `importFromUrl returns no update for not modified response without preparing import`() {
        val films = ListeFilme().apply { add(film("APPROVED", "Current", "Current title")) }
        val feedback = RecordingFeedback()
        val service = service(feedback)
        var prepareCalled = false

        withNotModifiedServer { url ->
            val outcome = service.importFromUrl(url, films, 0, immerNeuLaden = false) {
                prepareCalled = true
                emptySet()
            }

            assertEquals(FilmListImportResult.NO_UPDATE, outcome.result)
            assertTrue(outcome.oldFilmUrls.isEmpty())
            assertTrue(outcome.importedDiffList.isEmpty())
            assertFalse(prepareCalled)
            assertEquals(1, feedback.noUpdateCount.get())
            assertEquals(1, films.size)
            assertEquals("Current title", films[0].title)
        }
    }

    private fun service(feedback: FilmListImportFeedback = RecordingFeedback()): FilmListImportService =
        FilmListImportService(
            feedback = feedback,
            progressListener = ListenerFilmeLaden(),
        )

    private fun withNotModifiedServer(block: (String) -> Unit) {
        val server = HttpServer.create(InetSocketAddress(InetAddress.getLoopbackAddress(), 0), 0)
        server.createContext("/filmlist") { exchange ->
            exchange.sendResponseHeaders(304, -1)
            exchange.close()
        }
        server.start()
        try {
            val address = server.address
            block("http://${address.hostString}:${address.port}/filmlist")
        } finally {
            server.stop(0)
        }
    }

    private class RecordingFeedback : FilmListImportFeedback {
        val noUpdateCount = AtomicInteger(0)

        override fun showNoUpdateAvailable(showDialogs: Boolean) {
            noUpdateCount.incrementAndGet()
        }

        override fun showExceptionMessage(message: String, ex: Exception, showDialogs: Boolean) = Unit
    }

    private fun approveOnly(vararg senders: String) {
        SenderFilmlistLoadApprover.senderSet.clear()
        SenderFilmlistLoadApprover.senderSet.addAll(senders)
    }

    private fun writeFilmList(fileName: String, vararg entries: String): Path {
        val file = tempDir.resolve(fileName)
        Files.writeString(
            file,
            buildString {
                append("{")
                append("\"Filmliste\":[\"\",\"15.05.2026, 12:00\",\"\",\"\",\"test-list\"],")
                append("\"Filmliste\":[\"\"],")
                append(entries.joinToString(","))
                append("}")
            },
        )
        return file
    }

    private fun filmEntry(sender: String, thema: String, title: String): String =
        listOf(
            sender,
            thema,
            title,
            "15.05.2026",
            "12:00",
            "00:30:00",
            "",
            "Beschreibung",
            "https://example.test/$sender/$title.mp4",
            "https://example.test/$sender/$title",
            "",
            "",
            "",
            "",
            "",
            "",
            "",
            "",
            "",
            "false",
        ).joinToString(prefix = "\"X\":[", postfix = "]") { "\"${it.escapeJson()}\"" }

    private fun film(sender: String, thema: String, title: String): DatenFilm =
        DatenFilm().apply {
            this.sender = sender
            this.thema = thema
            this.title = title
            setSendeDatumFromFilmlistValue("15.05.2026")
            setSendeZeitFromFilmlistValue("12:00")
            description = "Beschreibung"
            urlNormalQuality = "https://example.test/$sender/$title.mp4"
            websiteUrl = "https://example.test/$sender/$title"
        }

    private fun String.escapeJson(): String =
        replace("\\", "\\\\")
            .replace("\"", "\\\"")
}
