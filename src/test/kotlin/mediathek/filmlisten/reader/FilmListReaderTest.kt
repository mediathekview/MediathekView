package mediathek.filmlisten.reader

import mediathek.controller.SenderFilmlistLoadApprover
import mediathek.daten.ListeFilme
import mediathek.filmlisten.FilmListLoadListener
import mediathek.filmlisten.FilmListLoadProgress
import org.junit.jupiter.api.AfterEach
import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.BeforeEach
import org.junit.jupiter.api.Test
import org.junit.jupiter.api.io.TempDir
import org.junit.jupiter.api.parallel.ResourceLock
import java.nio.file.Files
import java.nio.file.Path

@ResourceLock("SenderFilmlistLoadApprover")
class FilmListReaderTest {
    @TempDir
    lateinit var tempDir: Path

    private lateinit var approvedSenders: Set<String>

    @BeforeEach
    fun rememberApprovedSenders() {
        approvedSenders = SenderFilmlistLoadApprover.senderSet.toSet()
    }

    @AfterEach
    fun restoreApprovedSenders() {
        SenderFilmlistLoadApprover.senderSet.clear()
        SenderFilmlistLoadApprover.senderSet.addAll(approvedSenders)
    }

    @Test
    fun `filtered senders still update compressed theme state`() {
        approveOnly("APPROVED")
        val filmListFile = writeFilmList(
            "filtered-theme-state.json",
            filmEntry("BLOCKED", "Shared Thema", "Blocked title"),
            filmEntry("APPROVED", "", "Approved title"),
        )

        val films = ListeFilme()

        FilmListReader().readFilmListe(filmListFile.toString(), films, 0)

        assertEquals(1, films.size)
        assertEquals("APPROVED", films[0].sender)
        assertEquals("Shared Thema", films[0].thema)
    }

    @Test
    fun `compressed field state is reset for each read`() {
        approveOnly("APPROVED")
        val reader = FilmListReader()
        val firstFilms = ListeFilme()
        val secondFilms = ListeFilme()

        reader.readFilmListe(
            writeFilmList("first-read.json", filmEntry("APPROVED", "Previous Thema", "First title")).toString(),
            firstFilms,
            0,
        )
        reader.readFilmListe(
            writeFilmList("second-read.json", filmEntry("APPROVED", "", "Second title")).toString(),
            secondFilms,
            0,
        )

        assertEquals(1, secondFilms.size)
        assertEquals("", secondFilms[0].thema)
    }

    @Test
    fun `title and theme normalize typographic double quotes`() {
        approveOnly("APPROVED")
        val filmListFile = writeFilmList(
            "normalized-quotes.json",
            filmEntry("APPROVED", "„Thema“", "„Solingen, wie geht's dir?”"),
        )

        val films = ListeFilme()

        FilmListReader().readFilmListe(filmListFile.toString(), films, 0)

        assertEquals(1, films.size)
        assertEquals("\"Thema\"", films[0].thema)
        assertEquals("\"Solingen, wie geht's dir?\"", films[0].title)
    }

    @Test
    fun `progress listeners receive start and finish events until reader is closed`() {
        approveOnly("APPROVED")
        val filmListFile = writeFilmList(
            "progress-events.json",
            filmEntry("APPROVED", "Thema", "Title"),
        )
        val reader = FilmListReader()
        val firstListener = RecordingProgressListener()
        val secondListener = RecordingProgressListener()
        reader.addProgressListener(firstListener)
        reader.addProgressListener(secondListener)

        reader.readFilmListe(filmListFile.toString(), ListeFilme(), 0)
        reader.close()
        reader.readFilmListe(filmListFile.toString(), ListeFilme(), 0)

        assertEquals(listOf(filmListFile.toString()), firstListener.startedSources)
        assertEquals(listOf(filmListFile.toString()), firstListener.finishedSources)
        assertEquals(listOf(filmListFile.toString()), secondListener.startedSources)
        assertEquals(listOf(filmListFile.toString()), secondListener.finishedSources)
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

    private fun String.escapeJson(): String =
        replace("\\", "\\\\")
            .replace("\"", "\\\"")

    private class RecordingProgressListener : FilmListLoadListener {
        val startedSources = mutableListOf<String>()
        val finishedSources = mutableListOf<String>()

        override fun loadStarted(progress: FilmListLoadProgress) {
            startedSources += progress.senderUrl
        }

        override fun loadFinished(progress: FilmListLoadProgress) {
            finishedSources += progress.senderUrl
        }
    }
}
