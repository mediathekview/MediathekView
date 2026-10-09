package mediathek.gui.tabs.tab_film.table

import kotlinx.coroutines.runBlocking
import mediathek.config.application.ApplicationConfiguration
import mediathek.daten.DatenFilm
import mediathek.tool.datum.DatumFilm
import mediathek.tool.models.FilmColumn
import org.junit.jupiter.api.Assertions.*
import org.junit.jupiter.api.Test
import java.util.concurrent.CountDownLatch
import java.util.concurrent.TimeUnit
import java.util.concurrent.atomic.AtomicBoolean
import java.util.concurrent.atomic.AtomicLong
import javax.swing.JTable
import javax.swing.SwingUtilities
import javax.swing.Timer
import kotlin.time.Duration.Companion.seconds
import kotlin.time.measureTime

internal class FilmTableBindingTest {
    @Test
    fun directTableAccessRequiresTheEventDispatchThread() {
        val fixture = fixture()
        try {
            val failure = assertThrows(IllegalStateException::class.java) { fixture.binding.rowCount }
            assertEquals("Film table access must run on the Swing EDT", failure.message)
        } finally {
            onEdt { fixture.binding.dispose() }
        }
    }

    @Test
    fun disposalDetachesTheGlazedModelBeforeSwingCanQueryItAgain() {
        val fixture = fixture()
        val glazedModel = fixture.table.model

        onEdt { fixture.binding.dispose() }

        assertNotSame(glazedModel, fixture.table.model)
        assertEquals(0, onEdt { fixture.table.rowCount })
    }

    @Test
    fun tableModelIsInstalledOnlyOnceAcrossReplacementsAndRemovals() = runBlocking {
        val fixture = fixture()
        try {
            val originalModel = fixture.table.model
            val films = listOf(film("A", 1), film("B", 2), film("C", 3))

            fixture.binding.replaceFilms(films)
            fixture.binding.replaceFilms(films.reversed())
            onEdt { fixture.binding.removeFilms(listOf(films[1])) }
            awaitTitles(fixture, listOf("C", "A"))

            assertSame(originalModel, fixture.table.model)
            assertEquals(2, onEdt { fixture.binding.rowCount })
            assertEquals(
                listOf("C", "A"),
                onEdt { (0 until fixture.table.rowCount).map { fixture.binding.filmAtViewRow(it)?.title } },
            )
        } finally {
            onEdt { fixture.binding.dispose() }
        }
    }

    @Test
    fun glazedComparatorSortingIsPreparedOffTheEdtAndMirroredIntoTheStableModel() = runBlocking {
        val fixture = fixture()
        try {
            val originalModel = fixture.table.model
            fixture.binding.replaceFilms(listOf(film("C", 1), film("A", 2), film("B", 3)))

            onEdt { fixture.binding.sorting.restoreLegacySort(FilmColumn.TITLE.index, descending = false) }
            awaitTitles(fixture, listOf("A", "B", "C"))

            onEdt { fixture.binding.sorting.clear() }
            awaitTitles(fixture, listOf("C", "A", "B"))
            assertSame(originalModel, fixture.table.model)
        } finally {
            onEdt { fixture.binding.dispose() }
        }
    }

    @Test
    fun largeReplacementDoesNotStarveTheEventDispatchThread() = runBlocking {
        val fixture = fixture()
        val lastHeartbeat = AtomicLong(System.nanoTime())
        val longestPause = AtomicLong()
        val replacementCompleted = AtomicBoolean()
        val heartbeatAfterReplacement = CountDownLatch(1)
        val timer = Timer(20) {
            val now = System.nanoTime()
            longestPause.accumulateAndGet(now - lastHeartbeat.getAndSet(now), ::maxOf)
            if (replacementCompleted.get()) {
                heartbeatAfterReplacement.countDown()
            }
        }
        try {
            onEdt { timer.start() }
            val repeatedFilm = film("Large", 99)

            fixture.binding.replaceFilms(List(500_000) { repeatedFilm })
            replacementCompleted.set(true)
            assertTrue(heartbeatAfterReplacement.await(2, TimeUnit.SECONDS), "EDT heartbeat did not resume")
            onEdt { timer.stop() }

            assertTrue(
                longestPause.get() < 500_000_000L,
                "EDT was unresponsive for ${longestPause.get() / 1_000_000} ms",
            )
        } finally {
            onEdt {
                timer.stop()
                fixture.binding.dispose()
            }
        }
    }

    @Test
    fun restrictiveToPermissiveExpansionCommitsPromptlyWithSelection() = runBlocking {
        val fixture = fixture()
        try {
            val filler = film("Filler", 1)
            val selected = film("Selected", 2)
            val restrictive = MutableList(300_000) { filler }.apply { this[150_000] = selected }
            val permissive = MutableList(600_000) { filler }.apply { this[450_000] = selected }
            fixture.binding.replaceFilms(restrictive)
            onEdt { fixture.table.setRowSelectionInterval(150_000, 150_000) }

            val elapsed = measureTime { fixture.binding.replaceFilms(permissive) }

            assertTrue(elapsed < 2.seconds, "filter expansion took $elapsed")
            assertEquals(600_000, onEdt { fixture.binding.rowCount })
            assertSame(selected, onEdt { fixture.binding.selectedFilms().single() })
        } finally {
            onEdt { fixture.binding.dispose() }
        }
    }

    @Test
    fun replacementRestoresSelectionByFilmIdentityAndFallsBackToAnchor() = runBlocking {
        val fixture = fixture()
        try {
            val films = listOf(film("A", 1), film("B", 2), film("C", 3))
            fixture.binding.replaceFilms(films)
            onEdt { fixture.table.setRowSelectionInterval(1, 1) }

            fixture.binding.replaceFilms(listOf(films[2], films[1]))

            assertSame(films[1], onEdt { fixture.binding.selectedFilms().single() })

            fixture.binding.replaceFilms(listOf(films[0]))

            assertSame(films[0], onEdt { fixture.binding.selectedFilms().single() })
        } finally {
            onEdt { fixture.binding.dispose() }
        }
    }

    @Test
    fun filterReplacementDoesNotMaterializeIdentityForEveryResultRow() = runBlocking {
        val fixture = fixture()
        try {
            val films = listOf(film("A", 1), film("B", 2), film("C", 3))
            fixture.binding.replaceFilms(films)
            onEdt { fixture.table.setRowSelectionInterval(1, 1) }

            fixture.binding.replaceFilms(films.reversed())

            assertSame(films[1], onEdt { fixture.binding.selectedFilms().single() })
            assertNull(filmIdentityCache(films[0]))
            assertNull(filmIdentityCache(films[2]))
        } finally {
            onEdt { fixture.binding.dispose() }
        }
    }

    @Test
    fun catalogReloadRestoresSelectionToEquivalentReplacementObject() = runBlocking {
        val fixture = fixture()
        try {
            val selected = film("Selected", 7).apply {
                sender = "ARD"
                thema = "News"
                websiteUrl = "https://example.invalid/program"
            }
            fixture.binding.replaceFilms(listOf(selected))
            onEdt { fixture.table.setRowSelectionInterval(0, 0) }
            val replacement = film("Selected replacement", 7).apply {
                sender = selected.sender
                thema = selected.thema
                websiteUrl = selected.websiteUrl
            }

            fixture.binding.replaceFilms(listOf(replacement))

            assertSame(replacement, onEdt { fixture.binding.selectedFilms().single() })
        } finally {
            onEdt { fixture.binding.dispose() }
        }
    }

    @Test
    fun filmTableFormatExposesDomainValuesWithoutHiddenReferenceColumn() {
        val film = film("Titel", 7).apply {
            sender = "ARD"
            setFilmLengthSeconds(42)
            setFileSize("17")
        }
        val format = FilmTableFormat()

        assertEquals(15, format.getColumnCount())
        assertEquals("Sender", format.getColumnName(FilmColumn.SENDER.index))
        assertEquals(DatumFilm::class.java, format.getColumnClass(FilmColumn.DATE.index))
        assertEquals("ARD", format.getColumnValue(film, FilmColumn.SENDER.index))
        assertEquals(42, format.getColumnValue(film, FilmColumn.DURATION.index))
        assertEquals(17, format.getColumnValue(film, FilmColumn.SIZE.index))
    }

    @Test
    fun legacyColumnConfigurationMigratesTrailingPlaceholdersToStableColumnState() {
        val config = ApplicationConfiguration.getInstance()
        val originalLegacy = config.filmTableColumnConfiguration
        val originalColumns = config.getTableColumnSettings("film")
        val originalSort = config.getGlazedTableSortKeys("film")
        config.setTableColumnSettings("film", "")
        config.setGlazedTableSortKeys("film", "")
        config.filmTableColumnConfiguration =
            "0,85,352,521,23,23,20,84,53,66,77,0,0,0,1389,0,0|0,1,2,3,4,5,6,13,7,8,9,10,11,12,14,0,0|7|DESCENDING"

        val table = JTable()
        val binding = onEdt { FilmTableBinding(table) }
        val appearance = FilmTableAppearance(lineBreak = false, showSenderIcons = true, useSmallSenderIcons = true)
        val settings = onEdt { FilmTableSettingsController(table, binding.sorting, appearance) }
        try {
            assertEquals(FilmColumn.GEO.index, onEdt { table.convertColumnIndexToModel(7) })
            assertTrue(config.getTableColumnSettings("film").isNotBlank())
            assertTrue(config.getGlazedTableSortKeys("film").isNotBlank())
        } finally {
            onEdt {
                settings.dispose()
                binding.dispose()
            }
            config.filmTableColumnConfiguration = originalLegacy
            config.setTableColumnSettings("film", originalColumns)
            config.setGlazedTableSortKeys("film", originalSort)
        }
    }

    private fun fixture(): Fixture = onEdt {
        val table = JTable()
        Fixture(table, FilmTableBinding(table))
    }

    private fun film(title: String, number: Int): DatenFilm = DatenFilm().apply {
        this.title = title
        urlNormalQuality = "https://example.invalid/$number.mp4"
    }

    private fun filmIdentityCache(film: DatenFilm): Any? =
        DatenFilm::class.java.getDeclaredField("filmIdentityCache").let { field ->
            field.isAccessible = true
            field.get(film)
        }

    private data class Fixture(val table: JTable, val binding: FilmTableBinding)

    private fun awaitTitles(fixture: Fixture, expected: List<String>) {
        val deadline = System.nanoTime() + 5_000_000_000L
        while (System.nanoTime() < deadline) {
            val actual = onEdt {
                (0 until fixture.table.rowCount).mapNotNull { fixture.binding.filmAtViewRow(it)?.title }
            }
            if (actual == expected) {
                return
            }
            Thread.sleep(10)
        }
        fail<Unit>("Timed out waiting for film titles $expected")
    }

    private fun <T> onEdt(block: () -> T): T {
        if (SwingUtilities.isEventDispatchThread()) {
            return block()
        }
        var result: Result<T>? = null
        SwingUtilities.invokeAndWait { result = runCatching(block) }
        return checkNotNull(result).getOrThrow()
    }
}
