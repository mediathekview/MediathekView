package mediathek.gui.tabs.tab_film.table

import mediathek.config.application.ApplicationConfiguration
import mediathek.daten.DatenFilm
import mediathek.filmlisten.FilmCatalog
import mediathek.gui.tabs.tab_film.filter.FilmFilterController
import mediathek.gui.tabs.tab_film.helpers.FilmQueryEngine
import mediathek.gui.tabs.tab_film.search.SearchControlFieldMode
import mediathek.gui.tabs.tab_film.search.SearchFieldData
import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Assertions.assertTrue
import org.junit.jupiter.api.Test
import java.awt.Component
import java.util.concurrent.CountDownLatch
import java.util.concurrent.TimeUnit
import java.util.concurrent.atomic.AtomicInteger
import javax.swing.JPanel
import javax.swing.JTable
import javax.swing.SwingUtilities

internal class FilmTableReloaderTest {
    @Test
    fun onlyNewestOverlappingQueryIsAppliedAndTableStaysEnabled() {
        val binding = RecordingBinding()
        val completed = CountDownLatch(1)
        val firstStarted = CountDownLatch(1)
        val queryNumber = AtomicInteger()
        val firstFilm = film("first")
        val secondFilm = film("second")
        val host = TestHost(binding, completed = completed::countDown)
        val reloader = FilmTableReloader(host) {
            when (queryNumber.incrementAndGet()) {
                1 -> FilmQueryEngine {
                    firstStarted.countDown()
                    Thread.sleep(150)
                    listOf(firstFilm)
                }

                else -> FilmQueryEngine { listOf(secondFilm) }
            }
        }

        try {
            reloader.loadTable()
            assertTrue(firstStarted.await(5, TimeUnit.SECONDS))
            reloader.loadTable()
            assertTrue(completed.await(5, TimeUnit.SECONDS))

            assertEquals(listOf(listOf(secondFilm)), binding.replacements)
            assertTrue(binding.table.isEnabled)
        } finally {
            reloader.dispose()
        }
    }

    @Test
    fun debouncedReloadRetainsPendingBlacklistRebuild() {
        val binding = RecordingBinding()
        val completed = CountDownLatch(1)
        val blacklistApplications = AtomicInteger()
        val queryExecutions = AtomicInteger()
        val host = TestHost(binding, completed::countDown, blacklistApplications::incrementAndGet)
        val reloader = FilmTableReloader(host) {
            FilmQueryEngine {
                queryExecutions.incrementAndGet()
                emptyList()
            }
        }

        try {
            val swingThreadBlocked = CountDownLatch(1)
            val releaseSwingThread = CountDownLatch(1)
            SwingUtilities.invokeLater {
                swingThreadBlocked.countDown()
                releaseSwingThread.await(5, TimeUnit.SECONDS)
            }

            assertTrue(swingThreadBlocked.await(5, TimeUnit.SECONDS))
            try {
                reloader.requestZeitraumReload()
                reloader.requestTableReload()
            } finally {
                releaseSwingThread.countDown()
            }

            assertTrue(completed.await(5, TimeUnit.SECONDS))
            assertEquals(1, blacklistApplications.get())
            assertEquals(1, queryExecutions.get())
        } finally {
            reloader.dispose()
        }
    }

    private class TestHost(
        private val binding: RecordingBinding,
        private val completed: () -> Unit,
        private val applyBlacklistAction: () -> Unit = {},
    ) : FilmTableReloader.Host {
        private val catalog = FilmCatalog()
        private val filterController = FilmFilterController(
            ApplicationConfiguration.getInstance().createFilterConfiguration(),
        )

        override fun tableBinding(): FilmTableModelBinding = binding
        override fun filmCatalog(): FilmCatalog = catalog
        override fun owner(): Component = JPanel()
        override fun searchFieldData(): SearchFieldData = SearchFieldData("", SearchControlFieldMode.THEMA_TITEL)
        override fun filterController(): FilmFilterController = filterController
        override fun applyBlacklist() = applyBlacklistAction()
        override fun setSelectionUpdatesSuspended(suspended: Boolean) = Unit
        override fun updateFilmData() = Unit
        override fun onReloadCompleted(fromSearchField: Boolean) = completed()
    }

    private class RecordingBinding : FilmTableModelBinding {
        override val table = JTable()
        val replacements = mutableListOf<List<DatenFilm>>()
        override val rowCount: Int get() = replacements.lastOrNull()?.size ?: 0
        override fun filmAtViewRow(viewRow: Int): DatenFilm? = replacements.lastOrNull()?.getOrNull(viewRow)
        override fun selectedFilms(): List<DatenFilm> = emptyList()
        override suspend fun replaceFilms(films: Collection<DatenFilm>) {
            replacements += films.toList()
        }

        override fun removeFilms(films: Collection<DatenFilm>): Boolean = false
        override fun repaintVisibleRows() = Unit
        override fun dispose() = Unit
    }

    private fun film(title: String) = DatenFilm().apply {
        this.title = title
        urlNormalQuality = "https://example.invalid/$title.mp4"
    }
}
