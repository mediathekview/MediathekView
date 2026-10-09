package mediathek.gui.tabs.tab_film.table

import mediathek.config.application.FilterConfiguration
import mediathek.daten.DatenFilm
import mediathek.filmlisten.FilmCatalog
import mediathek.gui.tabs.tab_film.filter.FilmFilterController
import mediathek.gui.tabs.tab_film.filter_selection.FilmFilterSelectionController
import mediathek.gui.tabs.tab_film.filter_selection.FilterSelectionComboBoxModel
import mediathek.gui.tabs.tab_film.search.SearchControlFieldMode
import mediathek.gui.tabs.tab_film.search.SearchFieldData
import mediathek.tool.FilterDTO
import org.apache.commons.configuration2.XMLConfiguration
import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Assertions.fail
import org.junit.jupiter.api.Test
import java.awt.Component
import java.util.*
import javax.swing.JPanel
import javax.swing.JTable
import javax.swing.SwingUtilities

internal class FilmFilterSwitchPipelineTest {
    @Test
    fun switchingFromRestrictiveToPermissiveFilterPublishesExpandedRowsAndCount() {
        val restrictive = FilterDTO(UUID.randomUUID(), "Restrictive")
        val permissive = FilterDTO(UUID.randomUUID(), "Permissive")
        val configuration = TestFilterConfiguration(XMLConfiguration()).apply {
            addNewFilter(restrictive)
            addNewFilter(permissive)
            currentFilter = restrictive
            setShowNewOnly(true)
            currentFilter = permissive
            setShowNewOnly(false)
            currentFilter = restrictive
        }
        val controller = FilmFilterController(configuration)
        val catalog = FilmCatalog().apply {
            filteredFilms.addAll(List(6) { index ->
                DatenFilm().apply {
                    title = "Film $index"
                    urlNormalQuality = "https://example.invalid/$index.mp4"
                    isNew = index < 2
                }
            })
        }
        val binding = onEdt { FilmTableBinding(JTable()) }
        val host = PipelineHost(binding, catalog, controller)
        val reloader = FilmTableReloader(host)
        val selectionController = FilmFilterSelectionController(
            controller,
            object : FilmFilterController.ReloadRequester {
                override fun requestTableReload() = reloader.loadTable()
                override fun requestZeitraumReload() = reloader.loadTable()
            },
        )
        val comboModel = FilterSelectionComboBoxModel(
            controller::currentFilter,
            controller::availableFilters,
            controller::isFilterLocked,
            controller.selectionObserverRegistry(),
            selectionController::select,
        )

        try {
            reloader.loadTable()
            awaitRowCount(binding, 2)

            onEdt { comboModel.selectedItem = permissive }
            awaitRowCount(binding, 6)

            assertEquals(permissive, controller.currentFilter())
            assertEquals(6, host.publishedCounts.last())
        } finally {
            comboModel.close()
            reloader.dispose()
            onEdt { binding.dispose() }
        }
    }

    private class PipelineHost(
        private val binding: FilmTableModelBinding,
        private val catalog: FilmCatalog,
        private val controller: FilmFilterController,
    ) : FilmTableReloader.Host {
        val publishedCounts = mutableListOf<Int>()

        override fun tableBinding(): FilmTableModelBinding = binding
        override fun filmCatalog(): FilmCatalog = catalog
        override fun owner(): Component = JPanel()
        override fun searchFieldData(): SearchFieldData = SearchFieldData("", SearchControlFieldMode.THEMA_TITEL)
        override fun filterController(): FilmFilterController = controller
        override fun applyBlacklist() = Unit
        override fun setSelectionUpdatesSuspended(suspended: Boolean) = Unit
        override fun updateFilmData() {
            publishedCounts += binding.rowCount
        }
        override fun onReloadCompleted(fromSearchField: Boolean) = Unit
    }

    private class TestFilterConfiguration(configuration: XMLConfiguration) : FilterConfiguration(configuration)

    private fun awaitRowCount(binding: FilmTableModelBinding, expected: Int) {
        val deadline = System.nanoTime() + 5_000_000_000L
        while (System.nanoTime() < deadline) {
            if (onEdt { binding.rowCount } == expected) {
                return
            }
            Thread.sleep(10)
        }
        fail<Unit>("Timed out waiting for $expected rows; current=${onEdt { binding.rowCount }}")
    }

    private fun <T> onEdt(action: () -> T): T {
        if (SwingUtilities.isEventDispatchThread()) {
            return action()
        }
        var result: Result<T>? = null
        SwingUtilities.invokeAndWait { result = runCatching(action) }
        return checkNotNull(result).getOrThrow()
    }
}
