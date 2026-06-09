package mediathek.gui.tabs.tab_film.helpers

import ca.odell.glazedlists.BasicEventList
import ca.odell.glazedlists.EventList
import mediathek.config.application.FilterConfiguration
import mediathek.daten.DatenFilm
import mediathek.gui.tabs.tab_film.filter.FilmFilterController
import mediathek.gui.tabs.tab_film.filter.FilmLengthSlider
import mediathek.gui.tabs.tab_film.search.SearchControlFieldMode
import mediathek.gui.tabs.tab_film.search.SearchFieldData
import mediathek.tool.FilterDTO
import org.apache.commons.configuration2.XMLConfiguration
import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Assertions.assertTrue
import org.junit.jupiter.api.Test
import java.util.*
import java.util.stream.Stream
import kotlin.time.Duration.Companion.minutes

internal class GuiModelHelperSupportTest {

    @Test
    fun createFilterExecutionContext_usesLiveControllerStateWhenFilterIsLocked() {
        val filterConfiguration = TestFilterConfiguration(XMLConfiguration())
        val filter = FilterDTO(UUID.randomUUID(), "Filter 1")
        filterConfiguration.addNewFilter(filter)
        filterConfiguration.setCurrentFilter(filter)

        val controller = FilmFilterController(
            filterConfiguration,
            dataProvider =
                object : FilmFilterController.DataProvider {
                    override fun senderList(): EventList<String> = BasicEventList()

                    override fun getThemen(senders: Collection<String>): List<String> = emptyList()
                },
            reloadRequester =
                object : FilmFilterController.ReloadRequester {
                    override fun requestTableReload() = Unit

                    override fun requestZeitraumReload() = Unit
                },
        )

        controller.setCurrentFilterChangesLocked(true)
        controller.onSenderSelectionChanged(setOf("ARD"))

        val support = GuiModelHelperSupport(
            SearchFieldData("", SearchControlFieldMode.THEMA_TITEL),
            controller,
        )

        val context = support.createFilterExecutionContext()

        assertEquals(setOf("ARD"), context.selectedSenders)
        assertTrue(filterConfiguration.checkedChannels.isEmpty())
    }

    @Test
    fun applyCommonFilters_matchesLegacyStagedFilterBehaviorForTopicAndLengthFilters() {
        val support = supportWithFilter(
            thema = "ARD",
            filmLengthMin = 10.0,
            filmLengthMax = 60.0,
        )
        val context = support.createFilterExecutionContext()
        val films = listOf(
            film(thema = "ARD", lengthInSeconds = 0),
            film(thema = "ARD", lengthInSeconds = 5.minutes.inWholeSeconds.toInt()),
            film(thema = "ARD", lengthInSeconds = 20.minutes.inWholeSeconds.toInt()),
            film(thema = "ARD", lengthInSeconds = 90.minutes.inWholeSeconds.toInt()),
            film(thema = "ZDF", lengthInSeconds = 20.minutes.inWholeSeconds.toInt()),
        )

        val filtered = support.applyCommonFilters(films.stream(), context).toList()
        val legacyFiltered = legacyApplyCommonFilters(films.stream(), context).toList()

        assertEquals(legacyFiltered, filtered)
        assertEquals(listOf(films[0], films[2]), filtered)
    }

    @Test
    fun applyCommonFilters_keepsUnknownLengthFilmsWhenOnlyMinimumLengthIsSet() {
        val support = supportWithFilter(
            filmLengthMin = 10.0,
            filmLengthMax = FilmLengthSlider.UNLIMITED_VALUE.toDouble(),
        )
        val context = support.createFilterExecutionContext()
        val films = listOf(
            film(lengthInSeconds = 0),
            film(lengthInSeconds = 5.minutes.inWholeSeconds.toInt()),
            film(lengthInSeconds = 20.minutes.inWholeSeconds.toInt()),
        )

        val filtered = support.applyCommonFilters(films.stream(), context).toList()
        val legacyFiltered = legacyApplyCommonFilters(films.stream(), context).toList()

        assertEquals(legacyFiltered, filtered)
        assertEquals(listOf(films[0], films[2]), filtered)
    }

    private fun supportWithFilter(
        thema: String = "",
        filmLengthMin: Double = 0.0,
        filmLengthMax: Double = FilmLengthSlider.UNLIMITED_VALUE.toDouble(),
    ): GuiModelHelperSupport {
        val filterConfiguration = TestFilterConfiguration(XMLConfiguration())
        val filter = FilterDTO(UUID.randomUUID(), "Filter")
        filterConfiguration.addNewFilter(filter)
        filterConfiguration.setCurrentFilter(filter)
        filterConfiguration.setThema(thema)
        filterConfiguration.setFilmLengthMin(filmLengthMin)
        filterConfiguration.setFilmLengthMax(filmLengthMax)

        return GuiModelHelperSupport(
            SearchFieldData("", SearchControlFieldMode.THEMA_TITEL),
            FilmFilterController(
                filterConfiguration,
                dataProvider =
                    object : FilmFilterController.DataProvider {
                        override fun senderList(): EventList<String> = BasicEventList()

                        override fun getThemen(senders: Collection<String>): List<String> = emptyList()
                    },
                reloadRequester =
                    object : FilmFilterController.ReloadRequester {
                        override fun requestTableReload() = Unit

                        override fun requestZeitraumReload() = Unit
                    },
            ),
        )
    }

    private fun legacyApplyCommonFilters(
        source: Stream<DatenFilm>,
        filterContext: GuiModelHelperSupport.FilterExecutionContext,
    ): Stream<DatenFilm> {
        var stream = source
        if (filterContext.filterThema.isNotEmpty()) {
            stream = stream.filter { film -> film.thema.equals(filterContext.filterThema, ignoreCase = true) }
        }
        if (filterContext.lengthFilterRange.hasUpperLimit()) {
            stream = stream.filter { film -> film.filmLength < filterContext.lengthFilterRange.maxLengthInSeconds }
        }
        return stream.filter { film ->
            val filmLength = film.filmLength
            filmLength == 0 || filmLength >= filterContext.lengthFilterRange.minLengthInSeconds
        }
    }

    private fun film(thema: String = "", lengthInSeconds: Int): DatenFilm =
        DatenFilm().apply {
            this.thema = thema
            setFilmLengthSeconds(lengthInSeconds)
        }

    private class TestFilterConfiguration(configuration: XMLConfiguration) : FilterConfiguration(configuration)
}
