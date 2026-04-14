package mediathek.gui.tabs.tab_film.helpers

import ca.odell.glazedlists.BasicEventList
import ca.odell.glazedlists.EventList
import mediathek.gui.tabs.tab_film.SearchControlFieldMode
import mediathek.gui.tabs.tab_film.SearchFieldData
import mediathek.gui.tabs.tab_film.filter.FilmFilterController
import mediathek.tool.FilterConfiguration
import mediathek.tool.FilterDTO
import org.apache.commons.configuration2.XMLConfiguration
import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Assertions.assertTrue
import org.junit.jupiter.api.Test
import java.util.*

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

        assertEquals(setOf("ARD"), context.selectedSenders())
        assertTrue(filterConfiguration.checkedChannels.isEmpty())
    }

    private class TestFilterConfiguration(configuration: XMLConfiguration) : FilterConfiguration(configuration)
}
