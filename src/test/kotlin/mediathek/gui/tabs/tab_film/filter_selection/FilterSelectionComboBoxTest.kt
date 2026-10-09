package mediathek.gui.tabs.tab_film.filter_selection

import mediathek.gui.tabs.tab_film.filter.FilmFilterController
import mediathek.tool.FilterDTO
import org.junit.jupiter.api.Assertions.*
import org.junit.jupiter.api.Test
import java.awt.Font
import java.util.*
import java.util.concurrent.atomic.AtomicReference
import javax.swing.JLabel
import javax.swing.JList
import javax.swing.JPanel

internal class FilterSelectionComboBoxTest {

    @Test
    fun `renderer shows lock icon for locked filters`() {
        val lockedFilter = FilterDTO(UUID.randomUUID(), "Locked")
        val selected = AtomicReference(lockedFilter)
        val model = FilterSelectionComboBoxModel(
            selectedFilterSupplier = selected::get,
            availableFiltersSupplier = { listOf(lockedFilter) },
            filterLockedReader = { it == lockedFilter },
            selectionObserverRegistry = NoOpSelectionObserverRegistry
        )
        val comboBox = FilterSelectionComboBox(model)
        val list = JList(model)
        list.font = Font(Font.DIALOG, Font.PLAIN, 19)

        try {
            val component = comboBox.renderer.getListCellRendererComponent(list, lockedFilter, 0, false, false) as JPanel
            val iconLabel = component.getComponent(1) as JLabel

            assertNotNull(iconLabel.icon)
            assertEquals(list.font.size, iconLabel.icon.iconHeight)
        } finally {
            model.close()
        }
    }

    @Test
    fun `renderer hides lock icon for unlocked filters`() {
        val filter = FilterDTO(UUID.randomUUID(), "Unlocked")
        val selected = AtomicReference(filter)
        val model = FilterSelectionComboBoxModel(
            selectedFilterSupplier = selected::get,
            availableFiltersSupplier = { listOf(filter) },
            filterLockedReader = { false },
            selectionObserverRegistry = NoOpSelectionObserverRegistry
        )
        val comboBox = FilterSelectionComboBox(model)
        val list = JList(model)

        try {
            val component = comboBox.renderer.getListCellRendererComponent(list, filter, 0, false, false) as JPanel
            val iconLabel = component.getComponent(1) as JLabel

            assertNull(iconLabel.icon)
        } finally {
            model.close()
        }
    }

    private data object NoOpSelectionObserverRegistry : FilmFilterController.SelectionObserverRegistry {
        override fun addAvailableFiltersObserver(observer: Runnable) = Unit
        override fun removeAvailableFiltersObserver(observer: Runnable) = Unit
        override fun addCurrentFilterObserver(observer: java.util.function.Consumer<FilterDTO>) = Unit
        override fun removeCurrentFilterObserver(observer: java.util.function.Consumer<FilterDTO>) = Unit
    }
}
