/*
 * Copyright (c) 2026 derreisende77.
 * This code was developed as part of the MediathekView project https://github.com/mediathekview/MediathekView
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package mediathek.gui.tabs.tab_film.filter_selection

import mediathek.gui.tabs.tab_film.filter.FilmFilterController
import mediathek.tool.FilterDTO
import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Assertions.assertSame
import org.junit.jupiter.api.Test
import java.util.*
import java.util.concurrent.atomic.AtomicReference
import java.util.function.Consumer
import javax.swing.SwingUtilities

internal class FilterSelectionComboBoxModelTest {

    @Test
    fun `setSelectedItem updates the local combo box selection`() {
        val first = FilterDTO(UUID.randomUUID(), "Filter 1")
        val second = FilterDTO(UUID.randomUUID(), "Filter 2")
        val selected = AtomicReference(first)
        val model = FilterSelectionComboBoxModel(
            selectedFilterSupplier = selected::get,
            availableFiltersSupplier = { listOf(first, second) },
            filterLockedReader = { false },
            selectionObserverRegistry = NoOpSelectionObserverRegistry
        )

        model.setSelectedItem(second)

        assertEquals(second, model.selectedItem)
        model.close()
    }

    @Test
    fun `current filter observer updates selected item`() {
        val first = FilterDTO(UUID.randomUUID(), "Filter 1")
        val second = FilterDTO(UUID.randomUUID(), "Filter 2")
        val selected = AtomicReference(first)
        val observers = RecordingSelectionObserverRegistry()
        val model = FilterSelectionComboBoxModel(
            selectedFilterSupplier = selected::get,
            availableFiltersSupplier = { listOf(first, second) },
            filterLockedReader = { false },
            selectionObserverRegistry = observers
        )

        selected.set(second)
        observers.currentFilterObserver!!.accept(second)
        SwingUtilities.invokeAndWait {}

        assertSame(second, model.selectedItem)
        model.close()
    }

    @Test
    fun `initial selection comes from the current filter supplier`() {
        val filter = FilterDTO(UUID.randomUUID(), "Filter 1")
        val selected = AtomicReference(filter)
        val model = FilterSelectionComboBoxModel(
            selectedFilterSupplier = selected::get,
            availableFiltersSupplier = { listOf(filter) },
            filterLockedReader = { false },
            selectionObserverRegistry = NoOpSelectionObserverRegistry
        )

        assertSame(filter, model.selectedItem)
        model.close()
    }

    private data object NoOpSelectionObserverRegistry : FilmFilterController.SelectionObserverRegistry {
        override fun addAvailableFiltersObserver(observer: Runnable) = Unit
        override fun removeAvailableFiltersObserver(observer: Runnable) = Unit
        override fun addCurrentFilterObserver(observer: java.util.function.Consumer<FilterDTO>) = Unit
        override fun removeCurrentFilterObserver(observer: java.util.function.Consumer<FilterDTO>) = Unit
    }

    private class RecordingSelectionObserverRegistry : FilmFilterController.SelectionObserverRegistry {
        var currentFilterObserver: Consumer<FilterDTO>? = null

        override fun addAvailableFiltersObserver(observer: Runnable) = Unit
        override fun removeAvailableFiltersObserver(observer: Runnable) = Unit
        override fun addCurrentFilterObserver(observer: Consumer<FilterDTO>) {
            currentFilterObserver = observer
        }

        override fun removeCurrentFilterObserver(observer: Consumer<FilterDTO>) {
            if (currentFilterObserver == observer) {
                currentFilterObserver = null
            }
        }
    }
}
