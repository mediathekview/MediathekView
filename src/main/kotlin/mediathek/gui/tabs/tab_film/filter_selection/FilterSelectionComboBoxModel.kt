/*
 * Copyright (c) 2025-2026 derreisende77.
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

import kotlinx.coroutines.*
import kotlinx.coroutines.swing.Swing
import mediathek.gui.tabs.tab_film.filter.FilmFilterController
import mediathek.tool.FilterDTO
import java.util.function.Consumer
import java.util.function.Predicate
import java.util.function.Supplier
import javax.swing.DefaultComboBoxModel
import javax.swing.SwingUtilities

class FilterSelectionComboBoxModel(
    private val selectedFilterSupplier: Supplier<FilterDTO>,
    private val availableFiltersSupplier: Supplier<List<FilterDTO>>,
    private val filterLockedReader: Predicate<FilterDTO>,
    private val selectionObserverRegistry: FilmFilterController.SelectionObserverRegistry,
) : DefaultComboBoxModel<FilterDTO>(), AutoCloseable {
    private val uiScope = CoroutineScope(SupervisorJob() + Dispatchers.Swing)
    private val availableFilters = ArrayList<FilterDTO>()
    private val availableFiltersObserver = Runnable(::onAvailableFiltersChanged)
    private val currentFilterObserver = Consumer<FilterDTO>(::onCurrentFilterChanged)

    init {
        refreshAvailableFilters()
        super.setSelectedItem(selectedFilterSupplier.get())

        selectionObserverRegistry.addAvailableFiltersObserver(availableFiltersObserver)
        selectionObserverRegistry.addCurrentFilterObserver(currentFilterObserver)
    }

    override fun setSelectedItem(anObject: Any?) {
        super.setSelectedItem(anObject)
    }

    override fun getSelectedItem(): FilterDTO? = super.getSelectedItem() as? FilterDTO

    override fun getSize(): Int = availableFilters.size

    override fun getElementAt(index: Int): FilterDTO = availableFilters[index]

    override fun close() {
        selectionObserverRegistry.removeAvailableFiltersObserver(availableFiltersObserver)
        selectionObserverRegistry.removeCurrentFilterObserver(currentFilterObserver)
        uiScope.cancel()
    }

    fun isFilterLocked(filter: FilterDTO?): Boolean = filter != null && filterLockedReader.test(filter)

    private fun refreshAvailableFilters() {
        availableFilters.clear()
        availableFilters.addAll(availableFiltersSupplier.get())
    }

    private fun fireModelChanged() {
        fireContentsChanged(this, 0, availableFilters.size)
    }

    private fun onAvailableFiltersChanged() {
        dispatchOnEdt {
            refreshAvailableFilters()
            super.setSelectedItem(selectedFilterSupplier.get())
            fireModelChanged()
        }
    }

    private fun onCurrentFilterChanged(filterDTO: FilterDTO) {
        dispatchOnEdt {
            if (super.getSelectedItem() !== filterDTO) {
                super.setSelectedItem(filterDTO)
            }
            fireModelChanged()
        }
    }

    private fun dispatchOnEdt(action: () -> Unit) {
        if (SwingUtilities.isEventDispatchThread()) {
            action()
            return
        }

        uiScope.launch {
            action()
        }
    }
}
