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

package mediathek.gui.tabs.tab_film.table

import kotlinx.coroutines.CoroutineScope
import kotlinx.coroutines.Dispatchers
import kotlinx.coroutines.Job
import kotlinx.coroutines.SupervisorJob
import kotlinx.coroutines.asCoroutineDispatcher
import kotlinx.coroutines.cancel
import kotlinx.coroutines.launch
import kotlinx.coroutines.swing.Swing
import kotlinx.coroutines.withContext
import mediathek.gui.messages.TableModelChangeEvent
import mediathek.gui.tabs.tab_film.filter.FilmFilterController
import mediathek.gui.tabs.tab_film.helpers.GuiModelHelperFactory
import mediathek.gui.tabs.tab_film.search.SearchFieldData
import mediathek.tool.MessageBus
import mediathek.tool.table.MVFilmTable
import org.apache.logging.log4j.LogManager
import java.util.concurrent.Executor
import javax.swing.table.TableModel

class FilmTableReloader(private val host: Host) {
    interface Host {
        fun table(): MVFilmTable

        fun searchFieldData(): SearchFieldData

        fun filterController(): FilmFilterController

        fun tableModelExecutor(): Executor

        fun setSelectionUpdatesSuspended(suspended: Boolean)

        fun updateStartInfoProperty()

        fun updateFilmData()
    }

    private val uiScope = CoroutineScope(SupervisorJob() + Dispatchers.Swing)
    private var modelJob: Job? = null
    private var pendingTableReload = false
    private var pendingTableReloadFromSearchField = false

    fun loadTable() {
        loadTable(false)
    }

    fun dispose() {
        uiScope.cancel()
        modelJob = null
        pendingTableReload = false
        pendingTableReloadFromSearchField = false
    }

    fun loadTable(fromSearchField: Boolean) {
        if (modelJob?.isActive == true) {
            pendingTableReload = true
            pendingTableReloadFromSearchField = pendingTableReloadFromSearchField or fromSearchField
            return
        }

        val messageBus = MessageBus.messageBus
        messageBus.publish(TableModelChangeEvent(true, fromSearchField))

        host.setSelectionUpdatesSuspended(true)
        host.table().getSpalten()
        host.table().isEnabled = false

        val decoratedPool = host.tableModelExecutor()
        modelJob = uiScope.launch {
            val result = runCatching {
                withContext(decoratedPool.asCoroutineDispatcher()) {
                    val helper = GuiModelHelperFactory.createGuiModelHelper(host.searchFieldData(), host.filterController())
                    helper.filteredTableModel
                }
            }

            result.fold(
                onSuccess = { model -> applyFilteredModel(model, fromSearchField) },
                onFailure = { thrown ->
                    logger.error("Model filtering failed!", thrown)
                    restoreTableAfterFiltering(fromSearchField, scrollToSelection = false)
                },
            )
        }
    }

    private fun applyFilteredModel(
        model: TableModel,
        fromSearchField: Boolean,
    ) {
        host.table().model = model
        restoreTableAfterFiltering(fromSearchField, scrollToSelection = true)
    }

    private fun restoreTableAfterFiltering(
        fromSearchField: Boolean,
        scrollToSelection: Boolean,
    ) {
        host.table().isEnabled = true
        host.updateStartInfoProperty()
        host.table().setSpalten()
        host.updateFilmData()
        host.setSelectionUpdatesSuspended(false)
        if (scrollToSelection) {
            host.table().scrollToSelection()
        }
        MessageBus.messageBus.publish(TableModelChangeEvent(false, fromSearchField))
        triggerPendingTableReloadIfNecessary()
    }

    private fun triggerPendingTableReloadIfNecessary() {
        if (!pendingTableReload) {
            return
        }

        val reloadFromSearchField = pendingTableReloadFromSearchField
        pendingTableReload = false
        pendingTableReloadFromSearchField = false
        loadTable(reloadFromSearchField)
    }

    private companion object {
        private val logger = LogManager.getLogger()
    }
}
