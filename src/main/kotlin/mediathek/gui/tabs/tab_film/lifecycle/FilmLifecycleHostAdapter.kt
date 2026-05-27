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

package mediathek.gui.tabs.tab_film.lifecycle

import mediathek.config.Daten
import mediathek.gui.tabs.tab_film.FilmToolBar
import mediathek.gui.tabs.tab_film.actions.FilmUiActions
import mediathek.gui.tabs.tab_film.filter.SwingFilterDialog
import mediathek.gui.tabs.tab_film.search.SearchField
import mediathek.tool.FilterConfiguration
import mediathek.tool.table.MVFilmTable

class FilmLifecycleHostAdapter(
    private val messageBusSubscriber: Any,
    private val daten: Daten,
    private val tableProvider: () -> MVFilmTable,
    private val filterConfiguration: FilterConfiguration,
    private val bookmarkStartupReloadCoordinator: BookmarkStartupReloadCoordinator,
    private val swingFilterDialogProvider: () -> SwingFilterDialog,
    private val filmToolBarProvider: () -> FilmToolBar,
    private val searchFieldProvider: () -> SearchField,
    private val actionsProvider: () -> FilmUiActions,
    private val requestTableReloadAction: () -> Unit,
    private val updateStartInfoPropertyAction: () -> Unit,
    private val saveTableConfigurationAction: () -> Unit,
    private val closeFilterSelectionModelAction: () -> Unit,
) : FilmLifecycleController.Host {
    override fun messageBusSubscriber(): Any = messageBusSubscriber

    override fun daten(): Daten = daten

    override fun table(): MVFilmTable = tableProvider()

    override fun filterConfiguration(): FilterConfiguration = filterConfiguration

    override fun bookmarkStartupReloadCoordinator(): BookmarkStartupReloadCoordinator = bookmarkStartupReloadCoordinator

    override fun swingFilterDialog(): SwingFilterDialog = swingFilterDialogProvider()

    override fun filmToolBar(): FilmToolBar = filmToolBarProvider()

    override fun searchField(): SearchField = searchFieldProvider()

    override fun actions(): FilmUiActions = actionsProvider()

    override fun requestTableReload() {
        requestTableReloadAction()
    }

    override fun updateStartInfoProperty() {
        updateStartInfoPropertyAction()
    }

    override fun saveTableConfiguration() {
        saveTableConfigurationAction()
    }

    override fun closeFilterSelectionModel() {
        closeFilterSelectionModelAction()
    }
}
