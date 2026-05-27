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

import mediathek.daten.DatenFilm
import mediathek.daten.DatenPset
import mediathek.gui.tabs.tab_film.actions.FilmActionHost
import mediathek.gui.tabs.tab_film.actions.FilmUiActions
import mediathek.gui.tabs.tab_film.context.TableContextMenuHandler
import mediathek.gui.tabs.tab_film.filter.FilmFilterController
import mediathek.gui.tabs.tab_film.search.SearchFieldData
import mediathek.mainwindow.MediathekGui
import mediathek.tool.table.MVFilmTable
import java.awt.Component
import java.util.Optional
import java.util.concurrent.Executor
import javax.swing.JScrollPane

class FilmTableReloadHostAdapter(
    private val tableProvider: () -> MVFilmTable,
    private val searchFieldDataProvider: () -> SearchFieldData,
    private val filterController: FilmFilterController,
    private val tableModelExecutorProvider: () -> Executor,
    private val setSelectionUpdatesSuspendedAction: (Boolean) -> Unit,
    private val updateStartInfoPropertyAction: () -> Unit,
    private val updateFilmDataAction: () -> Unit,
) : FilmTableReloader.Host {
    override fun table(): MVFilmTable = tableProvider()

    override fun searchFieldData(): SearchFieldData = searchFieldDataProvider()

    override fun filterController(): FilmFilterController = filterController

    override fun tableModelExecutor(): Executor = tableModelExecutorProvider()

    override fun setSelectionUpdatesSuspended(suspended: Boolean) {
        setSelectionUpdatesSuspendedAction(suspended)
    }

    override fun updateStartInfoProperty() {
        updateStartInfoPropertyAction()
    }

    override fun updateFilmData() {
        updateFilmDataAction()
    }
}

class TableContextMenuHostAdapter(
    private val tableProvider: () -> MVFilmTable,
    private val currentlySelectedFilmProvider: () -> Optional<DatenFilm>,
    private val filmAtRowProvider: (Int) -> Optional<DatenFilm>,
    private val playSelectedFilmAction: () -> Unit,
    private val saveSelectedFilmAction: () -> Unit,
    private val startFilmWithPsetAction: (DatenPset) -> Unit,
    private val setSelectionUpdatesSuspendedAction: (Boolean) -> Unit,
    private val gui: MediathekGui,
    private val actionsProvider: () -> FilmUiActions,
) : TableContextMenuHandler.Host {
    override fun table(): MVFilmTable = tableProvider()

    override fun getCurrentlySelectedFilm(): Optional<DatenFilm> = currentlySelectedFilmProvider()

    override fun getFilm(row: Int): Optional<DatenFilm> = filmAtRowProvider(row)

    override fun playSelectedFilm() {
        playSelectedFilmAction()
    }

    override fun saveSelectedFilm() {
        saveSelectedFilmAction()
    }

    override fun startFilmWithPset(pSet: DatenPset) {
        startFilmWithPsetAction(pSet)
    }

    override fun setSelectionUpdatesSuspended(suspended: Boolean) {
        setSelectionUpdatesSuspendedAction(suspended)
    }

    override fun gui(): MediathekGui = gui

    override fun actions(): FilmUiActions = actionsProvider()
}

class FilmTableInstallerHostAdapter(
    private val tableProvider: () -> MVFilmTable,
    private val filmListScrollPane: JScrollPane,
    private val ownerComponent: Component,
    private val tableContextMenuHostProvider: () -> TableContextMenuHandler.Host,
    private val filmActionHost: FilmActionHost,
    private val actionsProvider: () -> FilmUiActions,
    private val updateSelectedListItemsCountAction: () -> Unit,
    private val onComponentShownAction: () -> Unit,
    private val updateFilmDataAction: () -> Unit,
    private val selectionUpdatesSuspendedProvider: () -> Boolean,
) : FilmTableInstaller.Host {
    override fun table(): MVFilmTable = tableProvider()

    override fun filmListScrollPane(): JScrollPane = filmListScrollPane

    override fun ownerComponent(): Component = ownerComponent

    override fun tableContextMenuHost(): TableContextMenuHandler.Host = tableContextMenuHostProvider()

    override fun filmActionHost(): FilmActionHost = filmActionHost

    override fun actions(): FilmUiActions = actionsProvider()

    override fun updateSelectedListItemsCount() {
        updateSelectedListItemsCountAction()
    }

    override fun onComponentShown() {
        onComponentShownAction()
    }

    override fun updateFilmData() {
        updateFilmDataAction()
    }

    override fun selectionUpdatesSuspended(): Boolean = selectionUpdatesSuspendedProvider()
}
