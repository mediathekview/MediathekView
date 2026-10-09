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

import mediathek.controller.starter.DownloadServices
import mediathek.daten.DatenFilm
import mediathek.daten.DatenPset
import mediathek.daten.ProgramSetRepository
import mediathek.daten.abo.AboServices
import mediathek.daten.blacklist.BlacklistServices
import mediathek.filmlisten.FilmCatalog
import mediathek.gui.tabs.tab_film.actions.FilmActionHost
import mediathek.gui.tabs.tab_film.actions.FilmUiActions
import mediathek.gui.tabs.tab_film.context.TableContextMenuHandler
import mediathek.gui.tabs.tab_film.filter.FilmFilterController
import mediathek.gui.tabs.tab_film.search.SearchFieldData
import mediathek.tool.table.MVFilmTable
import java.awt.Component
import java.util.*
import java.util.function.BiConsumer
import javax.swing.JFrame
import javax.swing.JScrollPane

class FilmTableReloadHostAdapter(
    private val filmCatalog: FilmCatalog,
    private val owner: Component,
    private val tableProvider: () -> MVFilmTable,
    private val searchFieldDataProvider: () -> SearchFieldData,
    private val filterController: FilmFilterController,
    private val setSelectionUpdatesSuspendedAction: (Boolean) -> Unit,
    private val updateStartInfoPropertyAction: () -> Unit,
    private val updateFilmDataAction: () -> Unit,
) : FilmTableReloader.Host {
    override fun table(): MVFilmTable = tableProvider()

    override fun filmCatalog(): FilmCatalog = filmCatalog

    override fun owner(): Component = owner

    override fun searchFieldData(): SearchFieldData = searchFieldDataProvider()

    override fun filterController(): FilmFilterController = filterController

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
    private val downloads: DownloadServices,
    private val programSets: ProgramSetRepository,
    private val filmCatalog: FilmCatalog,
    private val abos: AboServices,
    private val blacklist: BlacklistServices,
    private val programSetExporter: BiConsumer<Array<DatenPset>, String>,
    private val tableProvider: () -> MVFilmTable,
    private val currentlySelectedFilmProvider: () -> Optional<DatenFilm>,
    private val filmAtRowProvider: (Int) -> Optional<DatenFilm>,
    private val playSelectedFilmAction: () -> Unit,
    private val saveSelectedFilmAction: () -> Unit,
    private val startFilmWithPsetAction: (DatenPset) -> Unit,
    private val setSelectionUpdatesSuspendedAction: (Boolean) -> Unit,
    private val showFilmInfoAction: () -> Unit,
    private val ownerFrame: JFrame,
    private val actionsProvider: () -> FilmUiActions,
) : TableContextMenuHandler.Host {
    override fun table(): MVFilmTable = tableProvider()

    override fun downloads(): DownloadServices = downloads

    override fun programSets(): ProgramSetRepository = programSets

    override fun filmCatalog(): FilmCatalog = filmCatalog

    override fun abos(): AboServices = abos

    override fun blacklist(): BlacklistServices = blacklist

    override fun programSetExporter(): BiConsumer<Array<DatenPset>, String> = programSetExporter

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

    override fun showFilmInfo() {
        showFilmInfoAction()
    }

    override fun ownerFrame(): JFrame = ownerFrame

    override fun actions(): FilmUiActions = actionsProvider()
}

class FilmTableInstallerHostAdapter(
    private val downloads: DownloadServices,
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

    override fun downloads(): DownloadServices = downloads

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
