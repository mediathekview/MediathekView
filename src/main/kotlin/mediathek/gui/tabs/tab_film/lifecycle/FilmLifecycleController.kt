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

import kotlinx.coroutines.*
import kotlinx.coroutines.swing.Swing
import mediathek.config.application.FilterConfiguration
import mediathek.filmeSuchen.ListenerFilmeLaden
import mediathek.filmeSuchen.ListenerFilmeLadenEvent
import mediathek.filmlisten.FilmeLaden
import mediathek.gui.messages.*
import mediathek.gui.messages.history.SeenHistoryChangedEvent
import mediathek.gui.tabs.tab_film.FilmToolBar
import mediathek.gui.tabs.tab_film.actions.FilmUiActions
import mediathek.gui.tabs.tab_film.filter.SwingFilterDialog
import mediathek.gui.tabs.tab_film.search.SearchField
import mediathek.tool.MessageBus
import mediathek.tool.table.MVFilmTable

class FilmLifecycleController(private val host: Host) {
    interface Host {
        fun messageBusSubscriber(): Any
        fun filmListLoader(): FilmeLaden
        fun table(): MVFilmTable
        fun filterConfiguration(): FilterConfiguration
        fun bookmarkStartupReloadCoordinator(): BookmarkStartupReloadCoordinator
        fun swingFilterDialog(): SwingFilterDialog?
        fun filmToolBar(): FilmToolBar
        fun searchField(): SearchField
        fun actions(): FilmUiActions
        fun requestTableReload()
        fun updateStartInfoProperty()
        fun saveTableConfiguration()
        fun closeFilterSelectionModel()
    }

    private val uiScope = CoroutineScope(SupervisorJob() + Dispatchers.Swing)
    private val filmListReloadListener = createFilmListReloadListener()

    fun start() {
        MessageBus.messageBus.subscribe(host.messageBusSubscriber())
        host.filmListLoader().addFilmLoadListener(filmListReloadListener)
        launchOnSwing { host.requestTableReload() }
    }

    fun disposePanel() {
        host.saveTableConfiguration()
        host.swingFilterDialog()?.dispose()
        host.closeFilterSelectionModel()
        host.filmListLoader().removeFilmLoadListener(filmListReloadListener)
        uiScope.cancel()
    }

    fun handleTableModelChange(event: TableModelChangeEvent) {
        if (event.active) {
            launchOnSwing { setFilmControlsEnabled(false) }
        } else {
            launchOnSwing {
                setFilmControlsEnabled(true)
                if (event.fromSearchField) {
                    host.searchField().requestFocusInWindow()
                }
            }
        }

        host.swingFilterDialog()?.onTableModelChangeEvent(event)
    }

    fun handleSeenHistoryChangedEvent(@Suppress("UNUSED_PARAMETER") event: SeenHistoryChangedEvent) {
        launchOnSwing {
            if (host.filterConfiguration().isShowUnseenOnly) {
                host.requestTableReload()
            } else {
                host.table().fireTableDataChanged(true)
            }
        }
    }

    fun handleButtonStart(@Suppress("UNUSED_PARAMETER") event: ButtonStartEvent) {
        launchOnSwing {
            host.table().fireTableDataChanged(true)
            host.updateStartInfoProperty()
        }
    }

    fun handleStartEvent(@Suppress("UNUSED_PARAMETER") event: StartEvent) {
        launchOnSwing { host.updateStartInfoProperty() }
    }

    fun handleReloadTableDataEvent(@Suppress("UNUSED_PARAMETER") event: ReloadTableDataEvent) {
        host.requestTableReload()
    }

    fun handleBookmarkRefreshCompletedEvent(@Suppress("UNUSED_PARAMETER") event: BookmarkRefreshCompletedEvent) {
        if (host.bookmarkStartupReloadCoordinator()
                .onBookmarkRefreshCompleted(host.filterConfiguration().isShowBookMarkedOnly)
        ) {
            host.requestTableReload()
        }
    }

    private fun setFilmControlsEnabled(enabled: Boolean) {
        val actions = host.actions()
        actions.playFilm.isEnabled = enabled
        actions.saveFilm.isEnabled = enabled
        actions.bookmarkAddFilm.isEnabled = enabled
        actions.bookmarkRemoveFilm.isEnabled = enabled
        actions.deleteBookmarks.isEnabled = enabled
        actions.manageBookmarks.isEnabled = enabled
        host.filmToolBar().isEnabled = enabled
    }

    private fun launchOnSwing(block: () -> Unit) {
        uiScope.launch { block() }
    }

    private fun createFilmListReloadListener(): ListenerFilmeLaden =
        object : ListenerFilmeLaden() {
            override fun start(@Suppress("UNUSED_PARAMETER") event: ListenerFilmeLadenEvent) {
                launchOnSwing { host.swingFilterDialog()?.onFilmDataLoadingStarted() }
                host.bookmarkStartupReloadCoordinator().onFilmListLoadingStarted()
            }

            override fun fertig(@Suppress("UNUSED_PARAMETER") event: ListenerFilmeLadenEvent) {
                launchOnSwing {
                    host.swingFilterDialog()?.onFilmDataLoaded()
                    if (host.bookmarkStartupReloadCoordinator()
                            .onFilmListLoaded(host.filterConfiguration().isShowBookMarkedOnly)
                    ) {
                        host.requestTableReload()
                    }
                }
            }
        }
}
