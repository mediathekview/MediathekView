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
import mediathek.filmlisten.FilmListLoadCoordinator
import mediathek.filmlisten.FilmListLoadListener
import mediathek.filmlisten.FilmListLoadProgress
import mediathek.gui.messages.BookmarkRefreshCompletedEvent
import mediathek.gui.messages.ButtonStartEvent
import mediathek.gui.messages.ReloadTableDataEvent
import mediathek.gui.messages.history.FilmSeenStateChangedEvent
import mediathek.gui.messages.history.SeenHistoryChangedEvent
import mediathek.gui.tabs.tab_film.filter.SwingFilterDialog
import mediathek.gui.tabs.tab_film.table.FilmTableModelBinding
import mediathek.tool.MessageBus

class FilmLifecycleController(private val host: Host) {
    interface Host {
        fun messageBusSubscriber(): Any
        fun filmListLoader(): FilmListLoadCoordinator
        fun tableBinding(): FilmTableModelBinding
        fun filterConfiguration(): FilterConfiguration
        fun bookmarkStartupReloadCoordinator(): BookmarkStartupReloadCoordinator
        fun swingFilterDialog(): SwingFilterDialog?
        fun requestTableReload()
        fun invalidateTableReloads()
        fun saveTableConfiguration()
        fun closeFilterSelectionModel()
    }

    private val uiScope = CoroutineScope(SupervisorJob() + Dispatchers.Swing)
    private val filmListReloadListener = createFilmListReloadListener()

    fun start() {
        MessageBus.messageBus.subscribe(host.messageBusSubscriber())
        host.filmListLoader().addLoadListener(filmListReloadListener)
        launchOnSwing { host.requestTableReload() }
    }

    fun disposePanel() {
        host.saveTableConfiguration()
        host.swingFilterDialog()?.dispose()
        host.closeFilterSelectionModel()
        host.filmListLoader().removeLoadListener(filmListReloadListener)
        uiScope.cancel()
    }

    fun handleSeenHistoryChangedEvent(@Suppress("UNUSED_PARAMETER") event: SeenHistoryChangedEvent) {
        launchOnSwing {
            host.requestTableReload()
        }
    }

    fun handleFilmSeenStateChangedEvent(event: FilmSeenStateChangedEvent) {
        launchOnSwing {
            when {
                !host.filterConfiguration().isShowUnseenOnly -> {
                    host.tableBinding().repaintVisibleRows()
                }
                event.seen && host.tableBinding().removeFilms(event.films) -> Unit
                else -> host.requestTableReload()
            }
        }
    }

    fun handleButtonStart(@Suppress("UNUSED_PARAMETER") event: ButtonStartEvent) {
        launchOnSwing {
            host.tableBinding().table.repaint()
        }
    }

    fun handleReloadTableDataEvent(@Suppress("UNUSED_PARAMETER") event: ReloadTableDataEvent) {
        launchOnSwing { host.requestTableReload() }
    }

    fun handleBookmarkRefreshCompletedEvent(@Suppress("UNUSED_PARAMETER") event: BookmarkRefreshCompletedEvent) {
        if (host.bookmarkStartupReloadCoordinator()
                .onBookmarkRefreshCompleted(host.filterConfiguration().isShowBookMarkedOnly)
        ) {
            launchOnSwing { host.requestTableReload() }
        }
    }

    private fun launchOnSwing(block: () -> Unit) {
        uiScope.launch { block() }
    }

    private fun createFilmListReloadListener(): FilmListLoadListener =
        object : FilmListLoadListener {
            override fun loadStarted(@Suppress("UNUSED_PARAMETER") progress: FilmListLoadProgress) {
                launchOnSwing {
                    host.invalidateTableReloads()
                    host.swingFilterDialog()?.onFilmDataLoadingStarted()
                }
                host.bookmarkStartupReloadCoordinator().onFilmListLoadingStarted()
            }

            override fun loadFinished(@Suppress("UNUSED_PARAMETER") progress: FilmListLoadProgress) {
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
