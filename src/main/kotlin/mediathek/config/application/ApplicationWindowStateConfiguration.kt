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

package mediathek.config.application

import org.apache.commons.configuration2.XMLConfiguration
import org.apache.commons.configuration2.sync.LockMode
import kotlin.math.max

class ApplicationWindowStateConfiguration(
    private val config: XMLConfiguration,
) {
    var bandwidthMonitorVisible: Boolean
        get() = config.getBoolean(APPLICATION_UI_BANDWIDTH_MONITOR_VISIBLE, false)
        set(newValue) {
            config.setProperty(APPLICATION_UI_BANDWIDTH_MONITOR_VISIBLE, newValue)
        }

    val bandwidthMonitorDialogState: ApplicationConfiguration.BandwidthMonitorDialogState
        get() =
            config.withLock(LockMode.READ) {
                try {
                    ApplicationConfiguration.BandwidthMonitorDialogState(
                        getInt(APPLICATION_UI_BANDWIDTH_MONITOR_X),
                        getInt(APPLICATION_UI_BANDWIDTH_MONITOR_Y),
                        getInt(APPLICATION_UI_BANDWIDTH_MONITOR_WIDTH),
                        getInt(APPLICATION_UI_BANDWIDTH_MONITOR_HEIGHT),
                    )
                } catch (_: NoSuchElementException) {
                    ApplicationConfiguration.BandwidthMonitorDialogState.empty()
                }
            }

    fun setBandwidthMonitorDialogBounds(x: Int, y: Int, width: Int, height: Int) {
        config.setBounds(
            xKey = APPLICATION_UI_BANDWIDTH_MONITOR_X,
            yKey = APPLICATION_UI_BANDWIDTH_MONITOR_Y,
            widthKey = APPLICATION_UI_BANDWIDTH_MONITOR_WIDTH,
            heightKey = APPLICATION_UI_BANDWIDTH_MONITOR_HEIGHT,
            x = x,
            y = y,
            width = width,
            height = height,
        )
    }

    val memoryMonitorDialogState: ApplicationConfiguration.MemoryMonitorDialogState
        get() =
            config.withLock(LockMode.READ) {
                ApplicationConfiguration.MemoryMonitorDialogState(
                    getBoolean(APPLICATION_UI_MEMORY_MONITOR_VISIBLE, false),
                    getInt(APPLICATION_UI_MEMORY_MONITOR_X, Int.MIN_VALUE),
                    getInt(APPLICATION_UI_MEMORY_MONITOR_Y, Int.MIN_VALUE),
                    getInt(APPLICATION_UI_MEMORY_MONITOR_WIDTH, -1),
                    getInt(APPLICATION_UI_MEMORY_MONITOR_HEIGHT, -1),
                )
            }

    var memoryMonitorDialogVisible: Boolean
        get() = config.getBoolean(APPLICATION_UI_MEMORY_MONITOR_VISIBLE, false)
        set(newValue) {
            config.setProperty(APPLICATION_UI_MEMORY_MONITOR_VISIBLE, newValue)
        }

    fun setMemoryMonitorDialogBounds(x: Int, y: Int, width: Int, height: Int) {
        config.setBounds(
            xKey = APPLICATION_UI_MEMORY_MONITOR_X,
            yKey = APPLICATION_UI_MEMORY_MONITOR_Y,
            widthKey = APPLICATION_UI_MEMORY_MONITOR_WIDTH,
            heightKey = APPLICATION_UI_MEMORY_MONITOR_HEIGHT,
            x = x,
            y = y,
            width = width,
            height = height,
        )
    }

    val filmInfoDialogState: ApplicationConfiguration.FilmInfoDialogState
        get() =
            config.withLock(LockMode.READ) {
                ApplicationConfiguration.FilmInfoDialogState(
                    getBoolean(APPLICATION_UI_FILM_INFO_VISIBLE, false),
                    getInt(APPLICATION_UI_FILM_INFO_X, Int.MIN_VALUE),
                    getInt(APPLICATION_UI_FILM_INFO_Y, Int.MIN_VALUE),
                    getInt(APPLICATION_UI_FILM_INFO_WIDTH, -1),
                    getInt(APPLICATION_UI_FILM_INFO_HEIGHT, -1),
                )
            }

    var filmInfoDialogVisible: Boolean
        get() = config.getBoolean(APPLICATION_UI_FILM_INFO_VISIBLE, false)
        set(newValue) {
            config.setProperty(APPLICATION_UI_FILM_INFO_VISIBLE, newValue)
        }

    fun setFilmInfoDialogBounds(x: Int, y: Int, width: Int, height: Int) {
        config.setBounds(
            xKey = APPLICATION_UI_FILM_INFO_X,
            yKey = APPLICATION_UI_FILM_INFO_Y,
            widthKey = APPLICATION_UI_FILM_INFO_WIDTH,
            heightKey = APPLICATION_UI_FILM_INFO_HEIGHT,
            x = x,
            y = y,
            width = width,
            height = height,
        )
    }

    val loadFilmListDialogState: ApplicationConfiguration.LoadFilmListDialogState
        get() =
            config.withLock(LockMode.READ) {
                try {
                    ApplicationConfiguration.LoadFilmListDialogState(
                        getInt(APPLICATION_UI_LOAD_FILM_LIST_DIALOG_X, Int.MIN_VALUE),
                        getInt(APPLICATION_UI_LOAD_FILM_LIST_DIALOG_Y, Int.MIN_VALUE),
                        getInt(APPLICATION_UI_LOAD_FILM_LIST_DIALOG_WIDTH, -1),
                        getInt(APPLICATION_UI_LOAD_FILM_LIST_DIALOG_HEIGHT, -1),
                    )
                } catch (_: NoSuchElementException) {
                    ApplicationConfiguration.LoadFilmListDialogState.empty()
                }
            }

    fun setLoadFilmListDialogBounds(x: Int, y: Int, width: Int, height: Int) {
        config.setBounds(
            xKey = APPLICATION_UI_LOAD_FILM_LIST_DIALOG_X,
            yKey = APPLICATION_UI_LOAD_FILM_LIST_DIALOG_Y,
            widthKey = APPLICATION_UI_LOAD_FILM_LIST_DIALOG_WIDTH,
            heightKey = APPLICATION_UI_LOAD_FILM_LIST_DIALOG_HEIGHT,
            x = x,
            y = y,
            width = width,
            height = height,
        )
    }

    val settingsDialogState: ApplicationConfiguration.SettingsDialogState
        get() =
            config.withLock(LockMode.READ) {
                try {
                    ApplicationConfiguration.SettingsDialogState(
                        getInt(APPLICATION_UI_SETTINGS_DIALOG_X, Int.MIN_VALUE),
                        getInt(APPLICATION_UI_SETTINGS_DIALOG_Y, Int.MIN_VALUE),
                        getInt(APPLICATION_UI_SETTINGS_DIALOG_WIDTH, -1),
                        getInt(APPLICATION_UI_SETTINGS_DIALOG_HEIGHT, -1),
                    )
                } catch (_: Exception) {
                    ApplicationConfiguration.SettingsDialogState.empty()
                }
            }

    fun setSettingsDialogBounds(x: Int, y: Int, width: Int, height: Int) {
        config.setBounds(
            xKey = APPLICATION_UI_SETTINGS_DIALOG_X,
            yKey = APPLICATION_UI_SETTINGS_DIALOG_Y,
            widthKey = APPLICATION_UI_SETTINGS_DIALOG_WIDTH,
            heightKey = APPLICATION_UI_SETTINGS_DIALOG_HEIGHT,
            x = x,
            y = y,
            width = width,
            height = height,
        )
    }

    var filterDialogVisible: Boolean
        get() = config.getBoolean(APPLICATION_UI_FILTER_DIALOG_VISIBLE, false)
        set(visible) {
            config.setProperty(APPLICATION_UI_FILTER_DIALOG_VISIBLE, visible)
        }

    val filterDialogState: ApplicationConfiguration.FilterDialogState
        get() =
            config.withLock(LockMode.READ) {
                try {
                    ApplicationConfiguration.FilterDialogState(
                        getInt(APPLICATION_UI_FILTER_DIALOG_X),
                        getInt(APPLICATION_UI_FILTER_DIALOG_Y),
                        getInt(APPLICATION_UI_FILTER_DIALOG_WIDTH),
                        getInt(APPLICATION_UI_FILTER_DIALOG_HEIGHT),
                    )
                } catch (_: NoSuchElementException) {
                    ApplicationConfiguration.FilterDialogState.empty()
                }
            }

    fun setFilterDialogBounds(x: Int, y: Int, width: Int, height: Int) {
        config.setBounds(
            xKey = APPLICATION_UI_FILTER_DIALOG_X,
            yKey = APPLICATION_UI_FILTER_DIALOG_Y,
            widthKey = APPLICATION_UI_FILTER_DIALOG_WIDTH,
            heightKey = APPLICATION_UI_FILTER_DIALOG_HEIGHT,
            x = x,
            y = y,
            width = width,
            height = height,
        )
    }

    val editDownloadDialogState: ApplicationConfiguration.EditDownloadDialogState
        get() =
            config.withLock(LockMode.READ) {
                try {
                    ApplicationConfiguration.EditDownloadDialogState(
                        getInt(APPLICATION_UI_EDIT_DOWNLOAD_DIALOG_X),
                        getInt(APPLICATION_UI_EDIT_DOWNLOAD_DIALOG_Y),
                        getInt(APPLICATION_UI_EDIT_DOWNLOAD_DIALOG_WIDTH, -1),
                        getInt(APPLICATION_UI_EDIT_DOWNLOAD_DIALOG_HEIGHT, -1),
                    )
                } catch (_: NoSuchElementException) {
                    ApplicationConfiguration.EditDownloadDialogState.empty()
                }
            }

    fun setEditDownloadDialogBounds(x: Int, y: Int, width: Int, height: Int) {
        config.setBounds(
            xKey = APPLICATION_UI_EDIT_DOWNLOAD_DIALOG_X,
            yKey = APPLICATION_UI_EDIT_DOWNLOAD_DIALOG_Y,
            widthKey = APPLICATION_UI_EDIT_DOWNLOAD_DIALOG_WIDTH,
            heightKey = APPLICATION_UI_EDIT_DOWNLOAD_DIALOG_HEIGHT,
            x = x,
            y = y,
            width = width,
            height = height,
        )
    }

    val addDownloadDialogPosition: ApplicationConfiguration.AddDownloadDialogPosition
        get() =
            config.withLock(LockMode.READ) {
                try {
                    ApplicationConfiguration.AddDownloadDialogPosition(
                        getInt(APPLICATION_UI_ADD_DOWNLOAD_DIALOG_X),
                        getInt(APPLICATION_UI_ADD_DOWNLOAD_DIALOG_Y),
                    )
                } catch (_: NoSuchElementException) {
                    ApplicationConfiguration.AddDownloadDialogPosition.empty()
                }
            }

    fun setAddDownloadDialogPosition(x: Int, y: Int) {
        config.withLock(LockMode.WRITE) {
            setProperty(APPLICATION_UI_ADD_DOWNLOAD_DIALOG_X, x)
            setProperty(APPLICATION_UI_ADD_DOWNLOAD_DIALOG_Y, y)
        }
    }

    fun clearAddDownloadDialogSize() {
        config.withLock(LockMode.WRITE) {
            clearProperty(APPLICATION_UI_ADD_DOWNLOAD_DIALOG_WIDTH)
            clearProperty(APPLICATION_UI_ADD_DOWNLOAD_DIALOG_HEIGHT)
        }
    }

    val manageAboDialogState: ApplicationConfiguration.ManageAboDialogState
        get() =
            config.withLock(LockMode.READ) {
                try {
                    ApplicationConfiguration.ManageAboDialogState(
                        getInt(APPLICATION_UI_MANAGE_ABO_DIALOG_X),
                        getInt(APPLICATION_UI_MANAGE_ABO_DIALOG_Y),
                        getInt(APPLICATION_UI_MANAGE_ABO_DIALOG_WIDTH),
                        getInt(APPLICATION_UI_MANAGE_ABO_DIALOG_HEIGHT),
                    )
                } catch (_: NoSuchElementException) {
                    ApplicationConfiguration.ManageAboDialogState.empty()
                }
            }

    fun setManageAboDialogBounds(x: Int, y: Int, width: Int, height: Int) {
        config.setBounds(
            xKey = APPLICATION_UI_MANAGE_ABO_DIALOG_X,
            yKey = APPLICATION_UI_MANAGE_ABO_DIALOG_Y,
            widthKey = APPLICATION_UI_MANAGE_ABO_DIALOG_WIDTH,
            heightKey = APPLICATION_UI_MANAGE_ABO_DIALOG_HEIGHT,
            x = x,
            y = y,
            width = width,
            height = height,
        )
    }

    val duplicateFilmDetailsDialogState: ApplicationConfiguration.DuplicateFilmDetailsDialogState
        get() =
            config.withLock(LockMode.READ) {
                try {
                    ApplicationConfiguration.DuplicateFilmDetailsDialogState(
                        getInt(APPLICATION_UI_DUPLICATE_FILM_DETAILS_DIALOG_X),
                        getInt(APPLICATION_UI_DUPLICATE_FILM_DETAILS_DIALOG_Y),
                        getInt(APPLICATION_UI_DUPLICATE_FILM_DETAILS_DIALOG_WIDTH),
                        getInt(APPLICATION_UI_DUPLICATE_FILM_DETAILS_DIALOG_HEIGHT),
                    )
                } catch (_: NoSuchElementException) {
                    ApplicationConfiguration.DuplicateFilmDetailsDialogState.empty()
                }
            }

    fun setDuplicateFilmDetailsDialogBounds(x: Int, y: Int, width: Int, height: Int) {
        config.setBounds(
            xKey = APPLICATION_UI_DUPLICATE_FILM_DETAILS_DIALOG_X,
            yKey = APPLICATION_UI_DUPLICATE_FILM_DETAILS_DIALOG_Y,
            widthKey = APPLICATION_UI_DUPLICATE_FILM_DETAILS_DIALOG_WIDTH,
            heightKey = APPLICATION_UI_DUPLICATE_FILM_DETAILS_DIALOG_HEIGHT,
            x = x,
            y = y,
            width = width,
            height = height,
        )
    }

    val duplicateStatisticsDialogState: ApplicationConfiguration.DuplicateStatisticsDialogState
        get() =
            config.withLock(LockMode.READ) {
                try {
                    ApplicationConfiguration.DuplicateStatisticsDialogState(
                        getInt(APPLICATION_UI_DUPLICATE_STATISTICS_DIALOG_X),
                        getInt(APPLICATION_UI_DUPLICATE_STATISTICS_DIALOG_Y),
                        getInt(APPLICATION_UI_DUPLICATE_STATISTICS_DIALOG_WIDTH),
                        getInt(APPLICATION_UI_DUPLICATE_STATISTICS_DIALOG_HEIGHT),
                    )
                } catch (_: NoSuchElementException) {
                    ApplicationConfiguration.DuplicateStatisticsDialogState.empty()
                }
            }

    fun setDuplicateStatisticsDialogBounds(x: Int, y: Int, width: Int, height: Int) {
        config.setBounds(
            xKey = APPLICATION_UI_DUPLICATE_STATISTICS_DIALOG_X,
            yKey = APPLICATION_UI_DUPLICATE_STATISTICS_DIALOG_Y,
            widthKey = APPLICATION_UI_DUPLICATE_STATISTICS_DIALOG_WIDTH,
            heightKey = APPLICATION_UI_DUPLICATE_STATISTICS_DIALOG_HEIGHT,
            x = x,
            y = y,
            width = width,
            height = height,
        )
    }

    val editHistoryDialogState: ApplicationConfiguration.EditHistoryDialogState
        get() =
            config.withLock(LockMode.READ) {
                try {
                    ApplicationConfiguration.EditHistoryDialogState(
                        getInt(APPLICATION_UI_EDIT_HISTORY_DIALOG_X),
                        getInt(APPLICATION_UI_EDIT_HISTORY_DIALOG_Y),
                        getInt(APPLICATION_UI_EDIT_HISTORY_DIALOG_WIDTH),
                        getInt(APPLICATION_UI_EDIT_HISTORY_DIALOG_HEIGHT),
                    )
                } catch (_: NoSuchElementException) {
                    ApplicationConfiguration.EditHistoryDialogState.empty()
                }
            }

    fun setEditHistoryDialogBounds(x: Int, y: Int, width: Int, height: Int) {
        config.setBounds(
            xKey = APPLICATION_UI_EDIT_HISTORY_DIALOG_X,
            yKey = APPLICATION_UI_EDIT_HISTORY_DIALOG_Y,
            widthKey = APPLICATION_UI_EDIT_HISTORY_DIALOG_WIDTH,
            heightKey = APPLICATION_UI_EDIT_HISTORY_DIALOG_HEIGHT,
            x = x,
            y = y,
            width = width,
            height = height,
        )
    }

    val bookmarkDialogBounds: ApplicationConfiguration.BookmarkDialogBounds
        get() =
            config.withLock(LockMode.READ) {
                ApplicationConfiguration.BookmarkDialogBounds(
                    getInt(APPLICATION_UI_BOOKMARK_DIALOG_X, 100),
                    getInt(APPLICATION_UI_BOOKMARK_DIALOG_Y, 100),
                    getInt(APPLICATION_UI_BOOKMARK_DIALOG_WIDTH, 800),
                    getInt(APPLICATION_UI_BOOKMARK_DIALOG_HEIGHT, 600),
                )
            }

    fun setBookmarkDialogBounds(x: Int, y: Int, width: Int, height: Int) {
        config.setBounds(
            xKey = APPLICATION_UI_BOOKMARK_DIALOG_X,
            yKey = APPLICATION_UI_BOOKMARK_DIALOG_Y,
            widthKey = APPLICATION_UI_BOOKMARK_DIALOG_WIDTH,
            heightKey = APPLICATION_UI_BOOKMARK_DIALOG_HEIGHT,
            x = x,
            y = y,
            width = width,
            height = height,
        )
    }

    val mainWindowMaximized: Boolean
        get() = config.getBoolean(APPLICATION_UI_MAINWINDOW_MAXIMIZED, true)

    fun getMainWindowBounds(minimumWidth: Int, minimumHeight: Int): ApplicationConfiguration.MainWindowBounds =
        config.withLock(LockMode.READ) {
            ApplicationConfiguration.MainWindowBounds(
                getInt(APPLICATION_UI_MAINWINDOW_LOCATION_X),
                getInt(APPLICATION_UI_MAINWINDOW_LOCATION_Y),
                max(getInt(APPLICATION_UI_MAINWINDOW_WIDTH, minimumWidth), minimumWidth),
                max(getInt(APPLICATION_UI_MAINWINDOW_HEIGHT, minimumHeight), minimumHeight),
            )
        }

    fun setMainWindowResizedState(maximized: Boolean, width: Int, height: Int) {
        config.withLock(LockMode.WRITE) {
            setProperty(APPLICATION_UI_MAINWINDOW_MAXIMIZED, maximized)
            setProperty(APPLICATION_UI_MAINWINDOW_WIDTH, width)
            setProperty(APPLICATION_UI_MAINWINDOW_HEIGHT, height)
        }
    }

    fun setMainWindowMovedState(maximized: Boolean, x: Int, y: Int) {
        config.withLock(LockMode.WRITE) {
            setProperty(APPLICATION_UI_MAINWINDOW_MAXIMIZED, maximized)
            setProperty(APPLICATION_UI_MAINWINDOW_LOCATION_X, x)
            setProperty(APPLICATION_UI_MAINWINDOW_LOCATION_Y, y)
        }
    }

    private fun XMLConfiguration.setBounds(
        xKey: String,
        yKey: String,
        widthKey: String,
        heightKey: String,
        x: Int,
        y: Int,
        width: Int,
        height: Int,
    ) {
        withLock(LockMode.WRITE) {
            setProperty(xKey, x)
            setProperty(yKey, y)
            setProperty(widthKey, width)
            setProperty(heightKey, height)
        }
    }

    private companion object {
        const val APPLICATION_UI_MAINWINDOW_MAXIMIZED = "application.ui.mainwindow.maximized"
        const val APPLICATION_UI_MAINWINDOW_WIDTH = "application.ui.mainwindow.width"
        const val APPLICATION_UI_MAINWINDOW_HEIGHT = "application.ui.mainwindow.height"
        const val APPLICATION_UI_MAINWINDOW_LOCATION_X = "application.ui.mainwindow.location.x"
        const val APPLICATION_UI_MAINWINDOW_LOCATION_Y = "application.ui.mainwindow.location.y"

        const val APPLICATION_UI_BANDWIDTH_MONITOR_VISIBLE = "application.ui.bandwidth_monitor.visible"
        const val APPLICATION_UI_BANDWIDTH_MONITOR_X = "bandwidth_monitor.x"
        const val APPLICATION_UI_BANDWIDTH_MONITOR_Y = "bandwidth_monitor.y"
        const val APPLICATION_UI_BANDWIDTH_MONITOR_WIDTH = "bandwidth_monitor.width"
        const val APPLICATION_UI_BANDWIDTH_MONITOR_HEIGHT = "bandwidth_monitor.height"

        const val APPLICATION_UI_MEMORY_MONITOR_VISIBLE = "memory_monitor.visible"
        const val APPLICATION_UI_MEMORY_MONITOR_X = "memory_monitor.x"
        const val APPLICATION_UI_MEMORY_MONITOR_Y = "memory_monitor.y"
        const val APPLICATION_UI_MEMORY_MONITOR_WIDTH = "memory_monitor.width"
        const val APPLICATION_UI_MEMORY_MONITOR_HEIGHT = "memory_monitor.height"

        const val APPLICATION_UI_FILM_INFO_VISIBLE = "film.information.visible"
        const val APPLICATION_UI_FILM_INFO_X = "film.information.location.x"
        const val APPLICATION_UI_FILM_INFO_Y = "film.information.location.y"
        const val APPLICATION_UI_FILM_INFO_WIDTH = "film.information.location.width"
        const val APPLICATION_UI_FILM_INFO_HEIGHT = "film.information.location.height"

        const val APPLICATION_UI_LOAD_FILM_LIST_DIALOG_X = "dialog.load_filmlist.x"
        const val APPLICATION_UI_LOAD_FILM_LIST_DIALOG_Y = "dialog.load_filmlist.y"
        const val APPLICATION_UI_LOAD_FILM_LIST_DIALOG_WIDTH = "dialog.load_filmlist.width"
        const val APPLICATION_UI_LOAD_FILM_LIST_DIALOG_HEIGHT = "dialog.load_filmlist.height"

        const val APPLICATION_UI_SETTINGS_DIALOG_X = "application.ui.settings_dialog.x"
        const val APPLICATION_UI_SETTINGS_DIALOG_Y = "application.ui.settings_dialog.y"
        const val APPLICATION_UI_SETTINGS_DIALOG_WIDTH = "application.ui.settings_dialog.width"
        const val APPLICATION_UI_SETTINGS_DIALOG_HEIGHT = "application.ui.settings_dialog.height"

        const val APPLICATION_UI_FILTER_DIALOG_VISIBLE = "application.ui.filter_dialog.visible"
        const val APPLICATION_UI_FILTER_DIALOG_X = "application.ui.filter_dialog.location.x"
        const val APPLICATION_UI_FILTER_DIALOG_Y = "application.ui.filter_dialog.location.y"
        const val APPLICATION_UI_FILTER_DIALOG_WIDTH = "application.ui.filter_dialog.width"
        const val APPLICATION_UI_FILTER_DIALOG_HEIGHT = "application.ui.filter_dialog.height"

        const val APPLICATION_UI_EDIT_DOWNLOAD_DIALOG_X = "edit_download_dialog.x"
        const val APPLICATION_UI_EDIT_DOWNLOAD_DIALOG_Y = "edit_download_dialog.y"
        const val APPLICATION_UI_EDIT_DOWNLOAD_DIALOG_WIDTH = "edit_download_dialog.width"
        const val APPLICATION_UI_EDIT_DOWNLOAD_DIALOG_HEIGHT = "edit_download_dialog.height"

        const val APPLICATION_UI_ADD_DOWNLOAD_DIALOG_X = "application.ui.adddownload_dialog.location.x"
        const val APPLICATION_UI_ADD_DOWNLOAD_DIALOG_Y = "application.ui.adddownload_dialog.location.y"
        const val APPLICATION_UI_ADD_DOWNLOAD_DIALOG_WIDTH = "application.ui.adddownload_dialog.width"
        const val APPLICATION_UI_ADD_DOWNLOAD_DIALOG_HEIGHT = "application.ui.adddownload_dialog.height"

        const val APPLICATION_UI_MANAGE_ABO_DIALOG_X = "manage_abo_dialog.x"
        const val APPLICATION_UI_MANAGE_ABO_DIALOG_Y = "manage_abo_dialog.y"
        const val APPLICATION_UI_MANAGE_ABO_DIALOG_WIDTH = "manage_abo_dialog.width"
        const val APPLICATION_UI_MANAGE_ABO_DIALOG_HEIGHT = "manage_abo_dialog.height"

        const val APPLICATION_UI_DUPLICATE_FILM_DETAILS_DIALOG_X = "duplicate_film_details_dialog.x"
        const val APPLICATION_UI_DUPLICATE_FILM_DETAILS_DIALOG_Y = "duplicate_film_details_dialog.y"
        const val APPLICATION_UI_DUPLICATE_FILM_DETAILS_DIALOG_WIDTH = "duplicate_film_details_dialog.width"
        const val APPLICATION_UI_DUPLICATE_FILM_DETAILS_DIALOG_HEIGHT = "duplicate_film_details_dialog.height"

        const val APPLICATION_UI_DUPLICATE_STATISTICS_DIALOG_X = "duplicate_statistics_dialog.x"
        const val APPLICATION_UI_DUPLICATE_STATISTICS_DIALOG_Y = "duplicate_statistics_dialog.y"
        const val APPLICATION_UI_DUPLICATE_STATISTICS_DIALOG_WIDTH = "duplicate_statistics_dialog.width"
        const val APPLICATION_UI_DUPLICATE_STATISTICS_DIALOG_HEIGHT = "duplicate_statistics_dialog.height"

        const val APPLICATION_UI_EDIT_HISTORY_DIALOG_X = "edit_history.x"
        const val APPLICATION_UI_EDIT_HISTORY_DIALOG_Y = "edit_history.y"
        const val APPLICATION_UI_EDIT_HISTORY_DIALOG_WIDTH = "edit_history.width"
        const val APPLICATION_UI_EDIT_HISTORY_DIALOG_HEIGHT = "edit_history.height"

        const val APPLICATION_UI_BOOKMARK_DIALOG_PREFIX = "ui.bookmark-dialog"
        const val APPLICATION_UI_BOOKMARK_DIALOG_X = "$APPLICATION_UI_BOOKMARK_DIALOG_PREFIX.x"
        const val APPLICATION_UI_BOOKMARK_DIALOG_Y = "$APPLICATION_UI_BOOKMARK_DIALOG_PREFIX.y"
        const val APPLICATION_UI_BOOKMARK_DIALOG_WIDTH = "$APPLICATION_UI_BOOKMARK_DIALOG_PREFIX.width"
        const val APPLICATION_UI_BOOKMARK_DIALOG_HEIGHT = "$APPLICATION_UI_BOOKMARK_DIALOG_PREFIX.height"
    }
}
