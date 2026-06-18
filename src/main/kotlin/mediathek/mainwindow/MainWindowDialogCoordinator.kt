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

package mediathek.mainwindow

import mediathek.config.application.ApplicationConfiguration
import mediathek.gui.actions.ManageAboAction
import mediathek.gui.actions.MemoryMonitorAction
import mediathek.gui.actions.ShowBandwidthUsageAction
import mediathek.gui.dialogEinstellungen.DialogEinstellungen
import mediathek.gui.filmInformation.FilmInfoDialog
import org.apache.logging.log4j.LogManager

class MainWindowDialogCoordinator(
    private val owner: MediathekGui,
    private val showMemoryMonitorAction: MemoryMonitorAction,
    private val showBandwidthUsageAction: ShowBandwidthUsageAction,
    private val manageAboAction: ManageAboAction,
) {
    private var filmInfo: FilmInfoDialog? = null
    private var settingsDialog: DialogEinstellungen? = null

    fun restoreStartupDialogs() {
        createMemoryMonitorIfConfigured()
        loadBandwidthMonitorIfConfigured()
    }

    fun createMemoryMonitorIfConfigured() {
        if (ApplicationConfiguration.getInstance().memoryMonitorDialogVisible) {
            showMemoryMonitorAction.showMemoryMonitor()
        }
    }

    fun loadBandwidthMonitorIfConfigured() {
        logger.trace("Loading bandwidth monitor")
        if (ApplicationConfiguration.getInstance().bandwidthMonitorVisible) {
            showBandwidthUsageAction.actionPerformed(null)
        }
        logger.trace("Finished loading bandwidth monitor")
    }

    fun setupFilmInfoDialog() {
        logger.trace("Loading info dialog")
        filmInfo = FilmInfoDialog(owner)
        logger.trace("Finished loading info dialog")
    }

    fun getFilmInfoDialog(): FilmInfoDialog? = filmInfo

    fun getSettingsDialog(): DialogEinstellungen =
        settingsDialog ?: DialogEinstellungen(owner).also {
            settingsDialog = it
        }

    fun closeMemoryMonitor() {
        val config = ApplicationConfiguration.getInstance()
        val wasVisible = config.memoryMonitorDialogVisible
        if (wasVisible) {
            showMemoryMonitorAction.closeMemoryMonitorForShutdown()
        } else {
            showMemoryMonitorAction.closeMemoryMonitor()
        }
        if (wasVisible) {
            config.memoryMonitorDialogVisible = true
        }
    }

    fun closeBandwidthMonitor() {
        val config = ApplicationConfiguration.getInstance()
        val wasVisible = config.bandwidthMonitorVisible
        if (wasVisible) {
            showBandwidthUsageAction.closeBandwidthMonitorForShutdown()
        } else {
            showBandwidthUsageAction.dialogOptional.ifPresent { dialog ->
                dialog.dispose()
            }
        }
        if (wasVisible) {
            config.bandwidthMonitorVisible = true
        }
    }

    fun closeAboDialog() {
        manageAboAction.closeDialog()
    }

    private companion object {
        private val logger = LogManager.getLogger()
    }
}
