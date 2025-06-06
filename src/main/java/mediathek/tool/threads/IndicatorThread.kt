/*
 * Copyright (c) 2025 derreisende77.
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

package mediathek.tool.threads

import mediathek.config.Daten
import mediathek.controller.starter.Start
import mediathek.daten.DatenDownload

/**
 * Base class for platform-specific progress indicator threads
 */
open class IndicatorThread : Thread() {
    protected val daten: Daten
    protected fun calculateOverallPercentage(): Double {
        var numOfDownloadsActive = 0
        var accumPercentage = 0.0
        //only count running/active downloads and calc accumulated progress..
        val activeDownloadList = daten.listeDownloads.getListOfStartsNotFinished(DatenDownload.QUELLE_ALLE.toInt())
        for (download in activeDownloadList) {
            if (download.start.status == Start.STATUS_RUN) {
                numOfDownloadsActive++
                accumPercentage += download.start.percent / 10.0
            }
        }
        activeDownloadList.clear()
        return accumPercentage / numOfDownloadsActive
    }

    init {
        name = "IndicatorThread"
        daten = Daten.getInstance()
    }
}