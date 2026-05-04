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

package mediathek.gui.tabs.tab_downloads

import kotlinx.coroutines.*
import kotlinx.coroutines.swing.Swing
import mediathek.daten.DatenDownload
import mediathek.tool.ApplicationConfiguration
import org.apache.logging.log4j.LogManager
import java.lang.Runnable
import java.util.*
import java.util.concurrent.ConcurrentHashMap

class DownloadSizeLookupService(
    private val reloadTable: Runnable
) {
    private val logger = LogManager.getLogger(DownloadSizeLookupService::class.java)
    private val scope = CoroutineScope(SupervisorJob() + Dispatchers.IO.limitedParallelism(1))
    private val inFlight: MutableSet<DatenDownload> = Collections.newSetFromMap(ConcurrentHashMap())

    fun updateFilmSizes(downloads: List<DatenDownload>) {
        if (downloads.isEmpty()) {
            return
        }

        scope.launch {
            var updateNeeded = false

            for (download in downloads) {
                if (!download.needsLiveSizeLookup() || !inFlight.add(download)) {
                    continue
                }

                try {
                    val oldSize = download.mVFilmSize.size
                    val currentLocation = ApplicationConfiguration.getInstance().geographicLocation
                    val wasGeoBlocked = download.film?.isGeoBlockedForLocation(currentLocation) ?: false
                    download.queryLiveSize()
                    val isGeoBlocked = download.film?.isGeoBlockedForLocation(currentLocation) ?: false
                    if (download.mVFilmSize.size != oldSize || isGeoBlocked != wasGeoBlocked) {
                        updateNeeded = true
                    }
                } catch (ex: RuntimeException) {
                    logger.debug("Could not update live size for download {}", download.arr[DatenDownload.DOWNLOAD_TITEL], ex)
                } finally {
                    inFlight.remove(download)
                }
            }

            if (updateNeeded) {
                withContext(Dispatchers.Swing) {
                    reloadTable.run()
                }
            }
        }
    }

    private fun DatenDownload.needsLiveSizeLookup(): Boolean =
        film != null && mVFilmSize.size == 0L
}
