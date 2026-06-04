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

import com.github.benmanes.caffeine.cache.Caffeine
import kotlinx.coroutines.*
import kotlinx.coroutines.swing.Swing
import mediathek.daten.Country
import mediathek.daten.DatenDownload
import mediathek.tool.ApplicationConfiguration
import mediathek.tool.FileSize
import okhttp3.HttpUrl.Companion.toHttpUrlOrNull
import org.apache.logging.log4j.LogManager
import java.lang.Runnable

internal class DownloadSizeLookupService(
    private val reloadTable: Runnable,
    persistedLookupResults: List<PersistentLookupCacheEntry> = emptyList(),
) {
    private val logger = LogManager.getLogger(DownloadSizeLookupService::class.java)
    private val scope = CoroutineScope(SupervisorJob() + Dispatchers.IO.limitedParallelism(1))
    private val cachedLookupResults = Caffeine.newBuilder()
        .maximumSize(CACHE_MAXIMUM_SIZE)
        .build<LookupKey, CachedLookupResult>()

    init {
        persistedLookupResults.forEach { entry ->
            cachedLookupResults.put(entry.toLookupKey(), entry.toCachedLookupResult())
        }
    }

    fun updateFilmSizes(downloads: List<DatenDownload>, forceLookup: Boolean = false) {
        if (downloads.isEmpty()) {
            return
        }

        scope.launch {
            var updateNeeded = false

            for (download in downloads) {
                if (!download.needsLiveSizeLookup(forceLookup)) {
                    continue
                }

                val currentLocation = ApplicationConfiguration.getInstance().geographicLocation
                val fetchSizeEnabled = ApplicationConfiguration.getConfiguration()
                    .getBoolean(ApplicationConfiguration.DOWNLOAD_FETCH_FILE_SIZE, true)
                val probeHlsSegments = forceLookup
                val lookupKey = LookupKey(
                    url = download.downloadUrl,
                    location = currentLocation,
                    fetchSizeEnabled = fetchSizeEnabled,
                    probeHlsSegments = probeHlsSegments,
                )

                if (!forceLookup) {
                    val cachedResult = cachedLookupResults.getIfPresent(lookupKey)
                    if (cachedResult != null) {
                        updateNeeded = download.applyLookupResultIfChanged(cachedResult.lookupResult, currentLocation) || updateNeeded
                        continue
                    }
                }

                try {
                    val oldSize = download.runtime.filmSize.size
                    val wasGeoBlocked = download.isGeoBlockedFor(currentLocation)
                    val lookupResult = download.queryLiveSize(
                        forceFetch = forceLookup,
                        probeHlsSegments = probeHlsSegments,
                    )
                    if (lookupResult != null && lookupResult.byteLength > 0 && !forceLookup) {
                        cachedLookupResults.put(
                            lookupKey,
                            CachedLookupResult(
                                lookupResult = lookupResult,
                                storedAtMillis = System.currentTimeMillis(),
                            ),
                        )
                    }
                    if (download.runtime.filmSize.size != oldSize || download.isGeoBlockedFor(currentLocation) != wasGeoBlocked) {
                        updateNeeded = true
                    }
                } catch (ex: RuntimeException) {
                    logger.debug("Could not update live size for download {}", download.title, ex)
                }
            }

            if (updateNeeded) {
                withContext(Dispatchers.Swing) {
                    reloadTable.run()
                }
            }
        }
    }

    fun snapshotLookupResults(): List<PersistentLookupCacheEntry> =
        cachedLookupResults.asMap().map { (key, value) ->
            PersistentLookupCacheEntry(
                url = key.url,
                location = key.location,
                fetchSizeEnabled = key.fetchSizeEnabled,
                probeHlsSegments = key.probeHlsSegments,
                byteLength = value.lookupResult.byteLength,
                storedAtMillis = value.storedAtMillis,
                httpStatusCode = value.lookupResult.httpStatusCode,
                resolutionUrl = value.lookupResult.resolutionUrl?.toString(),
                quality = value.lookupResult.quality,
            )
        }

    private fun DatenDownload.needsLiveSizeLookup(forceLookup: Boolean): Boolean =
        film != null && (forceLookup || runtime.filmSize.size == 0L)

    private fun DatenDownload.applyLookupResultIfChanged(
        lookupResult: FileSize.LookupResult,
        location: Country,
    ): Boolean {
        val oldSize = runtime.filmSize.size
        val wasGeoBlocked = isGeoBlockedFor(location)
        applyLiveSizeLookupResult(lookupResult)
        return runtime.filmSize.size != oldSize || isGeoBlockedFor(location) != wasGeoBlocked
    }

    private fun DatenDownload.isGeoBlockedFor(location: Country): Boolean =
        film?.isGeoBlockedForLocation(location) ?: false

    private data class LookupKey(
        val url: String,
        val location: Country,
        val fetchSizeEnabled: Boolean,
        val probeHlsSegments: Boolean,
    )

    private data class CachedLookupResult(
        val lookupResult: FileSize.LookupResult,
        val storedAtMillis: Long,
    )

    private fun PersistentLookupCacheEntry.toLookupKey(): LookupKey =
        LookupKey(
            url = url,
            location = location,
            fetchSizeEnabled = fetchSizeEnabled,
            probeHlsSegments = probeHlsSegments,
        )

    private fun PersistentLookupCacheEntry.toCachedLookupResult(): CachedLookupResult =
        CachedLookupResult(
            lookupResult = FileSize.LookupResult(
                byteLength = byteLength,
                httpStatusCode = httpStatusCode,
                resolutionUrl = resolutionUrl?.toHttpUrlOrNull(),
                quality = quality,
            ),
            storedAtMillis = storedAtMillis,
        )

    private companion object {
        private const val CACHE_MAXIMUM_SIZE = 4096L
    }
}
