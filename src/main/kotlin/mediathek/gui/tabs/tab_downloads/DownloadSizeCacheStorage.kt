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

import kotlinx.serialization.Serializable
import kotlinx.serialization.json.Json
import mediathek.config.StandardLocations
import mediathek.daten.Country
import mediathek.tool.FileUtils
import org.apache.logging.log4j.LogManager
import java.nio.file.Path
import kotlin.io.path.*

internal object DownloadSizeCacheStorage {
    private const val FILE_VERSION = 1
    private const val STORAGE_FILENAME = "download-size-cache.json"
    private val logger = LogManager.getLogger(DownloadSizeCacheStorage::class.java)
    private val json = Json {
        ignoreUnknownKeys = true
        prettyPrint = true
        encodeDefaults = false
    }

    fun load(
        storagePath: Path = storagePath(),
        nowMillis: Long = System.currentTimeMillis(),
    ): DownloadSizeCacheSnapshot {
        if (!storagePath.exists()) {
            return DownloadSizeCacheSnapshot()
        }

        return runCatching {
            val cacheFile = json.decodeFromString<DownloadSizeCacheFile>(storagePath.readText())
            if (cacheFile.version != FILE_VERSION) {
                return DownloadSizeCacheSnapshot()
            }

            DownloadSizeCacheSnapshot(
                lookupResults = cacheFile.lookupResults
                    .asSequence()
                    .filter { it.isFresh(nowMillis) }
                    .filter { it.byteLength > 0 }
                    .take(DownloadSizeCachePolicy.MAXIMUM_ENTRIES)
                    .toList(),
                knownAboSizes = cacheFile.knownAboSizes
                    .asSequence()
                    .filter { it.isFresh(nowMillis) }
                    .filter { it.byteLength > 0 }
                    .take(DownloadSizeCachePolicy.MAXIMUM_ENTRIES)
                    .toList(),
            )
        }.getOrElse { ex ->
            logger.debug("Could not load download size cache from {}", storagePath, ex)
            DownloadSizeCacheSnapshot()
        }
    }

    fun save(
        snapshot: DownloadSizeCacheSnapshot,
        storagePath: Path = storagePath(),
        nowMillis: Long = System.currentTimeMillis(),
    ) {
        val cacheFile = DownloadSizeCacheFile(
            version = FILE_VERSION,
            lookupResults = snapshot.lookupResults
                .asSequence()
                .filter { it.isFresh(nowMillis) }
                .filter { it.byteLength > 0 }
                .take(DownloadSizeCachePolicy.MAXIMUM_ENTRIES)
                .toList(),
            knownAboSizes = snapshot.knownAboSizes
                .asSequence()
                .filter { it.isFresh(nowMillis) }
                .filter { it.byteLength > 0 }
                .take(DownloadSizeCachePolicy.MAXIMUM_ENTRIES)
                .toList(),
        )
        val temporaryPath = storagePath.resolveSibling(storagePath.fileName.toString() + ".tmp")

        try {
            storagePath.parent?.createDirectories()
            temporaryPath.writeText(json.encodeToString(cacheFile))
            FileUtils.moveAtomicallyWithFallback(temporaryPath, storagePath)
        } catch (ex: Exception) {
            logger.debug("Could not save download size cache to {}", storagePath, ex)
        } finally {
            temporaryPath.deleteIfExists()
        }
    }

    private fun PersistentCacheEntry.isFresh(nowMillis: Long): Boolean =
        storedAtMillis > 0 &&
            storedAtMillis <= nowMillis &&
            nowMillis - storedAtMillis <= DownloadSizeCachePolicy.maximumEntryAge.inWholeMilliseconds

    private fun storagePath(): Path = StandardLocations.getSettingsDirectory().resolve(STORAGE_FILENAME)
}

internal data class DownloadSizeCacheSnapshot(
    val lookupResults: List<PersistentLookupCacheEntry> = emptyList(),
    val knownAboSizes: List<PersistentKnownAboSize> = emptyList(),
)

@Serializable
private data class DownloadSizeCacheFile(
    val version: Int = 1,
    val lookupResults: List<PersistentLookupCacheEntry> = emptyList(),
    val knownAboSizes: List<PersistentKnownAboSize> = emptyList(),
)

@Serializable
internal data class PersistentLookupCacheEntry(
    val url: String,
    val location: Country,
    val fetchSizeEnabled: Boolean,
    val probeHlsSegments: Boolean,
    val byteLength: Long,
    override val storedAtMillis: Long,
    val httpStatusCode: Int? = null,
    val resolutionUrl: String? = null,
    val quality: String? = null,
) : PersistentCacheEntry

@Serializable
internal data class PersistentKnownAboSize(
    val key: String,
    val byteLength: Long,
    override val storedAtMillis: Long,
) : PersistentCacheEntry

internal sealed interface PersistentCacheEntry {
    val storedAtMillis: Long
}
