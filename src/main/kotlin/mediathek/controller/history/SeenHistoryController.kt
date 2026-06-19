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

package mediathek.controller.history

import kotlinx.coroutines.CoroutineDispatcher
import kotlinx.coroutines.Dispatchers
import kotlinx.coroutines.runBlocking
import kotlinx.coroutines.withContext
import mediathek.audiothek.model.AudioEntry
import mediathek.config.Daten
import mediathek.config.application.ApplicationConfiguration
import mediathek.daten.DatenFilm
import mediathek.gui.messages.history.DownloadHistoryChangedEvent
import mediathek.sqlite.SeenHistoryCorruptionHandler
import mediathek.tool.MessageBus
import mediathek.tool.sql.SqlDatabaseConfig
import org.apache.logging.log4j.LogManager
import java.nio.file.Path
import java.sql.SQLException
import java.time.LocalDate
import java.time.temporal.ChronoUnit
import java.util.concurrent.atomic.AtomicBoolean

/**
 * Database based seen history controller.
 *
 * Public methods remain blocking for compatibility, while all JDBC access is confined
 * to a process-wide coroutine dispatcher with parallelism 1.
 */
class SeenHistoryController : AutoCloseable {
    private val closed = AtomicBoolean(false)
    private val store = sharedStore()

    fun removeAll() {
        val removed = runStoreCatching("removeAll", false) {
            removeAllEntries()
            true
        }
        if (removed) {
            SeenHistoryCache.clear()
            sendChangeMessage()
        }
    }

    fun markUnseen(film: DatenFilm) {
        val success = runStoreCatching("markUnseen", false) {
            removeSeenUrl(film.urlNormalQuality)
            true
        }
        if (success) {
            SeenHistoryCache.remove(film.urlNormalQuality)
            Daten.getInstance().listeBookmarkList.updateSeen(false, film)
            sendChangeMessage()
        }
    }

    fun markUnseen(list: List<DatenFilm>) {
        val urls = list.asSequence()
            .map { it.urlNormalQuality }
            .filter(String::isNotBlank)
            .distinct()
            .toList()

        val success = runStoreCatching("markUnseen", false) {
            if (urls.isNotEmpty()) {
                removeSeenUrls(urls)
            }
            true
        }
        if (success) {
            SeenHistoryCache.remove(urls)
            Daten.getInstance().listeBookmarkList.updateSeen(false, list)
            sendChangeMessage()
        }
    }

    fun markSeen(film: DatenFilm?) {
        if (film == null) {
            logger.warn("markSeen: no film found")
            return
        }

        val entry = film.toSeenHistoryEntry() ?: return
        if (SeenHistoryCache.contains(entry.url)) {
            return
        }

        val inserted = runStoreCatching("markSeen single", false) {
            insertSeenEntry(entry)
        }
        if (inserted) {
            SeenHistoryCache.add(entry.url)
            Daten.getInstance().listeBookmarkList.updateSeen(true, film)
            sendChangeMessage()
        }
    }

    fun markSeen(list: List<DatenFilm>) {
        val candidates = list
            .asSequence()
            .mapNotNull { film -> film.toSeenHistoryEntry() }
            .distinctBy(SeenHistoryEntry::url)
            .toList()

        val success = runStoreCatching("markSeen", false) {
            if (candidates.isNotEmpty()) {
                insertSeenEntries(candidates)
            }
            true
        }
        if (success) {
            SeenHistoryCache.add(candidates.asSequence().map(SeenHistoryEntry::url).toList())
            Daten.getInstance().listeBookmarkList.updateSeen(true, list)
            sendChangeMessage()
        }
    }

    fun markSeen(entry: AudioEntry?) {
        if (entry == null) {
            logger.warn("markSeen: no audio entry found")
            return
        }

        val historyEntry = entry.toSeenHistoryEntry() ?: return
        if (SeenHistoryCache.contains(historyEntry.url)) {
            return
        }

        val inserted = runStoreCatching("markSeen audio", false) {
            insertSeenEntry(historyEntry)
        }
        if (inserted) {
            SeenHistoryCache.add(historyEntry.url)
            sendChangeMessage()
        }
    }

    fun markUnseen(entry: AudioEntry) {
        val url = entry.audioUrl?.toString().orEmpty()
        if (url.isBlank()) {
            return
        }

        val success = runStoreCatching("markUnseen audio", false) {
            removeSeenUrl(url)
            true
        }
        if (success) {
            SeenHistoryCache.remove(url)
            sendChangeMessage()
        }
    }

    /**
     * Load all URLs from database and store them in the process-wide memory cache.
     */
    fun prepareMemoryCache() {
        if (SeenHistoryCache.isPrepared()) {
            return
        }

        val urls = runStoreCatching("prepareMemoryCache", null as Set<String>?) {
            loadAllUrls()
        } ?: return

        SeenHistoryCache.load(urls)
        logger.trace("cache size: {}", SeenHistoryCache.size())
    }

    fun performMaintenance() {
        logger.trace("Start maintenance")

        val applicationConfiguration = ApplicationConfiguration.getInstance()
        val lastRunDate = applicationConfiguration.seenHistoryMaintenanceLastRun
        val now = LocalDate.now()
        val shouldRunHeavyMaintenance = lastRunDate == null ||
            ChronoUnit.DAYS.between(lastRunDate, now) >= MAX_DAYS

        val success = runStoreCatching("Failed to execute maintenance script", false) {
            performMaintenance(shouldRunHeavyMaintenance)
            true
        }
        if (success) {
            SeenHistoryCache.clear()
            if (shouldRunHeavyMaintenance) {
                applicationConfiguration.seenHistoryMaintenanceLastRun = now
            }
        }

        logger.trace("Finished maintenance")
    }

    fun performDatabaseCompact() {
        runStoreCatching("performDatabaseCompact", Unit) {
            compactDatabase()
        }
    }

    fun hasBeenSeen(film: DatenFilm): Boolean {
        if (SeenHistoryCache.isPrepared()) {
            return SeenHistoryCache.contains(film.urlNormalQuality)
        }

        return runStoreCatching("hasBeenSeen", false) {
            containsUrl(film.urlNormalQuality)
        }
    }

    fun hasBeenSeen(entry: AudioEntry): Boolean {
        val url = entry.audioUrl?.toString().orEmpty()
        if (url.isBlank()) {
            return false
        }

        if (SeenHistoryCache.isPrepared()) {
            return SeenHistoryCache.contains(url)
        }

        return runStoreCatching("hasBeenSeen", false) {
            containsUrl(url)
        }
    }

    override fun close() {
        closed.set(true)
    }

    private fun <T> runStore(block: suspend SeenHistoryStore.() -> T): T = runBlocking {
        withContext(databaseDispatcher) {
            check(!closed.get()) { "SeenHistoryController is already closed." }
            store.block()
        }
    }

    private fun <T> runStoreCatching(
        errorMessage: String,
        fallback: T,
        block: suspend SeenHistoryStore.() -> T
    ): T {
        return try {
            runStore(block)
        } catch (ex: SQLException) {
            logger.error(errorMessage, ex)
            fallback
        }
    }

    private fun sendChangeMessage() {
        MessageBus.messageBus.publishAsync(DownloadHistoryChangedEvent())
    }

    companion object {
        private val logger = LogManager.getLogger()
        private const val MAX_DAYS: Long = 30
        private val databaseDispatcher: CoroutineDispatcher = Dispatchers.IO.limitedParallelism(1)
        private val sharedStoreLock = Any()
        @Volatile
        private var sharedStore: SeenHistoryStore? = null

        private fun sharedStore(): SeenHistoryStore {
            sharedStore?.let { return it }

            val createdStore = createStore()

            synchronized(sharedStoreLock) {
                sharedStore?.let { existingStore ->
                    runCatching { createdStore.close() }
                        .onFailure { logger.warn("Failed to close redundant seen history store", it) }
                    return existingStore
                }

                sharedStore = createdStore
                return createdStore
            }
        }

        private fun createStore(): SeenHistoryStore {
            return try {
                openStore(SqlDatabaseConfig.historyDbPath)
            } catch (ex: SeenHistoryCorruptionHandler.CorruptSeenHistoryDatabaseException) {
                SeenHistoryCorruptionHandler.resolveCorruption(ex.dbPath) { databasePath ->
                    openStore(databasePath)
                }
            }
        }

        private fun openStore(databasePath: Path): SeenHistoryStore {
            return SeenHistoryCorruptionHandler.openStoreOrThrowCorrupt(databasePath) { resolvedPath ->
                SeenHistoryStore(SqlDatabaseConfig.createDataSource(resolvedPath), resolvedPath)
            }
        }

        fun closeSharedStore() {
            runBlocking {
                withContext(databaseDispatcher) {
                    val storeToClose = synchronized(sharedStoreLock) {
                        sharedStore.also { sharedStore = null }
                    }
                    runCatching {
                        storeToClose?.close()
                    }.onFailure {
                        logger.warn("Failed to close shared seen history store", it)
                    }
                }
            }
        }

        fun prepareSharedMemoryCache() {
            SeenHistoryController().use { it.prepareMemoryCache() }
        }

        fun hasBeenSeenFromSharedCache(film: DatenFilm): Boolean {
            if (!SeenHistoryCache.isPrepared()) {
                prepareSharedMemoryCache()
            }
            return SeenHistoryCache.contains(film.urlNormalQuality)
        }

    }
}

internal data class SeenHistoryEntry(
    val theme: String,
    val title: String,
    val url: String
)

private fun DatenFilm.toSeenHistoryEntry(): SeenHistoryEntry? {
    if (isLivestream) {
        return null
    }

    val url = urlNormalQuality.takeIf(String::isNotBlank) ?: return null
    return SeenHistoryEntry(
        theme = thema,
        title = title,
        url = url
    )
}

private fun AudioEntry.toSeenHistoryEntry(): SeenHistoryEntry? {
    val url = audioUrl?.toString()?.takeIf(String::isNotBlank) ?: return null
    return SeenHistoryEntry(
        theme = theme,
        title = title,
        url = url
    )
}

private object SeenHistoryCache {
    private val lock = Any()
    private val urlCache = java.util.concurrent.ConcurrentHashMap.newKeySet<String>()
    @Volatile
    private var prepared = false

    fun isPrepared(): Boolean = prepared

    fun size(): Int = urlCache.size

    fun contains(url: String): Boolean = prepared && url.isNotBlank() && urlCache.contains(url)

    fun load(urls: Set<String>) {
        synchronized(lock) {
            if (prepared) {
                return
            }
            urlCache.clear()
            urlCache.addAll(urls)
            prepared = true
        }
    }

    fun add(url: String) {
        if (url.isBlank()) {
            return
        }
        add(listOf(url))
    }

    fun add(urls: Collection<String>) {
        if (!prepared || urls.isEmpty()) {
            return
        }
        synchronized(lock) {
            if (prepared) {
                urls.asSequence().filter(String::isNotBlank).forEach(urlCache::add)
            }
        }
    }

    fun remove(url: String) {
        if (url.isBlank()) {
            return
        }
        remove(listOf(url))
    }

    fun remove(urls: Collection<String>) {
        if (!prepared || urls.isEmpty()) {
            return
        }
        synchronized(lock) {
            if (prepared) {
                urls.asSequence().filter(String::isNotBlank).forEach(urlCache::remove)
            }
        }
    }

    fun clear() {
        synchronized(lock) {
            urlCache.clear()
            prepared = false
        }
    }
}
