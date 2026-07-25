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

package mediathek.daten.watchlist

import kotlinx.coroutines.*
import kotlinx.coroutines.sync.Mutex
import kotlinx.coroutines.sync.withLock
import mediathek.config.StandardLocations
import mediathek.daten.DatenFilm
import mediathek.daten.ListeFilme
import mediathek.gui.messages.FilmListReadStopEvent
import mediathek.gui.messages.WatchlistChangedEvent
import mediathek.tool.MessageBus
import mediathek.tool.notification.MessageType
import mediathek.tool.notification.NotificationMessage
import mediathek.tool.notification.NotificationPublisher
import net.engio.mbassy.listener.Handler
import org.apache.logging.log4j.LogManager
import java.nio.file.Path
import java.util.concurrent.atomic.AtomicBoolean
import kotlin.io.path.exists

/**
 * Owns the watchlist entries and the pending "new episode" notifications.
 *
 * All mutations and all disk writes are serialized through a single operation mutex and
 * always run off the EDT. Matching happens after each film list read stop: films flagged
 * [DatenFilm.isNew] are matched against every entry and deduplicated via the stable film
 * identity. The red badge state ([hasUnseenNotifications]) is tracked separately from the
 * pending notification list and is cleared by [acknowledgeNotifications].
 */
class WatchlistServices internal constructor(
    private val allFilms: ListeFilme,
    private val notificationPublisher: NotificationPublisher,
    private val storagePath: Path = StandardLocations.getWatchlistFilePath(),
    private val persistence: WatchlistPersistence = WatchlistStorage,
) : AutoCloseable {
    constructor(
        allFilms: ListeFilme,
        notificationPublisher: NotificationPublisher,
    ) : this(allFilms, notificationPublisher, StandardLocations.getWatchlistFilePath(), WatchlistStorage)

    private val stateLock = Any()
    private val entries = mutableListOf<DatenWatchlistEntry>()
    private val pendingNotifications = mutableListOf<WatchlistNotification>()
    private var unseenNotifications = false

    /** Set whenever in-memory state diverges from disk; a failed write keeps it set for retry. */
    private var dirty = false

    /** Disabled when the on-disk file must not be overwritten, e.g. a newer file version. */
    private var writesEnabled = true

    private val scope = CoroutineScope(SupervisorJob() + Dispatchers.IO)
    private val operationMutex = Mutex()
    private val acceptingOperations = AtomicBoolean(true)

    init {
        MessageBus.messageBus.subscribe(this)
    }

    val hasUnseenNotifications: Boolean
        get() = synchronized(stateLock) { unseenNotifications }

    fun entriesSnapshot(): List<DatenWatchlistEntry> = synchronized(stateLock) { entries.toList() }

    fun notificationsSnapshot(): List<WatchlistNotification> =
        synchronized(stateLock) { pendingNotifications.toList() }

    fun loadFromFile() {
        val snapshot = try {
            persistence.read(storagePath)
        } catch (exception: UnsupportedWatchlistVersionException) {
            synchronized(stateLock) { writesEnabled = false }
            logger.error("Watchlist file {} has an unsupported version; keeping it untouched", storagePath, exception)
            return
        } catch (exception: Exception) {
            if (!quarantineUnreadableFile(exception)) {
                synchronized(stateLock) { writesEnabled = false }
                return
            }
            WatchlistSnapshot()
        }

        synchronized(stateLock) {
            entries.clear()
            entries.addAll(snapshot.entries)
            pendingNotifications.clear()
            pendingNotifications.addAll(snapshot.notifications)
            // Never restore a badge without anything to show.
            unseenNotifications = snapshot.hasUnseenNotifications && snapshot.notifications.isNotEmpty()
            dirty = false
        }
    }

    /**
     * Adds an entry for the show of the given film. All currently matching episodes are
     * recorded as seen so only future episodes are reported. Safe to call from the EDT;
     * completion is signalled via [WatchlistChangedEvent].
     */
    fun addEntryFromFilm(film: DatenFilm, withTitle: Boolean): Job =
        launchOperation("add watchlist entry") { performAddEntry(film, withTitle) }

    fun findEntryFor(film: DatenFilm, withTitle: Boolean): DatenWatchlistEntry? =
        synchronized(stateLock) {
            entries.firstOrNull { entry ->
                entry.sender.equals(film.sender, ignoreCase = true) &&
                    entry.thema.equals(film.thema, ignoreCase = true) &&
                    if (withTitle) entry.title.equals(film.title, ignoreCase = true) else entry.title.isEmpty()
            }
        }

    /** Removes the entry and all notifications belonging to it. Safe to call from the EDT. */
    fun removeEntry(entryId: String): Job =
        launchOperation("remove watchlist entry") { performRemoveEntry(entryId) }

    /** Removes a single notification. Safe to call from the EDT. */
    fun removeNotification(notification: WatchlistNotification): Job =
        launchOperation("remove watchlist notification") { performRemoveNotification(notification) }

    /**
     * Clears the badge and returns exactly the notifications covered by that
     * acknowledgement, so callers can never acknowledge something they did not display.
     * Pending notifications stay until the user removes them.
     */
    suspend fun acknowledgeNotifications(): List<WatchlistNotification> =
        withContext(Dispatchers.IO) {
            operationMutex.withLock {
                val acknowledged = synchronized(stateLock) {
                    if (unseenNotifications) {
                        unseenNotifications = false
                        dirty = true
                    }
                    pendingNotifications.toList()
                }
                publishChanged()
                persistIfDirty()
                acknowledged
            }
        }

    @Handler
    @Suppress("UNUSED_PARAMETER")
    fun handleFilmListReadStopEvent(event: FilmListReadStopEvent) {
        launchOperation("match new watchlist episodes") { performMatchNewEpisodes() }
    }

    override fun close() {
        if (!acceptingOperations.compareAndSet(true, false)) {
            return
        }
        MessageBus.messageBus.unsubscribe(this)
        try {
            runBlocking {
                awaitIdle()
                withContext(Dispatchers.IO) {
                    operationMutex.withLock { persistIfDirty() }
                }
            }
        } catch (exception: Exception) {
            logger.error("Failed to flush watchlist during shutdown", exception)
        } finally {
            scope.cancel()
        }
    }

    internal suspend fun addEntryFromFilmAndWait(film: DatenFilm, withTitle: Boolean) =
        runOperationAndWait { performAddEntry(film, withTitle) }

    internal suspend fun removeEntryAndWait(entryId: String) =
        runOperationAndWait { performRemoveEntry(entryId) }

    internal suspend fun removeNotificationAndWait(notification: WatchlistNotification) =
        runOperationAndWait { performRemoveNotification(notification) }

    internal suspend fun matchNewEpisodesAndWait() = runOperationAndWait { performMatchNewEpisodes() }

    /** Waits until all asynchronous watchlist operations have finished. */
    internal suspend fun awaitIdle() {
        while (true) {
            val children = scope.coroutineContext[Job]?.children?.toList().orEmpty()
            if (children.isEmpty()) {
                return
            }
            children.joinAll()
        }
    }

    private suspend fun runOperationAndWait(operation: suspend () -> Unit) {
        withContext(Dispatchers.IO) {
            operationMutex.withLock { operation() }
        }
    }

    private fun performAddEntry(film: DatenFilm, withTitle: Boolean) {
        val titleFilter = if (withTitle) film.title else ""
        val draft = DatenWatchlistEntry(
            name = film.thema,
            sender = film.sender,
            thema = film.thema,
            title = titleFilter,
        )
        val seenFilmIds = allFilms.snapshot()
            .asSequence()
            .filter(draft::matches)
            .map(DatenFilm::sha256)
            .toSet()

        val added = synchronized(stateLock) {
            if (entries.any { entry -> entry.hasSameCriteriaAs(draft) }) {
                false
            } else {
                entries.add(draft.copy(seenFilmIds = seenFilmIds))
                dirty = true
                true
            }
        }
        if (added) {
            publishChanged()
            persistIfDirty()
        }
    }

    private fun performRemoveEntry(entryId: String) {
        val removed = synchronized(stateLock) {
            val entryRemoved = entries.removeIf { entry -> entry.id == entryId }
            if (entryRemoved) {
                pendingNotifications.removeIf { notification -> notification.entryId == entryId }
                clearBadgeWithoutNotifications()
                dirty = true
            }
            entryRemoved
        }
        if (removed) {
            publishChanged()
            persistIfDirty()
        }
    }

    private fun performRemoveNotification(notification: WatchlistNotification) {
        val removed = synchronized(stateLock) {
            val notificationRemoved = pendingNotifications.remove(notification)
            if (notificationRemoved) {
                clearBadgeWithoutNotifications()
                dirty = true
            }
            notificationRemoved
        }
        if (removed) {
            publishChanged()
            persistIfDirty()
        }
    }

    private fun performMatchNewEpisodes() {
        val scanEntries = entriesSnapshot()
        if (scanEntries.isEmpty()) {
            return
        }

        val knownFilmIds = notificationsSnapshot().mapTo(HashSet()) { notification -> notification.filmId }
        val seenAdditions = mutableMapOf<String, MutableSet<String>>()
        val newNotifications = mutableListOf<WatchlistNotification>()

        for (film in allFilms.snapshot()) {
            if (!film.isNew) {
                continue
            }
            val filmId = film.sha256
            val matchingEntries = scanEntries.filter { entry ->
                filmId !in entry.seenFilmIds && entry.matches(film)
            }
            if (matchingEntries.isEmpty()) {
                continue
            }

            matchingEntries.forEach { entry -> seenAdditions.getOrPut(entry.id) { mutableSetOf() }.add(filmId) }
            // Overlapping entries must not produce duplicate rows for the same episode.
            if (knownFilmIds.add(filmId)) {
                val owningEntry = matchingEntries.firstOrNull { entry -> entry.title.isNotEmpty() }
                    ?: matchingEntries.first()
                newNotifications.add(
                    WatchlistNotification(
                        entryId = owningEntry.id,
                        entryName = owningEntry.name,
                        filmId = filmId,
                        sender = film.sender,
                        thema = film.thema,
                        title = film.title,
                        sendeDatum = film.sendeDatum,
                        urlNormalQuality = film.urlNormalQuality,
                    )
                )
            }
        }
        if (seenAdditions.isEmpty()) {
            return
        }

        synchronized(stateLock) {
            entries.replaceAll { entry ->
                val additions = seenAdditions[entry.id]
                if (additions == null) entry else entry.copy(seenFilmIds = entry.seenFilmIds + additions)
            }
            if (newNotifications.isNotEmpty()) {
                pendingNotifications.addAll(newNotifications)
                unseenNotifications = true
            }
            dirty = true
        }
        publishChanged()
        persistIfDirty()
        if (newNotifications.isNotEmpty()) {
            publishOsSummary(newNotifications)
        }
    }

    private fun clearBadgeWithoutNotifications() {
        if (pendingNotifications.isEmpty()) {
            unseenNotifications = false
        }
    }

    private fun launchOperation(description: String, operation: suspend () -> Unit): Job {
        if (!acceptingOperations.get()) {
            return Job().apply { complete() }
        }

        return scope.launch {
            operationMutex.withLock {
                try {
                    operation()
                } catch (exception: CancellationException) {
                    throw exception
                } catch (exception: Exception) {
                    logger.error("Failed to {}", description, exception)
                }
            }
        }
    }

    private fun publishOsSummary(newNotifications: List<WatchlistNotification>) {
        val names = newNotifications.mapTo(LinkedHashSet()) { notification -> notification.entryName }
        val message = if (newNotifications.size == 1) {
            "Neue Folge eingetroffen für „${names.first()}“."
        } else {
            "${newNotifications.size} neue Folgen eingetroffen für: ${names.joinToString(", ")}"
        }
        notificationPublisher.publish(NotificationMessage(OS_NOTIFICATION_TITLE, message, MessageType.INFO))
    }

    private fun publishChanged() {
        MessageBus.messageBus.publishAsync(WatchlistChangedEvent())
    }

    private fun persistIfDirty() {
        val snapshot = synchronized(stateLock) {
            if (!dirty || !writesEnabled) {
                return
            }
            WatchlistSnapshot(
                entries = entries.toList(),
                notifications = pendingNotifications.toList(),
                hasUnseenNotifications = unseenNotifications,
            )
        }

        try {
            persistence.write(storagePath, snapshot)
            synchronized(stateLock) { dirty = false }
        } catch (exception: Exception) {
            // Keep the dirty flag so the next operation or the shutdown flush retries.
            logger.error("Failed to write watchlist to {}; will retry later", storagePath, exception)
        }
    }

    private fun quarantineUnreadableFile(readFailure: Exception): Boolean {
        if (!storagePath.exists()) {
            logger.error("Failed to read watchlist from {}", storagePath, readFailure)
            return false
        }

        return try {
            val quarantinedPath = persistence.quarantine(storagePath)
            logger.error("Moved unreadable watchlist {} aside to {}", storagePath, quarantinedPath, readFailure)
            true
        } catch (quarantineFailure: Exception) {
            quarantineFailure.addSuppressed(readFailure)
            logger.error("Cannot read or preserve watchlist {}; disabling writes", storagePath, quarantineFailure)
            false
        }
    }

    private companion object {
        private const val OS_NOTIFICATION_TITLE = "MediathekView Watchlist"
        private val logger = LogManager.getLogger(WatchlistServices::class.java)
    }
}
