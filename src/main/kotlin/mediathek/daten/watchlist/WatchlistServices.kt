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

/**
 * Owns the watchlist entries and pending "new episode" notifications.
 *
 * Entries and notifications are persisted immediately on every mutation. Matching runs
 * off the EDT after each film list read stop: films flagged [DatenFilm.isNew] are matched
 * against all entries, deduplicated via each entry's seen URL keys, and turned into
 * notifications. The red-badge state ([hasUnseenNotifications]) is tracked separately
 * from the pending notification list and is cleared when the user opens the
 * notification window.
 */
class WatchlistServices(
    private val allFilms: ListeFilme,
    private val notificationPublisher: NotificationPublisher,
    private val storagePath: Path = StandardLocations.getWatchlistFilePath(),
) {
    private val lock = Any()
    private val entries = mutableListOf<DatenWatchlistEntry>()
    private val pendingNotifications = mutableListOf<WatchlistNotification>()
    private var unseenNotifications = false
    private val scope = CoroutineScope(SupervisorJob() + Dispatchers.IO)
    private val matchMutex = Mutex()

    init {
        MessageBus.messageBus.subscribe(this)
    }

    val hasUnseenNotifications: Boolean
        get() = synchronized(lock) { unseenNotifications }

    fun entriesSnapshot(): List<DatenWatchlistEntry> =
        synchronized(lock) { entries.toList() }

    fun notificationsSnapshot(): List<WatchlistNotification> =
        synchronized(lock) { pendingNotifications.toList() }

    fun loadFromFile() {
        val snapshot = WatchlistStorage.read(storagePath)
        synchronized(lock) {
            entries.clear()
            entries.addAll(snapshot.entries)
            pendingNotifications.clear()
            pendingNotifications.addAll(snapshot.notifications)
            unseenNotifications = snapshot.hasUnseenNotifications
        }
    }

    /**
     * Adds an entry for the show of the given film. All currently matching episodes are
     * recorded as seen so only future episodes trigger notifications. Runs asynchronously
     * off the EDT; the result is signalled via [WatchlistChangedEvent].
     */
    fun addEntryFromFilm(film: DatenFilm, withTitle: Boolean) {
        scope.launch {
            try {
                addEntryFromFilmInternal(film, withTitle)
            } catch (ex: Exception) {
                logger.error("Failed to add watchlist entry", ex)
            }
        }
    }

    internal suspend fun addEntryFromFilmInternal(film: DatenFilm, withTitle: Boolean) {
        val draft = DatenWatchlistEntry().apply {
            name = film.thema
            sender = film.sender
            thema = film.thema
            title = if (withTitle) film.title else ""
        }

        val matchingUrlKeys = mutableSetOf<String>()
        for (candidate in allFilms.snapshot()) {
            val urlKey = candidate.storedNormalQualityUrl
            if (urlKey.isNotEmpty() && draft.matches(candidate)) {
                matchingUrlKeys.add(urlKey)
            }
        }

        val added = synchronized(lock) {
            if (entries.any { entry -> sameCriteria(entry, draft) }) {
                false
            } else {
                draft.seenUrlKeys.addAll(matchingUrlKeys)
                entries.add(draft)
                true
            }
        }

        if (added) {
            saveToFile()
            publishChanged()
        }
    }

    fun findEntryFor(film: DatenFilm, withTitle: Boolean): DatenWatchlistEntry? =
        synchronized(lock) {
            entries.firstOrNull { entry ->
                entry.sender.equals(film.sender, ignoreCase = true) &&
                    entry.thema.equals(film.thema, ignoreCase = true) &&
                    if (withTitle) entry.title.equals(film.title, ignoreCase = true) else entry.title.isEmpty()
            }
        }

    fun removeEntry(entry: DatenWatchlistEntry) {
        val removed = synchronized(lock) {
            if (entries.remove(entry)) {
                pendingNotifications.removeIf { notification -> notification.entryId == entry.id }
                true
            } else {
                false
            }
        }

        if (removed) {
            saveToFile()
            publishChanged()
        }
    }

    fun removeNotification(notification: WatchlistNotification) {
        val removed = synchronized(lock) { pendingNotifications.remove(notification) }
        if (removed) {
            saveToFile()
            publishChanged()
        }
    }

    /**
     * Clears the red-badge state. Pending notifications stay untouched until the user
     * explicitly removes them.
     */
    fun markAllSeen() {
        val changed = synchronized(lock) {
            if (unseenNotifications) {
                unseenNotifications = false
                true
            } else {
                false
            }
        }

        if (changed) {
            saveToFile()
            publishChanged()
        }
    }

    @Handler
    @Suppress("UNUSED_PARAMETER")
    fun handleFilmListReadStopEvent(event: FilmListReadStopEvent) {
        scope.launch {
            try {
                matchNewEpisodes()
            } catch (ex: CancellationException) {
                throw ex
            } catch (ex: Exception) {
                logger.error("Watchlist matching failed", ex)
            }
        }
    }

    internal suspend fun matchNewEpisodes() {
        if (entriesSnapshot().isEmpty()) {
            return
        }

        matchMutex.withLock {
            val scanEntries = synchronized(lock) { entries.map { it.copy() } }
            if (scanEntries.isEmpty()) {
                return@withLock
            }

            val candidates = mutableListOf<Pair<DatenWatchlistEntry, DatenFilm>>()
            for (film in allFilms.snapshot()) {
                if (!film.isNew) {
                    continue
                }
                val urlKey = film.storedNormalQualityUrl
                if (urlKey.isEmpty()) {
                    continue
                }
                for (entry in scanEntries) {
                    if (urlKey !in entry.seenUrlKeys && entry.matches(film)) {
                        candidates.add(entry to film)
                    }
                }
            }
            if (candidates.isEmpty()) {
                return@withLock
            }

            val newNotifications = synchronized(lock) {
                buildList {
                    for ((scanEntry, film) in candidates) {
                        val liveEntry = entries.firstOrNull { it.id == scanEntry.id } ?: continue
                        if (!liveEntry.seenUrlKeys.add(film.storedNormalQualityUrl)) {
                            continue
                        }
                        add(
                            WatchlistNotification(
                                entryId = liveEntry.id,
                                entryName = liveEntry.name,
                                sender = film.sender,
                                thema = film.thema,
                                title = film.title,
                                sendeDatum = film.sendeDatum,
                                urlNormalQuality = film.urlNormalQuality,
                            )
                        )
                    }
                }.also { list ->
                    if (list.isNotEmpty()) {
                        pendingNotifications.addAll(list)
                        unseenNotifications = true
                    }
                }
            }

            if (newNotifications.isEmpty()) {
                return@withLock
            }

            saveToFile()
            publishChanged()
            publishOsSummary(newNotifications)
        }
    }

    private fun publishOsSummary(newNotifications: List<WatchlistNotification>) {
        val names = newNotifications.mapTo(LinkedHashSet()) { it.entryName }
        val message = if (names.size == 1) {
            "Neue Folge eingetroffen für „${names.first()}“."
        } else {
            "Neue Folgen eingetroffen für: ${names.joinToString(", ")}"
        }
        notificationPublisher.publish(
            NotificationMessage(OS_NOTIFICATION_TITLE, message, MessageType.INFO)
        )
    }

    private fun saveToFile() {
        val snapshot = synchronized(lock) {
            WatchlistStorage.Snapshot(
                entries = entries.map { it.copy() },
                notifications = pendingNotifications.toList(),
                hasUnseenNotifications = unseenNotifications,
            )
        }
        try {
            WatchlistStorage.write(storagePath, snapshot)
        } catch (ex: Exception) {
            logger.error("Failed to write watchlist to {}", storagePath, ex)
        }
    }

    private fun publishChanged() {
        MessageBus.messageBus.publishAsync(WatchlistChangedEvent())
    }

    private fun sameCriteria(a: DatenWatchlistEntry, b: DatenWatchlistEntry): Boolean =
        a.sender.equals(b.sender, ignoreCase = true) &&
            a.thema.equals(b.thema, ignoreCase = true) &&
            a.title.equals(b.title, ignoreCase = true)

    private companion object {
        private const val OS_NOTIFICATION_TITLE = "MediathekView Watchlist"
        private val logger = LogManager.getLogger(WatchlistServices::class.java)
    }
}
