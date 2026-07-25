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

import kotlinx.serialization.EncodeDefault
import kotlinx.serialization.ExperimentalSerializationApi
import kotlinx.serialization.Serializable
import kotlinx.serialization.json.Json
import mediathek.tool.FileUtils
import java.nio.file.Files
import java.nio.file.Path
import kotlin.io.path.createDirectories
import kotlin.io.path.deleteIfExists
import kotlin.io.path.exists
import kotlin.io.path.readText
import kotlin.io.path.writeText

internal const val WATCHLIST_FILE_VERSION: Int = 1

/** Thrown when a watchlist file was written by an incompatible, newer version. */
internal class UnsupportedWatchlistVersionException(version: Int) :
    IllegalArgumentException("Unsupported watchlist file version $version")

/** Storage boundary of the watchlist; kept narrow so failure modes can be tested. */
internal interface WatchlistPersistence {
    /**
     * @throws UnsupportedWatchlistVersionException if the file version is not supported.
     * @throws Exception if the file exists but cannot be read or parsed.
     */
    fun read(storagePath: Path): WatchlistSnapshot

    fun write(storagePath: Path, snapshot: WatchlistSnapshot)

    /** Moves an unreadable file aside and returns the new location. */
    fun quarantine(storagePath: Path): Path
}

internal data class WatchlistSnapshot(
    val entries: List<DatenWatchlistEntry> = emptyList(),
    val notifications: List<WatchlistNotification> = emptyList(),
    val hasUnseenNotifications: Boolean = false,
)

internal object WatchlistStorage : WatchlistPersistence {
    private val json = Json {
        ignoreUnknownKeys = true
        prettyPrint = true
        encodeDefaults = false
        explicitNulls = false
    }

    override fun read(storagePath: Path): WatchlistSnapshot {
        if (!storagePath.exists()) {
            return WatchlistSnapshot()
        }

        val file = json.decodeFromString<WatchlistFileDto>(storagePath.readText())
        if (file.version != WATCHLIST_FILE_VERSION) {
            throw UnsupportedWatchlistVersionException(file.version)
        }
        return file.toSnapshot()
    }

    override fun write(storagePath: Path, snapshot: WatchlistSnapshot) {
        val directory = storagePath.toAbsolutePath().parent
        directory.createDirectories()
        // A unique temporary file keeps concurrent writers from clobbering each other.
        val temporaryPath = Files.createTempFile(directory, storagePath.fileName.toString() + ".", ".tmp")
        try {
            temporaryPath.writeText(json.encodeToString(WatchlistFileDto.fromSnapshot(snapshot)))
            FileUtils.moveAtomicallyWithFallback(temporaryPath, storagePath)
        } finally {
            temporaryPath.deleteIfExists()
        }
    }

    override fun quarantine(storagePath: Path): Path {
        val baseName = storagePath.fileName.toString() + ".corrupt"
        var target = storagePath.resolveSibling(baseName)
        var suffix = 1
        while (target.exists()) {
            target = storagePath.resolveSibling("$baseName.$suffix")
            suffix++
        }
        FileUtils.moveAtomicallyWithFallback(storagePath, target)
        return target
    }
}

@Serializable
private data class WatchlistFileDto(
    @OptIn(ExperimentalSerializationApi::class)
    @EncodeDefault
    val version: Int = WATCHLIST_FILE_VERSION,
    val hasUnseenNotifications: Boolean = false,
    val entries: List<WatchlistEntryDto> = emptyList(),
    val notifications: List<WatchlistNotificationDto> = emptyList(),
) {
    fun toSnapshot(): WatchlistSnapshot =
        WatchlistSnapshot(
            entries = entries.map { dto -> dto.toEntry() },
            notifications = notifications.map { dto -> dto.toNotification() },
            hasUnseenNotifications = hasUnseenNotifications,
        )

    companion object {
        fun fromSnapshot(snapshot: WatchlistSnapshot): WatchlistFileDto =
            WatchlistFileDto(
                hasUnseenNotifications = snapshot.hasUnseenNotifications,
                entries = snapshot.entries.map { entry -> WatchlistEntryDto.fromEntry(entry) },
                notifications = snapshot.notifications.map { notification ->
                    WatchlistNotificationDto.fromNotification(notification)
                },
            )
    }
}

@Serializable
private data class WatchlistEntryDto(
    val id: String,
    val name: String = "",
    val sender: String = "",
    val thema: String = "",
    val title: String = "",
    val seenFilmIds: Set<String> = emptySet(),
) {
    fun toEntry(): DatenWatchlistEntry =
        DatenWatchlistEntry(
            id = id,
            name = name,
            sender = sender,
            thema = thema,
            title = title,
            seenFilmIds = seenFilmIds,
        )

    companion object {
        fun fromEntry(entry: DatenWatchlistEntry): WatchlistEntryDto =
            WatchlistEntryDto(
                id = entry.id,
                name = entry.name,
                sender = entry.sender,
                thema = entry.thema,
                title = entry.title,
                seenFilmIds = entry.seenFilmIds,
            )
    }
}

@Serializable
private data class WatchlistNotificationDto(
    val entryId: String,
    val entryName: String = "",
    val filmId: String = "",
    val sender: String = "",
    val thema: String = "",
    val title: String = "",
    val sendeDatum: String = "",
    val urlNormalQuality: String = "",
) {
    fun toNotification(): WatchlistNotification =
        WatchlistNotification(
            entryId = entryId,
            entryName = entryName,
            filmId = filmId,
            sender = sender,
            thema = thema,
            title = title,
            sendeDatum = sendeDatum,
            urlNormalQuality = urlNormalQuality,
        )

    companion object {
        fun fromNotification(notification: WatchlistNotification): WatchlistNotificationDto =
            WatchlistNotificationDto(
                entryId = notification.entryId,
                entryName = notification.entryName,
                filmId = notification.filmId,
                sender = notification.sender,
                thema = notification.thema,
                title = notification.title,
                sendeDatum = notification.sendeDatum,
                urlNormalQuality = notification.urlNormalQuality,
            )
    }
}
