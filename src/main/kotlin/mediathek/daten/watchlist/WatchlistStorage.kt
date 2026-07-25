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
import org.apache.logging.log4j.LogManager
import java.nio.file.Path
import kotlin.io.path.createDirectories
import kotlin.io.path.deleteIfExists
import kotlin.io.path.exists
import kotlin.io.path.readText
import kotlin.io.path.writeText

object WatchlistStorage {
    private const val FILE_VERSION = 1
    private val logger = LogManager.getLogger(WatchlistStorage::class.java)
    private val json = Json {
        ignoreUnknownKeys = true
        prettyPrint = true
        encodeDefaults = false
        explicitNulls = false
    }

    fun read(storagePath: Path): Snapshot {
        if (!storagePath.exists()) {
            return Snapshot()
        }

        return try {
            json.decodeFromString<WatchlistFileDto>(storagePath.readText()).toSnapshot()
        } catch (ex: Exception) {
            logger.error("Failed to read watchlist from {}", storagePath, ex)
            Snapshot()
        }
    }

    fun write(storagePath: Path, snapshot: Snapshot) {
        storagePath.parent?.createDirectories()
        val temporaryPath = storagePath.resolveSibling(storagePath.fileName.toString() + ".tmp")
        try {
            val file = WatchlistFileDto.fromSnapshot(snapshot)
            temporaryPath.writeText(json.encodeToString(file))
            FileUtils.moveAtomicallyWithFallback(temporaryPath, storagePath)
        } finally {
            temporaryPath.deleteIfExists()
        }
    }

    data class Snapshot(
        val entries: List<DatenWatchlistEntry> = emptyList(),
        val notifications: List<WatchlistNotification> = emptyList(),
        val hasUnseenNotifications: Boolean = false,
    )
}

@Serializable
private data class WatchlistFileDto(
    @OptIn(ExperimentalSerializationApi::class)
    @EncodeDefault
    val version: Int = 1,
    val hasUnseenNotifications: Boolean = false,
    val entries: List<WatchlistEntryDto> = emptyList(),
    val notifications: List<WatchlistNotificationDto> = emptyList(),
) {
    fun toSnapshot(): WatchlistStorage.Snapshot =
        WatchlistStorage.Snapshot(
            entries = entries.map { dto -> dto.toEntry() },
            notifications = notifications.map { dto -> dto.toNotification() },
            hasUnseenNotifications = hasUnseenNotifications,
        )

    companion object {
        fun fromSnapshot(snapshot: WatchlistStorage.Snapshot): WatchlistFileDto =
            WatchlistFileDto(
                hasUnseenNotifications = snapshot.hasUnseenNotifications,
                entries = snapshot.entries.map { entry -> WatchlistEntryDto.fromEntry(entry) },
                notifications = snapshot.notifications.map { n -> WatchlistNotificationDto.fromNotification(n) },
            )
    }
}

@Serializable
private data class WatchlistEntryDto(
    val id: String = "",
    val name: String = "",
    val sender: String = "",
    val thema: String = "",
    val title: String = "",
    val seenUrlKeys: Set<String> = emptySet(),
) {
    fun toEntry(): DatenWatchlistEntry =
        DatenWatchlistEntry().apply {
            if (this@WatchlistEntryDto.id.isNotEmpty()) {
                id = this@WatchlistEntryDto.id
            }
            name = this@WatchlistEntryDto.name
            sender = this@WatchlistEntryDto.sender
            thema = this@WatchlistEntryDto.thema
            title = this@WatchlistEntryDto.title
            seenUrlKeys.addAll(this@WatchlistEntryDto.seenUrlKeys)
        }

    companion object {
        fun fromEntry(entry: DatenWatchlistEntry): WatchlistEntryDto =
            WatchlistEntryDto(
                id = entry.id,
                name = entry.name,
                sender = entry.sender,
                thema = entry.thema,
                title = entry.title,
                seenUrlKeys = entry.seenUrlKeys.toSet(),
            )
    }
}

@Serializable
private data class WatchlistNotificationDto(
    val entryId: String = "",
    val entryName: String = "",
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
                sender = notification.sender,
                thema = notification.thema,
                title = notification.title,
                sendeDatum = notification.sendeDatum,
                urlNormalQuality = notification.urlNormalQuality,
            )
    }
}
