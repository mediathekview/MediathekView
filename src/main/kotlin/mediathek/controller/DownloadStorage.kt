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

package mediathek.controller

import kotlinx.serialization.Serializable
import kotlinx.serialization.encodeToString
import kotlinx.serialization.json.Json
import mediathek.daten.DatenDownload
import mediathek.daten.DownloadConfig
import mediathek.tool.FileUtils
import java.nio.file.Path
import kotlin.io.path.createDirectories
import kotlin.io.path.deleteIfExists
import kotlin.io.path.exists
import kotlin.io.path.readText
import kotlin.io.path.writeText

object DownloadStorage {
    private const val FILE_VERSION = 1
    private val json = Json {
        ignoreUnknownKeys = true
        prettyPrint = true
        encodeDefaults = false
    }

    fun read(storagePath: Path): List<DatenDownload> {
        if (!storagePath.exists()) {
            return emptyList()
        }

        return json.decodeFromString<DownloadFileDto>(storagePath.readText())
            .downloads
            .map(DatenDownload::fromConfig)
    }

    fun write(storagePath: Path, downloads: Iterable<DatenDownload>) {
        storagePath.parent?.createDirectories()
        val temporaryPath = storagePath.resolveSibling(storagePath.fileName.toString() + ".tmp")
        try {
            val file = DownloadFileDto(
                version = FILE_VERSION,
                downloads = downloadsToSave(downloads).map(DatenDownload::toConfig),
            )
            temporaryPath.writeText(json.encodeToString(file))
            FileUtils.moveAtomicallyWithFallback(temporaryPath, storagePath)
        } finally {
            temporaryPath.deleteIfExists()
        }
    }

    private fun downloadsToSave(downloads: Iterable<DatenDownload>): List<DatenDownload> =
        downloads.filter { download ->
            download.isInterrupted || (!download.isFinished && !download.isFromAbo)
        }
}

@Serializable
private data class DownloadFileDto(
    val version: Int = 1,
    val downloads: List<DownloadConfig> = emptyList(),
)
