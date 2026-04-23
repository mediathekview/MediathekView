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

package mediathek.audiothek.repository

import java.io.InputStream
import java.nio.file.Files
import java.nio.file.Path
import java.util.*

class AudioDownloadCache(
    private val cacheDir: Path = AudiothekPaths.defaultAudiothekCachePath()
) {
    private val audioFile: Path = cacheDir.resolve("audios.xz")
    private val metadataFile: Path = cacheDir.resolve("cache.properties")

    fun readMetadata(): AudioCacheMetadata? {
        if (!Files.exists(metadataFile)) {
            return null
        }

        val properties = Properties()
        Files.newInputStream(metadataFile).use(properties::load)
        val sourceUrl = properties.getProperty(KEY_SOURCE_URL)?.takeIf(String::isNotBlank) ?: return null
        return AudioCacheMetadata(
            sourceUrl = sourceUrl,
            eTag = properties.getProperty(KEY_ETAG)?.takeIf(String::isNotBlank),
            lastModified = properties.getProperty(KEY_LAST_MODIFIED)?.takeIf(String::isNotBlank)
        )
    }

    fun write(sourceUrl: String, eTag: String?, lastModified: String?, body: ByteArray) {
        Files.createDirectories(cacheDir)
        Files.write(audioFile, body)

        val properties = Properties().apply {
            setProperty(KEY_SOURCE_URL, sourceUrl)
            eTag?.let { setProperty(KEY_ETAG, it) }
            lastModified?.let { setProperty(KEY_LAST_MODIFIED, it) }
        }
        Files.newOutputStream(metadataFile).use { properties.store(it, "Audiothek download cache") }
    }

    fun hasCachedAudio(): Boolean = Files.exists(audioFile)

    fun openCachedAudio(): InputStream = Files.newInputStream(audioFile)

    fun readCachedAudioBytes(): ByteArray? =
        if (Files.exists(audioFile)) Files.readAllBytes(audioFile) else null

    companion object {
        private const val KEY_SOURCE_URL = "sourceUrl"
        private const val KEY_ETAG = "etag"
        private const val KEY_LAST_MODIFIED = "lastModified"
    }
}

data class AudioCacheMetadata(
    val sourceUrl: String,
    val eTag: String?,
    val lastModified: String?
)
