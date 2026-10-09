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

package mediathek.filmlisten

import mediathek.config.StandardLocations
import org.apache.logging.log4j.LogManager
import java.io.InputStreamReader
import java.io.OutputStreamWriter
import java.nio.charset.StandardCharsets
import java.nio.file.Files
import java.nio.file.Path
import java.util.*

object FilmListMetadataStore {
    private const val COMMENT = "MediathekView filmlist metadata"
    private const val ETAG_PREFIX = "etag."
    private val logger = LogManager.getLogger()

    fun readEtag(sourceUrl: String): String? {
        if (sourceUrl.isBlank()) {
            return null
        }
        return loadProperties().getProperty(toEtagKey(sourceUrl))?.takeIf { it.isNotBlank() }
    }

    fun writeEtag(sourceUrl: String, eTag: String?) {
        if (sourceUrl.isBlank()) {
            return
        }

        val properties = loadProperties()
        val key = toEtagKey(sourceUrl)
        if (eTag.isNullOrBlank()) {
            properties.remove(key)
        } else {
            properties.setProperty(key, eTag)
        }
        saveProperties(properties)
    }

    private fun loadProperties(): Properties {
        val properties = Properties()
        val path = metadataPath()
        if (!Files.exists(path)) {
            return properties
        }

        try {
            Files.newInputStream(path).use { input ->
                InputStreamReader(input, StandardCharsets.UTF_8).use { reader ->
                    properties.load(reader)
                }
            }
        } catch (ex: Exception) {
            logger.warn("Failed to read filmlist metadata from {}", path, ex)
        }
        return properties
    }

    private fun saveProperties(properties: Properties) {
        val path = metadataPath()
        try {
            if (properties.isEmpty) {
                Files.deleteIfExists(path)
                return
            }

            path.parent?.let(Files::createDirectories)
            Files.newOutputStream(path).use { output ->
                OutputStreamWriter(output, StandardCharsets.UTF_8).use { writer ->
                    properties.store(writer, COMMENT)
                }
            }
        } catch (ex: Exception) {
            logger.warn("Failed to write filmlist metadata to {}", path, ex)
        }
    }

    private fun metadataPath(): Path {
        return StandardLocations.getFilmlistMetadataFilePath()
    }

    private fun toEtagKey(sourceUrl: String): String = ETAG_PREFIX + sourceUrl
}
