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

package mediathek.daten

import org.apache.lucene.store.*
import java.nio.file.Path
import java.util.*

enum class LuceneDirectoryMode(
    val configValue: String,
    val description: String,
) {
    AUTO(
        "auto",
        "auto: Lucene wählt automatisch die passende Directory-Implementierung für das Betriebssystem.",
    ),
    MMAP(
        "mmap",
        "mmap: Oft schnell bei Suchzugriffen, kann aber auf einigen Systemen Dateihandling beim Neuaufbau erschweren.",
    ),
    NIOFS(
        "niofs",
        "niofs: Robustes Datei-I/O ohne Memory Mapping, meist stabiler beim Austausch/Löschen von Indexdateien.",
    ),
    IN_MEMORY(
        "in-memory",
        "in-memory: Beste Performance, aber höherer Speicherverbrauch.",
    );

    @Throws(Exception::class)
    fun createDirectory(indexPath: Path): Directory = when (this) {
        AUTO -> FSDirectory.open(indexPath)
        MMAP -> MMapDirectory(indexPath)
        NIOFS -> NIOFSDirectory(indexPath)
        IN_MEMORY -> ByteBuffersDirectory()
    }

    override fun toString(): String = configValue

    companion object {
        fun fromConfigValue(value: String?): LuceneDirectoryMode =
            fromConfigValueOrNull(value) ?: AUTO

        fun fromConfigValueOrNull(value: String?): LuceneDirectoryMode? {
            val normalizedValue = value
                ?.trim()
                ?.lowercase(Locale.ROOT)
                ?: return null

            return entries.firstOrNull { it.configValue == normalizedValue }
        }
    }
}
