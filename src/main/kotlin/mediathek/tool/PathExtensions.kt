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

package mediathek.tool

import java.nio.file.Path
import kotlin.io.path.Path
import kotlin.io.path.name

object PathExtensions {
    fun withExtension(path: Path, newExtension: String): Path {
        require(newExtension.isNotBlank()) { "extension must not be blank" }

        val normalizedExtension = if (newExtension.startsWith(".")) newExtension else ".$newExtension"
        val name = path.fileName?.name ?: throw IllegalArgumentException("Path has no filename: $path")
        val lastDot = getLikelyExtensionDotIndex(name)
        val baseName = if (lastDot > 0) name.substring(0, lastDot) else name
        val newName = baseName + normalizedExtension

        return path.parent?.resolve(newName) ?: Path(newName)
    }
}
