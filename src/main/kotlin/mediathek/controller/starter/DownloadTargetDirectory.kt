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

package mediathek.controller.starter

import java.io.IOException
import java.nio.file.Files
import java.nio.file.InvalidPathException
import java.nio.file.Path

@Throws(IOException::class)
internal fun createDownloadTargetDirectory(targetPath: String): Path {
    val directory = try {
        Path.of(targetPath)
    } catch (ex: InvalidPathException) {
        throw directoryCreationException(targetPath, ex)
    }

    try {
        Files.createDirectories(directory)
    } catch (ex: IOException) {
        throw directoryCreationException(directory.toString(), ex)
    } catch (ex: SecurityException) {
        throw directoryCreationException(directory.toString(), ex)
    }

    val isDirectory = try {
        Files.isDirectory(directory)
    } catch (ex: SecurityException) {
        throw directoryCreationException(directory.toString(), ex)
    }
    if (!isDirectory) {
        throw IOException("Download target is not a directory: $directory")
    }

    return directory
}

private fun directoryCreationException(targetPath: String, cause: Exception): IOException {
    val details = cause.localizedMessage?.takeIf { it.isNotBlank() }?.let { ": $it" }.orEmpty()
    return IOException("Failed to create download target directory '$targetPath'$details", cause)
}
