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

fun getLikelyExtensionDotIndex(fileName: String): Int {
    val lastSeparator = getLastPathSeparator(fileName)
    val suffixEnd = getSuffixEnd(fileName, lastSeparator)
    val lastDot = getLikelyExtensionDotIndex(fileName, suffixEnd, lastSeparator)
    if (lastDot >= 0) {
        return lastDot
    }

    if (suffixEnd < fileName.length && looksLikeUrl(fileName, suffixEnd)) {
        return -1
    }

    return getLikelyExtensionDotIndex(fileName, fileName.length, lastSeparator)
}

private fun getLikelyExtensionDotIndex(fileName: String, suffixEnd: Int, lastSeparator: Int): Int {
    val lastDot = fileName.lastIndexOf('.', suffixEnd - 1)
    if (lastDot < 0 || lastDot <= lastSeparator) {
        return -1
    }

    return if (looksLikeExtension(fileName.substring(lastDot + 1, suffixEnd))) lastDot else -1
}

fun getLastPathSeparator(fileName: String): Int =
    getLastPathSeparator(fileName, fileName.length)

private fun getLastPathSeparator(fileName: String, upperBoundExclusive: Int): Int =
    maxOf(
        fileName.lastIndexOf('/', upperBoundExclusive - 1),
        fileName.lastIndexOf('\\', upperBoundExclusive - 1)
    )

private fun getSuffixEnd(fileName: String, lastSeparator: Int): Int {
    val queryIndex = fileName.indexOf('?', lastSeparator + 1)
    val fragmentIndex = fileName.indexOf('#', lastSeparator + 1)
    var suffixEnd = fileName.length
    if (queryIndex >= 0) {
        suffixEnd = minOf(suffixEnd, queryIndex)
    }
    if (fragmentIndex >= 0) {
        suffixEnd = minOf(suffixEnd, fragmentIndex)
    }
    return suffixEnd
}

private fun looksLikeUrl(fileName: String, delimiterIndex: Int): Boolean {
    val schemeIndex = fileName.indexOf("://")
    return schemeIndex > 0 && schemeIndex < delimiterIndex
}

fun looksLikeExtension(suffix: String): Boolean =
    suffix.isNotEmpty() &&
        suffix.length <= 10 &&
        ' ' !in suffix &&
        '/' !in suffix &&
        '\\' !in suffix
