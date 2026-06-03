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

import org.apache.logging.log4j.LogManager

/**
 * Store film size in megabytes.
 */
class FilmSize : Comparable<FilmSize> {
    private var size = 0

    fun setSize(strSize: String) {
        try {
            size = FileSize.megabyteTextToInt(strSize)
        } catch (ex: NumberFormatException) {
            logger.error("String: {}", strSize, ex)
            size = 0
        }
    }

    fun toInteger(): Int = size

    override fun toString(): String = if (size == 0) "" else size.toString()

    override fun compareTo(other: FilmSize): Int = size.compareTo(other.size)

    private companion object {
        private val logger = LogManager.getLogger()
    }
}
