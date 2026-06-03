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
 * Store download progress sizes in bytes and render them as megabyte text.
 */
class DownloadSizeState : Comparable<DownloadSizeState> {
    var size: Long = 0L

    var aktSize: Long = -1L

    override fun compareTo(other: DownloadSizeState): Int = size.compareTo(other.size)

    override fun toString(): String = prepareString()

    fun reset() {
        aktSize = -1L
    }

    /**
     * Store film size text in megabytes.
     */
    fun setSize(size: String) {
        if (size.isEmpty()) {
            aktSize = -1L
            this.size = 0L
            return
        }

        try {
            this.size = FileSize.megabyteTextToBytes(size)
        } catch (ex: NumberFormatException) {
            logger.error("string: {}, ex: {}", size, ex)
            this.size = 0L
        }
    }

    fun addAktSize(size: Long) {
        aktSize += size
    }

    private fun prepareString(): String =
        when {
            aktSize <= 0 && size > 0 -> FileSize.convertSize(size)
            aktSize <= 0 -> ""
            size > 0 -> "${FileSize.convertSize(aktSize)} von ${FileSize.convertSize(size)}"
            else -> FileSize.convertSize(aktSize)
        }

    private companion object {
        private val logger = LogManager.getLogger()
    }
}
