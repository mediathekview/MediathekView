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
import java.util.concurrent.atomic.AtomicLong

/**
 * Store download progress sizes in bytes and render them as megabyte text.
 */
class DownloadSizeState : Comparable<DownloadSizeState> {
    private val sizeBytes = AtomicLong(0L)
    private val activeSizeBytes = AtomicLong(-1L)

    var size: Long
        get() = sizeBytes.get()
        set(value) {
            sizeBytes.set(value)
        }

    var aktSize: Long
        get() = activeSizeBytes.get()
        set(value) {
            activeSizeBytes.set(value)
        }

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
        activeSizeBytes.addAndGet(size)
    }

    private fun prepareString(): String {
        val currentSize = size
        val currentActiveSize = aktSize
        return when {
            currentActiveSize <= 0 && currentSize > 0 -> FileSize.convertSize(currentSize)
            currentActiveSize <= 0 -> ""
            currentSize > 0 -> "${FileSize.convertSize(currentActiveSize)} von ${FileSize.convertSize(currentSize)}"
            else -> FileSize.convertSize(currentActiveSize)
        }
    }

    private companion object {
        private val logger = LogManager.getLogger()
    }
}
