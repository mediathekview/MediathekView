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

import java.io.FilterInputStream
import java.io.IOException
import java.io.InputStream

class ProgressMonitorInputStream(
    input: InputStream,
    private val size: Long,
    private val monitor: ((bytesRead: Long, size: Long) -> Unit)?,
) : FilterInputStream(input) {

    private var bytesRead = 0L

    init {
        if (size == 0L) {
            throw IOException("Size must be greater than zero!")
        }
    }

    @Throws(IOException::class)
    override fun read(): Int {
        val read = super.read()
        if (read != -1) {
            updateProgress(1)
        }
        return read
    }

    @Throws(IOException::class)
    override fun read(buffer: ByteArray): Int {
        val read = super.read(buffer)
        if (read != -1) {
            updateProgress(read.toLong())
        }
        return read
    }

    @Throws(IOException::class)
    override fun read(buffer: ByteArray, offset: Int, length: Int): Int {
        val read = super.read(buffer, offset, length)
        if (read != -1) {
            updateProgress(read.toLong())
        }
        return read
    }

    private fun updateProgress(readBytes: Long) {
        bytesRead += readBytes
        monitor?.invoke(bytesRead, size)
    }
}
