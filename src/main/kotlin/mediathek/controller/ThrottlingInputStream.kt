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

package mediathek.controller

import java.io.IOException
import java.io.InputStream

/**
 * InputStream which limits reads based on a [ByteRateLimiter].
 */
class ThrottlingInputStream(
    private val target: InputStream,
    private val maxBytesPerSecond: ByteRateLimiter
) : InputStream() {

    @Throws(IOException::class)
    override fun read(): Int {
        val startedAtNanos = System.nanoTime()
        val value = target.read()
        if (value != -1) {
            maxBytesPerSecond.acquire(1, startedAtNanos, System.nanoTime())
        }
        return value
    }

    @Throws(IOException::class)
    override fun read(b: ByteArray): Int {
        val startedAtNanos = System.nanoTime()
        val bytesRead = target.read(b)
        if (bytesRead > 0) {
            maxBytesPerSecond.acquire(bytesRead, startedAtNanos, System.nanoTime())
        }
        return bytesRead
    }

    @Throws(IOException::class)
    override fun read(b: ByteArray, off: Int, len: Int): Int {
        val startedAtNanos = System.nanoTime()
        val bytesRead = target.read(b, off, len)
        if (bytesRead > 0) {
            maxBytesPerSecond.acquire(bytesRead, startedAtNanos, System.nanoTime())
        }
        return bytesRead
    }

    @Throws(IOException::class)
    override fun skip(n: Long): Long = target.skip(n)

    @Throws(IOException::class)
    override fun available(): Int = target.available()

    @Synchronized
    override fun mark(readlimit: Int) {
        target.mark(readlimit)
    }

    @Synchronized
    @Throws(IOException::class)
    override fun reset() {
        target.reset()
    }

    override fun markSupported(): Boolean = target.markSupported()

    @Throws(IOException::class)
    override fun close() {
        target.close()
    }
}
