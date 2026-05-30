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

import mediathek.tool.BandwidthFormatter
import mediathek.tool.FileUtils
import java.io.InputStream
import java.lang.Math.clamp

class MVBandwidthCountingInputStream(
    private val inputStream: InputStream
) : InputStream() {
    private val lock = Any()
    private val startedAtNanos = System.nanoTime()
    private var currentWindowStartNanos = startedAtNanos
    private var totalByteCount = 0L
    private var currentWindowBytesRead = 0L
    private var currentBandwidthBytesPerSecond = 0L

    val currentBandwidth: Long
        get() = synchronized(lock) {
            refreshCurrentBandwidth(System.nanoTime())
            currentBandwidthBytesPerSecond
        }

    val totalBytesRead: Long
        get() = synchronized(lock) {
            totalByteCount
        }

    val averageBandwidth: Long
        get() = synchronized(lock) {
            if (totalByteCount <= 0) {
                return@synchronized 0
            }

            val elapsedNanos = System.nanoTime() - startedAtNanos
            if (elapsedNanos <= 0) {
                return@synchronized 0
            }

            calculateBytesPerSecond(totalByteCount, elapsedNanos)
        }

    override fun close() {
        inputStream.close()
        super.close()
    }

    override fun read(): Int {
        val bytesRead = inputStream.read()
        if (bytesRead != -1) {
            incrementBytesRead(1)
        }

        return bytesRead
    }

    override fun read(buffer: ByteArray): Int = read(buffer, 0, buffer.size)

    override fun read(buffer: ByteArray, offset: Int, length: Int): Int {
        val bytesRead = inputStream.read(buffer, offset, length)
        if (bytesRead != -1) {
            incrementBytesRead(bytesRead)
        }

        return bytesRead
    }

    override fun toString(): String {
        val bytesRead = totalBytesRead
        val bandwidth = averageBandwidth
        val readableBytes = FileUtils.humanReadableByteCountBinary(bytesRead)
        return "Download: Bytes gelesen: $readableBytes  Bandbreite: ${BandwidthFormatter.format(bandwidth)}"
    }

    private fun incrementBytesRead(value: Int) {
        synchronized(lock) {
            val nowNanos = System.nanoTime()
            totalByteCount += value.toLong()
            currentWindowBytesRead += value.toLong()
            refreshCurrentBandwidth(nowNanos)
        }
    }

    private fun refreshCurrentBandwidth(nowNanos: Long) {
        val elapsedNanos = nowNanos - currentWindowStartNanos
        if (elapsedNanos >= NANOS_PER_SECOND) {
            currentBandwidthBytesPerSecond = if (currentWindowBytesRead > 0) {
                calculateBytesPerSecond(currentWindowBytesRead, elapsedNanos)
            } else {
                0
            }

            currentWindowBytesRead = 0
            currentWindowStartNanos = nowNanos
        }
    }

    companion object {
        private const val NANOS_PER_SECOND = 1_000_000_000L

        internal fun calculateBytesPerSecond(bytesRead: Long, elapsedNanos: Long): Long {
            if (bytesRead <= 0 || elapsedNanos <= 0) {
                return 0
            }

            val bytesPerSecond = bytesRead * NANOS_PER_SECOND.toDouble() / elapsedNanos
            return clamp(bytesPerSecond, 0.0, Long.MAX_VALUE.toDouble()).toLong()
        }
    }
}
