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

import java.util.concurrent.locks.LockSupport
import kotlin.math.ceil
import kotlin.math.max
import kotlin.math.min

/**
 * Lightweight byte-based rate limiter using a monotonic clock.
 * Uses a smooth-bursty model similar to Guava's RateLimiter so small scheduler differences
 * between operating systems do not dominate the effective throughput.
 */
class ByteRateLimiter(bytesPerSecond: Long) {
    @Volatile
    private var bytesPerSecond = 0L

    @Volatile
    private var unlimited = false

    private var storedPermits = 0.0
    private var maxPermits = 0.0
    private var stableNanosPerPermit = 0.0
    private var nextAvailableNanos = 0L

    init {
        setRate(bytesPerSecond)
    }

    fun setRate(bytesPerSecond: Long) {
        synchronized(this) {
            val now = System.nanoTime()
            unlimited = bytesPerSecond >= UNLIMITED_THRESHOLD
            this.bytesPerSecond = if (bytesPerSecond <= 0) 1 else bytesPerSecond

            if (unlimited) {
                storedPermits = 0.0
                maxPermits = 0.0
                stableNanosPerPermit = 0.0
                nextAvailableNanos = 0L
            } else {
                resync(now)

                val oldMaxPermits = maxPermits
                stableNanosPerPermit = NANOS_PER_SECOND / this.bytesPerSecond.toDouble()
                maxPermits = max(1.0, this.bytesPerSecond * MAX_BURST_SECONDS)

                storedPermits = if (oldMaxPermits <= 0.0 || !oldMaxPermits.isFinite()) {
                    min(storedPermits, maxPermits)
                } else {
                    min(maxPermits, storedPermits * maxPermits / oldMaxPermits)
                }
            }
        }
    }

    fun acquire(permits: Int, operationStartedNanos: Long, operationCompletedNanos: Long) {
        if (permits <= 0) {
            return
        }

        val waitNanos = synchronized(this) {
            if (unlimited) {
                return
            }

            val now = max(operationStartedNanos, operationCompletedNanos)
            reserveAndGetWaitLength(permits, now)
        }

        if (waitNanos > 0) {
            LockSupport.parkNanos(waitNanos)
        }
    }

    private fun reserveAndGetWaitLength(permits: Int, nowNanos: Long): Long {
        resync(nowNanos)

        val momentAvailable = nextAvailableNanos
        val permitsToSpendFromStorage = min(permits.toDouble(), storedPermits)
        val freshPermits = permits - permitsToSpendFromStorage
        val waitNanos = nanosForPermits(freshPermits, stableNanosPerPermit)

        nextAvailableNanos = safeAdd(nextAvailableNanos, waitNanos)
        storedPermits -= permitsToSpendFromStorage

        return max(momentAvailable - nowNanos, 0L)
    }

    private fun resync(nowNanos: Long) {
        if (nowNanos <= nextAvailableNanos) {
            return
        }
        if (stableNanosPerPermit <= 0.0) {
            nextAvailableNanos = nowNanos
            return
        }

        val newPermits = (nowNanos - nextAvailableNanos) / stableNanosPerPermit
        storedPermits = min(maxPermits, storedPermits + newPermits)
        nextAvailableNanos = nowNanos
    }

    private companion object {
        private const val NANOS_PER_SECOND = 1_000_000_000L
        private const val UNLIMITED_THRESHOLD = Long.MAX_VALUE / 2
        private const val MAX_BURST_SECONDS = 1.0

        private fun nanosForPermits(permits: Double, nanosPerPermit: Double): Long {
            val nanos = permits * nanosPerPermit
            if (!nanos.isFinite() || nanos >= Long.MAX_VALUE.toDouble()) {
                return Long.MAX_VALUE
            }
            return max(1L, ceil(nanos).toLong())
        }

        private fun safeAdd(a: Long, b: Long): Long {
            if (Long.MAX_VALUE - a < b) {
                return Long.MAX_VALUE
            }
            return a + b
        }
    }
}
