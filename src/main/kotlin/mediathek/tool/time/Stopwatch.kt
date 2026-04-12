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

package mediathek.tool.time

import java.time.Duration
import java.time.Instant
import java.util.*
import java.util.concurrent.TimeUnit

/**
 * Lightweight stopwatch replacement for Guava Stopwatch.
 */
class Stopwatch private constructor() {
    private var elapsed = Duration.ZERO
    private var startedAt: Instant? = null
    private var running = false

    @Synchronized
    fun start(): Stopwatch {
        check(!running) { "This stopwatch is already running." }
        startedAt = Instant.now()
        running = true
        return this
    }

    @Synchronized
    fun stop(): Stopwatch {
        check(running) { "This stopwatch is already stopped." }
        elapsed = elapsed.plus(Duration.between(startedAt, Instant.now()))
        startedAt = null
        running = false
        return this
    }

    @Synchronized
    fun reset(): Stopwatch {
        elapsed = Duration.ZERO
        startedAt = null
        running = false
        return this
    }

    @Synchronized
    fun isRunning(): Boolean {
        return running
    }

    @Synchronized
    fun elapsed(): Duration {
        if (!running) {
            return elapsed
        }
        return elapsed.plus(Duration.between(startedAt, Instant.now()))
    }

    @Synchronized
    fun elapsed(desiredUnit: TimeUnit): Long {
        return desiredUnit.convert(elapsed().toNanos(), TimeUnit.NANOSECONDS)
    }

    @Synchronized
    override fun toString(): String {
        val nanos = elapsed().toNanos()
        val unit = chooseUnit(nanos)
        val value = nanos / TimeUnit.NANOSECONDS.convert(1, unit).toDouble()
        return String.format(Locale.ROOT, "%.4g %s", value, abbreviate(unit))
    }

    companion object {
        @JvmStatic
        fun createStarted(): Stopwatch {
            return Stopwatch().start()
        }

        @JvmStatic
        fun createUnstarted(): Stopwatch {
            return Stopwatch()
        }

        private fun chooseUnit(nanos: Long): TimeUnit {
            if (TimeUnit.NANOSECONDS.toDays(nanos) > 0) {
                return TimeUnit.DAYS
            }
            if (TimeUnit.NANOSECONDS.toHours(nanos) > 0) {
                return TimeUnit.HOURS
            }
            if (TimeUnit.NANOSECONDS.toMinutes(nanos) > 0) {
                return TimeUnit.MINUTES
            }
            if (TimeUnit.NANOSECONDS.toSeconds(nanos) > 0) {
                return TimeUnit.SECONDS
            }
            if (TimeUnit.NANOSECONDS.toMillis(nanos) > 0) {
                return TimeUnit.MILLISECONDS
            }
            if (TimeUnit.NANOSECONDS.toMicros(nanos) > 0) {
                return TimeUnit.MICROSECONDS
            }
            return TimeUnit.NANOSECONDS
        }

        private fun abbreviate(unit: TimeUnit): String {
            return when (unit) {
                TimeUnit.NANOSECONDS -> "ns"
                TimeUnit.MICROSECONDS -> "us"
                TimeUnit.MILLISECONDS -> "ms"
                TimeUnit.SECONDS -> "s"
                TimeUnit.MINUTES -> "min"
                TimeUnit.HOURS -> "h"
                TimeUnit.DAYS -> "d"
            }
        }
    }
}
