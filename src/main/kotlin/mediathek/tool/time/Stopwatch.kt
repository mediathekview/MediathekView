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

import java.util.*
import kotlin.time.Duration
import kotlin.time.TimeSource

/**
 * Lightweight stopwatch.
 */
class Stopwatch private constructor() {
    private var elapsed = Duration.ZERO
    private var startedAt: TimeSource.Monotonic.ValueTimeMark? = null
    private var running = false

    @Synchronized
    fun start(): Stopwatch {
        check(!running) { "This stopwatch is already running." }
        startedAt = TimeSource.Monotonic.markNow()
        running = true
        return this
    }

    @Synchronized
    fun stop(): Stopwatch {
        check(running) { "This stopwatch is already stopped." }
        elapsed += checkNotNull(startedAt).elapsedNow()
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
        return elapsed + checkNotNull(startedAt).elapsedNow()
    }

    @Synchronized
    override fun toString(): String {
        val duration = elapsed()
        val unit = DisplayUnit.choose(duration)
        val value = duration.inWholeNanoseconds / unit.nanoseconds.toDouble()
        return String.format(Locale.ROOT, "%.4g %s", value, unit.abbreviation)
    }

    private enum class DisplayUnit(val nanoseconds: Long, val abbreviation: String) {
        DAYS(86_400_000_000_000L, "d"),
        HOURS(3_600_000_000_000L, "h"),
        MINUTES(60_000_000_000L, "min"),
        SECONDS(1_000_000_000L, "s"),
        MILLISECONDS(1_000_000L, "ms"),
        MICROSECONDS(1_000L, "us"),
        NANOSECONDS(1L, "ns");

        companion object {
            fun choose(duration: Duration): DisplayUnit {
                val nanoseconds = duration.inWholeNanoseconds
                return entries.firstOrNull { nanoseconds >= it.nanoseconds } ?: NANOSECONDS
            }
        }
    }

    companion object {
        fun createStarted(): Stopwatch {
            return Stopwatch().start()
        }

    }
}
