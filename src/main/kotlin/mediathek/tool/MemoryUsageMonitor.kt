/*
 * Copyright (c) 2025-2026 derreisende77.
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

import java.util.concurrent.Executors
import java.util.concurrent.TimeUnit

private const val HISTORY_WINDOW_MILLIS = 120_000L

internal object MemoryUsageMonitor {
    const val SAMPLE_INTERVAL_MILLIS = 1_000

    private val runtime = Runtime.getRuntime()
    private val history = MemoryUsageHistory(System::currentTimeMillis, SleepAwareElapsedClock::millis)

    private val sampler = Executors.newSingleThreadScheduledExecutor { task ->
        Thread(task, "memory-usage-monitor").apply { isDaemon = true }
    }

    fun start() {
        recordSample()
        val interval = SAMPLE_INTERVAL_MILLIS.toLong()
        sampler.scheduleWithFixedDelay(::recordSample, interval, interval, TimeUnit.MILLISECONDS)
    }

    fun stop() {
        sampler.shutdownNow()
    }

    fun snapshot(): MemoryUsageSnapshot = history.snapshot()

    private fun recordSample() {
        val usedMiB = (runtime.totalMemory() - runtime.freeMemory()) / (1024.0 * 1024.0)
        history.recordSample(usedMiB)
    }
}

internal data class MemoryUsageSample(val timestampMillis: Long, val usedMiB: Double)

internal data class MemoryUsageSnapshot(
    val windowEndMillis: Long,
    val samples: List<MemoryUsageSample>,
) {
    val windowStartMillis: Long
        get() = windowEndMillis - HISTORY_WINDOW_MILLIS
}

internal class MemoryUsageHistory(
    private val wallClockMillis: () -> Long,
    private val elapsedClockMillis: () -> Long?,
    private val awakeClockMillis: () -> Long = { TimeUnit.NANOSECONDS.toMillis(System.nanoTime()) },
) {
    private val recent = ArrayDeque<TimedSample>()
    private var fallbackOffsetBounds: FallbackOffsetBounds? = null

    @Synchronized
    fun recordSample(usedMiB: Double) {
        val (wallMillis, elapsedMillis) = readClocks()
        trimToWindow(elapsedMillis, wallMillis)
        if (recent.lastOrNull()?.sample?.timestampMillis == wallMillis) {
            recent.removeLast()
        }
        recent.addLast(TimedSample(elapsedMillis, MemoryUsageSample(wallMillis, usedMiB)))
    }

    @Synchronized
    fun snapshot(): MemoryUsageSnapshot {
        val (wallMillis, elapsedMillis) = readClocks()
        trimToWindow(elapsedMillis, wallMillis)
        val wallCutoff = wallMillis - HISTORY_WINDOW_MILLIS
        val samples = recent.mapNotNull { timedSample -> timedSample.sample.takeIf { it.timestampMillis >= wallCutoff } }
        return MemoryUsageSnapshot(wallMillis, samples)
    }

    private fun readClocks(): ClockReading {
        val nativeElapsed = elapsedClockMillis()
        if (nativeElapsed != null) {
            return ClockReading(wallClockMillis(), nativeElapsed)
        }

        val awakeBefore = awakeClockMillis()
        val wallMillis = wallClockMillis()
        val awakeAfter = awakeClockMillis()
        // One millisecond of drift per second is one microsecond per awake millisecond.
        val toleranceMicros = CLOCK_TOLERANCE_MICROS + (awakeAfter - awakeBefore)
        var minimumOffsetMicros = TimeUnit.MILLISECONDS.toMicros(wallMillis - awakeAfter) - toleranceMicros
        var maximumOffsetMicros = TimeUnit.MILLISECONDS.toMicros(wallMillis - awakeBefore) + toleranceMicros
        fallbackOffsetBounds?.let { previous ->
            // Intersect with earlier observations so a delayed read cannot erase their precision.
            val driftMicros = awakeAfter - previous.observedAtMillis
            val lowerBound = maxOf(minimumOffsetMicros, previous.minimumOffsetMicros - driftMicros)
            val upperBound = minOf(maximumOffsetMicros, previous.maximumOffsetMicros + driftMicros)
            if (lowerBound <= upperBound) {
                minimumOffsetMicros = lowerBound
                maximumOffsetMicros = upperBound
            } else {
                recent.clear()
            }
        }
        fallbackOffsetBounds = FallbackOffsetBounds(minimumOffsetMicros, maximumOffsetMicros, awakeAfter)
        return ClockReading(wallMillis, awakeAfter)
    }

    private fun trimToWindow(elapsedMillis: Long, nowMillis: Long) {
        val cutoff = elapsedMillis - HISTORY_WINDOW_MILLIS
        while (recent.isNotEmpty() && recent.first().elapsedMillis < cutoff) {
            recent.removeFirst()
        }
        // A backward clock adjustment can put the newest samples after the current time.
        while (recent.isNotEmpty() && recent.last().sample.timestampMillis > nowMillis) {
            recent.removeLast()
        }
    }

    private data class ClockReading(val wallMillis: Long, val elapsedMillis: Long)
    private data class TimedSample(val elapsedMillis: Long, val sample: MemoryUsageSample)
    private data class FallbackOffsetBounds(
        val minimumOffsetMicros: Long,
        val maximumOffsetMicros: Long,
        val observedAtMillis: Long,
    )

    private companion object {
        const val CLOCK_TOLERANCE_MICROS = 10_000L
    }
}
