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

import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Assertions.assertTrue
import org.junit.jupiter.api.Test
import org.junit.jupiter.params.ParameterizedTest
import org.junit.jupiter.params.provider.ValueSource

internal class MemoryUsageHistoryTest {
    @Test
    fun `one rolling history includes the first sample until it ages out`() {
        var now = 0L
        val history = MemoryUsageHistory({ now }, { now })
        val first = MemoryUsageSample(0, 30.0)
        val second = MemoryUsageSample(1_000, 31.0)
        val latest = MemoryUsageSample(120_000, 32.5)
        history.recordSample(first.usedMiB)
        now = second.timestampMillis
        history.recordSample(second.usedMiB)
        now = latest.timestampMillis
        history.recordSample(latest.usedMiB)

        assertEquals(listOf(first, second, latest), history.snapshot().samples)
        now++
        assertEquals(listOf(second, latest), history.snapshot().samples)
    }

    @ParameterizedTest(name = "fallback clock: {0}")
    @ValueSource(booleans = [false, true])
    fun `a snapshot expires pre-sleep samples even when the wall clock advances only one minute`(useFallback: Boolean) {
        var wallTime = 1_000_000L
        var elapsedTime = 0L
        val history = MemoryUsageHistory({ wallTime }, { if (useFallback) null else elapsedTime }, { 0L })
        history.recordSample(30.0)

        elapsedTime += 180_000
        wallTime += 60_000
        assertTrue(history.snapshot().samples.isEmpty())

        history.recordSample(35.0)
        assertEquals(listOf(MemoryUsageSample(1_060_000, 35.0)), history.snapshot().samples)
    }

    @Test
    fun `the chart hides samples outside the wall window until the clock is corrected`() {
        var wallTime = 1_000_000L
        var elapsedTime = 0L
        val history = MemoryUsageHistory({ wallTime }, { elapsedTime })
        history.recordSample(30.0)

        wallTime += 3_600_000
        elapsedTime += 1_000
        history.recordSample(35.0)
        assertEquals(listOf(MemoryUsageSample(4_600_000, 35.0)), history.snapshot().samples)

        wallTime = 1_002_000
        elapsedTime += 1_000
        assertEquals(listOf(MemoryUsageSample(1_000_000, 30.0)), history.snapshot().samples)
    }

    @Test
    fun `fallback keeps recent samples while the two JVM clocks advance together`() {
        var wallTime = 1_000_000L
        var awakeTime = 0L
        val history = MemoryUsageHistory({ wallTime }, { null }, { awakeTime })
        history.recordSample(30.0)
        wallTime += 2_000
        awakeTime += 2_000
        history.recordSample(35.0)

        assertEquals(
            listOf(MemoryUsageSample(1_000_000, 30.0), MemoryUsageSample(1_002_000, 35.0)),
            history.snapshot().samples,
        )
    }

    @Test
    fun `fallback allows gradual clock drift without clearing recent samples`() {
        var wallTime = 1_000_000L
        var awakeTime = 0L
        val history = MemoryUsageHistory({ wallTime }, { null }, { awakeTime })
        history.recordSample(30.0)
        repeat(60) {
            wallTime += 1_001
            awakeTime += 1_000
            history.recordSample(35.0)
        }

        val samples = history.snapshot().samples
        assertEquals(61, samples.size)
        assertEquals(MemoryUsageSample(1_000_000, 30.0), samples.first())
    }

    @Test
    fun `a pause between fallback clock reads preserves samples when neither clock jumped`() {
        var time = 0L
        var pauseAfterWallRead = false
        val history = MemoryUsageHistory(
            wallClockMillis = {
                val wallMillis = 1_000_000 + time
                if (pauseAfterWallRead) {
                    time += 1_250
                    pauseAfterWallRead = false
                }
                wallMillis
            },
            elapsedClockMillis = { null },
            awakeClockMillis = { time },
        )
        history.recordSample(30.0)
        time += 1_000
        pauseAfterWallRead = true
        assertEquals(listOf(MemoryUsageSample(1_000_000, 30.0)), history.snapshot().samples)

        time += 1_000
        history.recordSample(35.0)
        assertEquals(
            listOf(MemoryUsageSample(1_000_000, 30.0), MemoryUsageSample(1_003_250, 35.0)),
            history.snapshot().samples,
        )
    }

    @Test
    fun `a backward clock adjustment discards future samples`() {
        var wallTime = 1_000L
        var elapsedTime = 0L
        val history = MemoryUsageHistory({ wallTime }, { elapsedTime })
        history.recordSample(30.0)
        wallTime = 3_000
        elapsedTime = 2_000
        history.recordSample(31.0)

        wallTime = 2_000
        elapsedTime = 3_000
        history.recordSample(32.0)
        assertEquals(
            listOf(MemoryUsageSample(1_000, 30.0), MemoryUsageSample(2_000, 32.0)),
            history.snapshot().samples,
        )
    }
}
