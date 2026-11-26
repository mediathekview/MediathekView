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

package mediathek.gui.dialog

import mediathek.config.application.ApplicationConfiguration
import mediathek.mainwindow.MemoryUsagePanel
import mediathek.tool.MemoryUsageHistory
import mediathek.tool.SleepAwareElapsedClock
import org.jfree.chart.ChartPanel
import org.jfree.chart.plot.XYPlot
import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Assertions.assertNull
import org.junit.jupiter.api.Assertions.assertTrue
import org.junit.jupiter.api.Assumptions.assumeFalse
import org.junit.jupiter.api.Test
import org.junit.jupiter.api.io.TempDir
import org.junit.jupiter.params.ParameterizedTest
import org.junit.jupiter.params.provider.CsvSource
import org.junit.jupiter.params.provider.ValueSource
import org.mockito.Mockito.mock
import java.awt.GraphicsEnvironment
import java.nio.file.Path
import java.time.Instant
import java.util.concurrent.ScheduledFuture
import java.util.concurrent.TimeUnit
import javax.swing.JFrame
import javax.swing.SwingUtilities
import kotlin.io.path.readText
import kotlin.system.exitProcess
import kotlin.time.Duration.Companion.minutes
import kotlin.time.Duration.Companion.seconds

internal class MemoryMonitorDialogTest {
    @ParameterizedTest(name = "wall clock corrected backward by {0} minutes")
    @ValueSource(longs = [0, 2])
    fun `reopening after sleep drops samples immediately before the sampler wakes up`(
        backwardCorrectionMinutes: Long,
        @TempDir directory: Path,
    ) {
        assumeFalse(GraphicsEnvironment.isHeadless(), "Swing dialog test requires a display")
        var wallTime = BEFORE_SLEEP
        var elapsedTime = 0L
        val history = MemoryUsageHistory({ wallTime }, { elapsedTime })
        verifyReopeningAfterSleep(history, testConfiguration(directory)) {
            elapsedTime += 3.minutes.inWholeMilliseconds
            wallTime += (3.minutes - backwardCorrectionMinutes.minutes).inWholeMilliseconds
        }
    }

    @ParameterizedTest(name = "{0} ms visible sleep, {1} ms initial read pause, {2} additional delayed reads")
    @CsvSource("60000, 0, 0", "500, 0, 0", "500, 1250, 0", "55, 0, 50")
    fun `reopening after sleep and a backward correction drops samples with native access denied`(
        visibleSleepMillis: Long,
        pauseBeforeSleepMillis: Long,
        delayedReadCount: Int,
        @TempDir directory: Path,
    ) {
        assumeFalse(GraphicsEnvironment.isHeadless(), "Swing dialog test requires a display")
        val java = Path.of(System.getProperty("java.home"), "bin", "java")
        val output = directory.resolve("denied-native-access.log")
        val process = ProcessBuilder(
            java.toString(),
            "--illegal-native-access=deny",
            "-cp", System.getProperty("java.class.path"),
            MemoryMonitorDialogTest::class.java.name,
            directory.toString(),
            visibleSleepMillis.toString(),
            pauseBeforeSleepMillis.toString(),
            delayedReadCount.toString(),
        ).redirectErrorStream(true).redirectOutput(output.toFile()).start()
        try {
            assertTrue(process.waitFor(30, TimeUnit.SECONDS), "Swing regression process timed out")
            assertEquals(0, process.exitValue(), output.readText())
        } finally {
            if (process.isAlive) {
                process.destroyForcibly()
                process.waitFor(5, TimeUnit.SECONDS)
            }
        }
    }

    @Test
    fun `a forward clock jump keeps the chart within two wall-clock minutes`(@TempDir directory: Path) {
        assumeFalse(GraphicsEnvironment.isHeadless(), "Swing dialog test requires a display")
        var wallTime = BEFORE_SLEEP
        var elapsedTime = 0L
        val history = MemoryUsageHistory({ wallTime }, { elapsedTime })
        history.recordSample(30.0)
        wallTime += 60.minutes.inWholeMilliseconds
        val afterJump = wallTime
        elapsedTime += 1.seconds.inWholeMilliseconds
        history.recordSample(35.0)
        wallTime += 1.seconds.inWholeMilliseconds
        elapsedTime += 1.seconds.inWholeMilliseconds
        history.recordSample(36.0)
        val configuration = testConfiguration(directory)

        SwingUtilities.invokeAndWait {
            val owner = JFrame()
            try {
                withVisibleChart(owner, history, configuration) { plot ->
                    assertEquals(2, plot.dataset.getItemCount(0))
                    assertEquals(afterJump.toDouble(), plot.dataset.getXValue(0, 0))
                    assertEquals(wallTime.toDouble(), plot.dataset.getXValue(0, 1))
                    assertTimeWindow(plot, wallTime)
                }
            } finally {
                owner.dispose()
            }
        }
    }

    @Test
    fun `an empty chart uses the current two-minute window immediately after a forward clock jump`(
        @TempDir directory: Path,
    ) {
        assumeFalse(GraphicsEnvironment.isHeadless(), "Swing dialog test requires a display")
        var wallTime = BEFORE_SLEEP
        val history = MemoryUsageHistory({ wallTime }, { 0L })
        history.recordSample(30.0)
        val configuration = testConfiguration(directory)
        SwingUtilities.invokeAndWait {
            val owner = JFrame()
            try {
                withVisibleChart(owner, history, configuration) { plot ->
                    assertEquals(1, plot.dataset.getItemCount(0))
                }
                wallTime += 60.minutes.inWholeMilliseconds
                withVisibleChart(owner, history, configuration) { plot ->
                    assertEquals(0, plot.dataset.getItemCount(0))
                    assertTimeWindow(plot, wallTime)
                }
            } finally {
                owner.dispose()
            }
        }
    }

    private fun assertTimeWindow(plot: XYPlot, wallTime: Long) {
        val range = plot.domainAxis.range
        assertEquals((wallTime - 2.minutes.inWholeMilliseconds).toDouble(), range.lowerBound)
        assertEquals(wallTime.toDouble(), range.upperBound)
    }

    private fun testConfiguration(directory: Path) = ApplicationConfiguration(
        directory.resolve("settings.xml"),
        scheduleAutosave = { mock(ScheduledFuture::class.java) },
    )

    companion object {
        private val BEFORE_SLEEP = Instant.parse("2026-01-01T10:00:00Z").toEpochMilli()

        @JvmStatic
        fun main(args: Array<String>) {
            try {
                assertNull(SleepAwareElapsedClock.millis(), "Native clock unexpectedly available in the denied JVM")
                var wallTime = BEFORE_SLEEP
                var awakeTime = 0L
                var pauseBeforeWallRead = 0L
                val history = MemoryUsageHistory(
                    wallClockMillis = {
                        wallTime += pauseBeforeWallRead
                        awakeTime += pauseBeforeWallRead
                        pauseBeforeWallRead = 0L
                        wallTime
                    },
                    elapsedClockMillis = SleepAwareElapsedClock::millis,
                    awakeClockMillis = { awakeTime },
                )
                val configuration = ApplicationConfiguration(Path.of(args[0], "settings.xml"))
                val visibleSleepMillis = args[1].toLong()
                val pauseBeforeSleepMillis = args[2].toLong()
                val delayedReadCount = args[3].toInt()
                verifyReopeningAfterSleep(
                    history,
                    configuration,
                    beforeFirstOpen = { pauseBeforeWallRead = pauseBeforeSleepMillis },
                ) {
                    repeat(delayedReadCount) { index ->
                        // Alternate the sampler and chart reads at 500 ms intervals.
                        wallTime += 375
                        awakeTime += 375
                        pauseBeforeWallRead = 125
                        if (index % 2 == 0) {
                            history.recordSample(35.0)
                        } else {
                            assertEquals(2 + index / 2, history.snapshot().samples.size)
                        }
                    }
                    // A backward correction masks all but visibleSleepMillis of the three-minute sleep.
                    wallTime += visibleSleepMillis + 250
                    awakeTime += 250
                }
                exitProcess(0)
            } catch (failure: Throwable) {
                failure.printStackTrace()
                exitProcess(1)
            }
        }

        private fun verifyReopeningAfterSleep(
            history: MemoryUsageHistory,
            configuration: ApplicationConfiguration,
            beforeFirstOpen: () -> Unit = {},
            advanceClocks: () -> Unit,
        ) {
            history.recordSample(30.0)
            SwingUtilities.invokeAndWait {
                val owner = JFrame()
                try {
                    beforeFirstOpen()
                    withVisibleChart(owner, history, configuration) { plot ->
                        assertEquals(1, plot.dataset.getItemCount(0))
                        assertEquals(BEFORE_SLEEP.toDouble(), plot.dataset.getXValue(0, 0))
                    }
                    advanceClocks()
                    withVisibleChart(owner, history, configuration) { plot ->
                        assertEquals(0, plot.dataset.getItemCount(0))
                    }
                } finally {
                    owner.dispose()
                }
            }
        }

        private fun withVisibleChart(
            owner: JFrame,
            history: MemoryUsageHistory,
            configuration: ApplicationConfiguration,
            verify: (XYPlot) -> Unit,
        ) {
            val panel = MemoryUsagePanel(history::snapshot)
            val dialog = MemoryMonitorDialog(owner, panel, configuration) {}
            try {
                dialog.isVisible = true
                assertTrue(dialog.isShowing)
                val chart = panel.getComponent(0) as ChartPanel
                verify(chart.chart.xyPlot)
            } finally {
                dialog.dispose()
            }
        }
    }
}
