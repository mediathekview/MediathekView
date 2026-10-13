package mediathek.controller.starter

import org.junit.jupiter.api.Assertions.assertDoesNotThrow
import org.junit.jupiter.api.Assertions.assertThrows
import org.junit.jupiter.api.Test

internal class SlowDownloadMonitorTest {
    @Test
    fun switchesOnlyAfterSustainedSlowTransferWithAnotherRouteAvailable() {
        var now = 0L
        val monitor = SlowDownloadMonitor(canSwitch = { true }, nowNanos = { now })
        monitor.record(512 * 1024)
        now = 14_000_000_000L
        assertDoesNotThrow { monitor.record(512 * 1024) }
        now = 15_000_000_000L
        assertThrows(SlowDownloadException::class.java) { monitor.record(1) }
    }

    @Test
    fun slowConnectionContinuesWhenNoOtherRouteExists() {
        var now = 0L
        val monitor = SlowDownloadMonitor(canSwitch = { false }, nowNanos = { now })
        now = 15_000_000_000L
        assertDoesNotThrow { monitor.record(1) }
    }

    @Test
    fun fastTransferDoesNotTriggerRouteChange() {
        var now = 0L
        val monitor = SlowDownloadMonitor(canSwitch = { true }, nowNanos = { now })
        now = 15_000_000_000L
        assertDoesNotThrow { monitor.record(16 * 1024 * 1024) }
    }
}
