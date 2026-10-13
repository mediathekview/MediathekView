package mediathek.controller.starter

import java.io.IOException

internal class SlowDownloadException(val bytesPerSecond: Long) :
    IOException("Download remained slow at $bytesPerSecond bytes/s")

/** Checks sustained network throughput after each write, before resuming on another route. */
internal class SlowDownloadMonitor(
    private val canSwitch: () -> Boolean,
    private val nowNanos: () -> Long = System::nanoTime,
) {
    private var windowStartedAt = nowNanos()
    private var bytes = 0L

    fun record(bytesRead: Int) {
        bytes += bytesRead
        val now = nowNanos()
        val elapsed = now - windowStartedAt
        if (elapsed < SAMPLE_NANOS) return

        val rate = bytes * NANOS_PER_SECOND / elapsed
        if (rate < MIN_BYTES_PER_SECOND && canSwitch()) {
            throw SlowDownloadException(rate)
        }
        windowStartedAt = now
        bytes = 0
    }

    private companion object {
        const val NANOS_PER_SECOND = 1_000_000_000L
        const val SAMPLE_NANOS = 15L * NANOS_PER_SECOND
        const val MIN_BYTES_PER_SECOND = 1024L * 1024L
    }
}
