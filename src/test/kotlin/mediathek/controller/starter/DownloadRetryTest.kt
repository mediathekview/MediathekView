package mediathek.controller.starter

import org.junit.jupiter.api.Assertions.assertFalse
import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Assertions.assertTrue
import org.junit.jupiter.api.Test
import java.io.IOException
import java.net.SocketTimeoutException

internal class DownloadRetryTest {
    @Test
    fun recognizesTimeoutRegardlessOfMessageOrWrapper() {
        assertTrue(SocketTimeoutException("Read timed out").isDownloadTimeout())
        assertTrue(IOException("request failed", SocketTimeoutException("timeout")).isDownloadTimeout())
    }

    @Test
    fun doesNotRetryOtherIoFailuresAsTimeouts() {
        assertFalse(IOException("permission denied").isDownloadTimeout())
    }

    @Test
    fun progressResetsConsecutiveFailureCount() {
        assertEquals(1, nextDownloadRetryCount(0, 0, 2L * 1024 * 1024))
        assertEquals(1, nextDownloadRetryCount(1, 2L * 1024 * 1024, 16L * 1024 * 1024))
        assertEquals(2, nextDownloadRetryCount(1, 16L * 1024 * 1024, 16L * 1024 * 1024))
        assertEquals(1, nextDownloadRetryCount(3, 16L * 1024 * 1024, 17L * 1024 * 1024))
    }
}
