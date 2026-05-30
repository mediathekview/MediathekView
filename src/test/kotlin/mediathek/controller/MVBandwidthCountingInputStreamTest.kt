package mediathek.controller

import org.junit.jupiter.api.Assertions.assertArrayEquals
import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Test
import java.io.InputStream

internal class MVBandwidthCountingInputStreamTest {

    @Test
    fun readWithOffsetCountsBytesAndUsesDelegateBulkRead() {
        val delegate = BulkOnlyInputStream(byteArrayOf(1, 2, 3))
        val stream = MVBandwidthCountingInputStream(delegate)
        val buffer = ByteArray(6)

        val bytesRead = stream.read(buffer, 2, 3)

        assertEquals(3, bytesRead)
        assertEquals(3, stream.totalBytesRead)
        assertEquals(1, delegate.bulkReadCalls)
        assertArrayEquals(byteArrayOf(0, 0, 1, 2, 3, 0), buffer)
    }

    @Test
    fun bandwidthCalculationUsesDoubleAndClampsOverflow() {
        val bandwidth = MVBandwidthCountingInputStream.calculateBytesPerSecond(Long.MAX_VALUE, 1)

        assertEquals(Long.MAX_VALUE, bandwidth)
    }

    @Test
    fun bandwidthCalculationReturnsZeroForInvalidInputs() {
        assertEquals(0, MVBandwidthCountingInputStream.calculateBytesPerSecond(0, 1))
        assertEquals(0, MVBandwidthCountingInputStream.calculateBytesPerSecond(1, 0))
    }

    private class BulkOnlyInputStream(private val data: ByteArray) : InputStream() {
        var bulkReadCalls: Int = 0
            private set
        private var consumed = false

        override fun read(): Int {
            error("single-byte read should not be used")
        }

        override fun read(buffer: ByteArray, offset: Int, length: Int): Int {
            bulkReadCalls++
            if (consumed) {
                return -1
            }

            val bytesToRead = minOf(length, data.size)
            data.copyInto(buffer, offset, endIndex = bytesToRead)
            consumed = true
            return bytesToRead
        }
    }
}
