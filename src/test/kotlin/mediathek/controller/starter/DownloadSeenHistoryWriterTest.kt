package mediathek.controller.starter

import mediathek.daten.DatenFilm
import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Assertions.assertTrue
import org.junit.jupiter.api.Test
import java.util.concurrent.CountDownLatch
import java.util.concurrent.TimeUnit

internal class DownloadSeenHistoryWriterTest {
    @Test
    fun slowHistoryWriteDoesNotBlockDownloadStartAndFlushWaitsForEveryBatch() {
        val writeStarted = CountDownLatch(1)
        val releaseWrite = CountDownLatch(1)
        val batches = mutableListOf<List<DatenFilm>>()
        val writer = DownloadSeenHistoryWriter { films ->
            writeStarted.countDown()
            assertTrue(releaseWrite.await(5, TimeUnit.SECONDS))
            batches += films
        }
        val first = DatenFilm()
        val second = DatenFilm()

        writer.markSeen(listOf(first))
        assertTrue(writeStarted.await(5, TimeUnit.SECONDS))
        writer.markSeen(listOf(second))
        releaseWrite.countDown()
        writer.flush()

        assertEquals(listOf(listOf(first), listOf(second)), batches)
    }
}
