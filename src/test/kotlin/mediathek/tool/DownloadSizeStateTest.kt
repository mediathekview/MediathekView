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

package mediathek.tool

import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Assertions.assertTrue
import org.junit.jupiter.api.Test
import java.util.concurrent.CountDownLatch
import java.util.concurrent.Executors
import java.util.concurrent.TimeUnit

internal class DownloadSizeStateTest {
    @Test
    fun rendersConfiguredSizeWhenNoActiveSizeIsKnown() {
        val state = DownloadSizeState()

        state.setSize("3")

        assertEquals("3", state.toString())
    }

    @Test
    fun rendersActiveAndTotalSize() {
        val state = DownloadSizeState()
        state.setSize("3")

        state.addAktSize(FileSize.ONE_MIB.toLong())

        assertEquals("1 von 3", state.toString())
    }

    @Test
    fun resetClearsActiveSizeOnly() {
        val state = DownloadSizeState()
        state.setSize("3")
        state.addAktSize(FileSize.ONE_MIB.toLong())

        state.reset()

        assertEquals("3", state.toString())
    }

    @Test
    fun activeSizeIncrementsAreAtomic() {
        val state = DownloadSizeState()
        state.aktSize = 0
        val workers = 8
        val incrementsPerWorker = 1_000
        val executor = Executors.newFixedThreadPool(workers)
        val start = CountDownLatch(1)
        val done = CountDownLatch(workers)

        repeat(workers) {
            executor.execute {
                try {
                    start.await()
                    repeat(incrementsPerWorker) {
                        state.addAktSize(1)
                    }
                } finally {
                    done.countDown()
                }
            }
        }

        try {
            start.countDown()
            assertTrue(done.await(5, TimeUnit.SECONDS), "Download size workers did not finish in time")
        } finally {
            executor.shutdownNow()
        }

        assertEquals((workers * incrementsPerWorker).toLong(), state.aktSize)
    }
}
