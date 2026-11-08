package mediathek.controller.starter

import mediathek.daten.DatenDownload
import mediathek.daten.DownloadSource
import mediathek.daten.DownloadType
import mediathek.tool.notification.DisabledNotificationBackend
import org.junit.jupiter.api.Assertions.*
import org.junit.jupiter.api.Test
import org.junit.jupiter.api.io.TempDir
import org.junit.jupiter.params.ParameterizedTest
import org.junit.jupiter.params.provider.ValueSource
import org.mockito.Mockito.*
import java.nio.file.Path
import java.time.LocalDateTime

internal class ExternalProgramDownloadTest {
    @TempDir
    lateinit var directory: Path

    @ParameterizedTest
    @ValueSource(booleans = [false, true])
    fun `cancellation during process startup destroys the returned process before finalization`(downloadManager: Boolean) {
        val download = download().apply { isDownloadManager = downloadManager }
        val state = requireNotNull(download.runtime.runState)
        val process = mock(Process::class.java)
        doAnswer {
            assertSame(state, download.runtime.runState, "Destroy the process before finalization resets the run state")
            assertTrue(state.isRunning)
            null
        }.`when`(process).destroy()
        mockConstruction(RuntimeExec::class.java) { runtime, _ ->
            `when`(runtime.exec(true)).thenAnswer {
                // The user stops the download while ProcessBuilder.start() is returning.
                state.requestStop()
                process
            }
        }.use { construction ->
            runner(download).run()
            assertEquals(1, construction.constructed().size)
        }
        assertSame(process, state.process)
        verify(process).destroy()
        assertNull(download.runtime.runState)
        assertFalse(state.isError)
    }

    @Test
    fun `cancellation before startup never launches a process`() {
        val download = download()
        val state = requireNotNull(download.runtime.runState).apply { requestStop() }
        mockConstruction(RuntimeExec::class.java).use { construction ->
            runner(download).run()
            assertTrue(construction.constructed().isEmpty())
        }
        assertNull(state.process)
        assertNull(download.runtime.runState)
        assertFalse(state.isError)
    }

    @Test
    fun `cancellation during failed startup finishes without retrying or marking an error`() {
        val download = download()
        val state = requireNotNull(download.runtime.runState)
        mockConstruction(RuntimeExec::class.java) { runtime, _ ->
            `when`(runtime.exec(true)).thenAnswer {
                state.requestStop()
                null
            }
        }.use { construction ->
            runner(download).run()
            assertEquals(1, construction.constructed().size)
        }
        assertNull(state.process)
        assertNull(download.runtime.runState)
        assertFalse(state.isError)
    }

    @Test
    fun `normal successful process is monitored and is not destroyed`() {
        val download = download()
        val state = requireNotNull(download.runtime.runState)
        val process = mock(Process::class.java)
        `when`(process.exitValue()).thenReturn(0)
        mockConstruction(RuntimeExec::class.java) { runtime, _ ->
            `when`(runtime.exec(true)).thenReturn(process)
        }.use {
            runner(download).run()
        }
        verify(process).exitValue()
        verify(process, never()).destroy()
        assertTrue(state.isFinished)
        assertSame(state, download.runtime.runState)
    }

    private fun runner(download: DatenDownload) = ExternalProgramDownload(
        aboHistoryControllerProvider = { error("Unexpected subscription history access") },
        datenDownload = download,
        notificationPublisher = DisabledNotificationBackend,
    )

    private fun download() = DatenDownload().apply {
        title = "Startup cancellation"
        sender = "ARTE.DE"
        quelle = DownloadSource.BUTTON
        art = DownloadType.PROGRAM
        // Non-HLS URL keeps these lifecycle tests independent of HTTP and playlist parsing.
        downloadUrl = "https://example.invalid/film.mp4"
        targetPath = directory.toString()
        targetPathFileName = directory.resolve("film.mp4").toString()
        runtime.runState = DownloadRunState().apply { startTime = LocalDateTime.now() }
        programInvocation = "ffmpeg -i $downloadUrl $targetPathFileName"
    }
}
