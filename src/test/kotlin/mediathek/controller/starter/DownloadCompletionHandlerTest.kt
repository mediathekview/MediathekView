package mediathek.controller.starter

import mediathek.daten.DatenDownload
import mediathek.daten.DownloadSource
import mediathek.daten.DownloadType
import org.junit.jupiter.api.Assertions.assertNull
import org.junit.jupiter.api.Assertions.assertTrue
import org.junit.jupiter.api.Test
import org.junit.jupiter.api.io.TempDir
import java.nio.file.Path
import java.time.LocalDateTime

internal class DownloadCompletionHandlerTest {
    @TempDir
    lateinit var tempDir: Path

    @Test
    fun stoppedDownloadIsNotLeftRunningAfterFinalization() {
        val download = download("Stopped")
        val start = DownloadRunState().apply {
            status = StartStatus.RUNNING
            startTime = LocalDateTime.now()
            requestStop()
        }
        download.runtime.runState = start
        DownloadLifecycleActions.markInterrupted(download)

        DownloadCompletionHandler.finalizeDownload(download, start, HttpDownloadState.DOWNLOAD)

        assertNull(download.runtime.runState)
        assertTrue(download.isInterrupted)
    }

    private fun download(title: String): DatenDownload =
        DatenDownload().apply {
            sender = "Sender"
            topic = "Topic"
            this.title = title
            date = "01.06.2026"
            time = "20:15:00"
            duration = "00:45:00"
            historyUrl = "https://example.invalid/history/$title"
            filmUrl = "https://example.invalid/film/$title"
            downloadUrl = "https://example.invalid/download/$title.mp4"
            targetFileName = "$title.mp4"
            targetPath = tempDir.toString()
            targetPathFileName = tempDir.resolve("$title.mp4").toString()
            programSetName = "Set"
            art = DownloadType.DIRECT
            quelle = DownloadSource.ABO
            init()
        }
}
