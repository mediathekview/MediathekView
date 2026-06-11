package mediathek.controller

import mediathek.controller.starter.DownloadLifecycleActions
import mediathek.controller.starter.DownloadRunState
import mediathek.controller.starter.StartStatus
import mediathek.daten.DatenDownload
import mediathek.daten.DownloadSource
import mediathek.daten.DownloadType
import mediathek.tool.FileSize
import org.junit.jupiter.api.Assertions.*
import org.junit.jupiter.api.Test
import org.junit.jupiter.api.io.TempDir
import java.nio.file.Path
import kotlin.io.path.readText

internal class DownloadStorageTest {
    @TempDir
    lateinit var tempDir: Path

    @Test
    fun writeAndReadPreservesExplicitDownloadFields() {
        val storagePath = tempDir.resolve("downloads.json")
        val download = download("Title").apply {
            runtime.filmSize.size = 42L * FileSize.ONE_MIB
            art = DownloadType.PROGRAM
            quelle = DownloadSource.DOWNLOAD
        }

        DownloadStorage.write(storagePath, listOf(download))
        val json = storagePath.readText()

        assertTrue(json.contains("\"title\""))
        assertTrue(json.contains("\"targetPathFileName\""))
        assertTrue(json.contains("\"sizeInMiB\": 42"))
        assertTrue(json.contains("\"restart\": true"))
        assertTrue(json.contains("\"type\": \"PROGRAM\""))
        assertTrue(json.contains("\"source\": \"DOWNLOAD\""))
        assertFalse(json.contains("\"filmNumber\""))
        assertFalse(json.contains("\"number\""))
        assertFalse(json.contains("\"buttonStart\""))
        assertFalse(json.contains("\"buttonDelete\""))
        assertFalse(json.contains("\"progress\""))
        assertFalse(json.contains("\"remainingTime\""))
        assertFalse(json.contains("\"bandwidth\""))
        assertFalse(json.contains("\"highQuality\""))
        assertFalse(json.contains("\"subtitleAvailable\""))
        assertFalse(json.contains("\"geo\""))
        assertFalse(json.contains("\"reference\""))
        assertFalse(json.contains("\"values\""))

        val loaded = DownloadStorage.read(storagePath)

        assertEquals(1, loaded.size)
        val loadedDownload = loaded.single()
        assertEquals("Title", loadedDownload.title)
        assertEquals("Sender", loadedDownload.sender)
        assertEquals("/tmp/Title.mp4", loadedDownload.targetPathFileName)
        assertEquals(42L * FileSize.ONE_MIB, loadedDownload.runtime.filmSize.size)
        assertEquals(DownloadType.PROGRAM, loadedDownload.art)
        assertEquals(DownloadSource.DOWNLOAD, loadedDownload.quelle)
    }

    @Test
    fun writeAppliesExistingDownloadPersistenceFilter() {
        val storagePath = tempDir.resolve("downloads.json")
        val queued = download("Queued")
        val finished = download("Finished").apply {
            runtime.runState = DownloadRunState().also { it.status = StartStatus.FINISHED }
        }
        val aboDownload = download("Abo").apply {
            aboName = "Daily"
        }
        val interruptedAboDownload = download("Interrupted Abo").apply {
            aboName = "Daily"
            DownloadLifecycleActions.markInterrupted(this)
        }

        DownloadStorage.write(storagePath, listOf(queued, finished, aboDownload, interruptedAboDownload))

        val loadedTitles = DownloadStorage.read(storagePath).map(DatenDownload::title)
        assertEquals(listOf("Queued", "Interrupted Abo"), loadedTitles)
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
            subtitleUrl = "https://example.invalid/subtitle/$title.vtt"
            programSetName = "Set"
            programName = "Program"
            programInvocation = "program $title"
            programInvocationArray = "program|$title"
            targetFileName = "$title.mp4"
            targetPath = "/tmp"
            targetPathFileName = "/tmp/$title.mp4"
            isRestart = true
            isInfoFile = true
            isSubtitle = true
            isSpotlight = true
            art = DownloadType.PROGRAM
            quelle = DownloadSource.DOWNLOAD
            init()
        }
}
