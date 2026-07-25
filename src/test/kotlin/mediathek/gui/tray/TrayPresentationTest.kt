package mediathek.gui.tray

import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Test

internal class TrayPresentationTest {
    @Test
    fun `error icon takes precedence over running downloads`() {
        val snapshot = snapshot(running = 2, error = 1)

        assertEquals(TrayIconState.ERROR, snapshot.iconState)
    }

    @Test
    fun `running download uses download icon`() {
        assertEquals(TrayIconState.DOWNLOADING, snapshot(running = 1).iconState)
    }

    @Test
    fun `idle download status has compact text`() {
        assertEquals("Downloads: 0", TrayPresentation.downloadText(snapshot()))
    }

    @Test
    fun `nonstandard download metadata keeps status details visible`() {
        assertEquals(
            "Downloads: 0   [ 0 laufen, 0 warten ]",
            TrayPresentation.downloadText(snapshot(hasValues = true)),
        )
    }

    @Test
    fun `download text includes progress and result counts`() {
        val snapshot = snapshot(
            totalStarts = 5,
            initialized = 1,
            running = 2,
            finished = 1,
            error = 1,
            bandwidthText = "3 MiB/s",
        )

        assertEquals(
            "Downloads: 5   [ 2 laufen (3 MiB/s), 1 wartet, 1 fertig, 1 fehlerhaft ]",
            TrayPresentation.downloadText(snapshot),
        )
    }

    @Test
    fun `information text combines film list and download status`() {
        assertEquals(
            "Filmliste erstellt: 10.07.2026, 12:00 Uhr  \nAnz. Filme: 42\nDownloads: 0",
            TrayPresentation.informationText("10.07.2026, 12:00", 42, snapshot()),
        )
    }

    private fun snapshot(
        totalStarts: Int = 0,
        initialized: Int = 0,
        running: Int = 0,
        finished: Int = 0,
        error: Int = 0,
        bandwidthText: String = "",
        hasValues: Boolean = totalStarts > 0 || initialized > 0 || running > 0 || finished > 0 || error > 0,
    ) = TrayDownloadSnapshot(totalStarts, initialized, running, finished, error, bandwidthText, hasValues)
}
