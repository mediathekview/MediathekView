package mediathek.mainwindow

import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Assertions.assertNull
import org.junit.jupiter.api.Test
import javax.swing.JPanel
import javax.swing.JTabbedPane

class ToggleOnlineSearchTabActionTest {
    @Test
    fun `reenabling Zapp tab keeps online search before Zapp`() {
        val tabbedPane = JTabbedPane().apply {
            addTab("Filme", JPanel())
            addTab("Downloads", JPanel())
            addTab("Onlinesuche", JPanel())
        }
        var createdPanels = 0
        val zappTab = MainWindowTab("zapp Livestreams", { JPanel().also { createdPanels++ } })
        val action = ToggleZappLivestreamsTabAction(tabbedPane, zappTab)

        assertNull(zappTab.existingComponent())
        assertEquals(0, createdPanels)

        action.actionPerformed(null)

        assertEquals(1, createdPanels)
        assertEquals(
            listOf("Filme", "Downloads", "Onlinesuche", "zapp Livestreams"),
            tabbedPane.tabTitles(),
        )
    }
}

private fun JTabbedPane.tabTitles(): List<String> = (0 until tabCount).map(::getTitleAt)
