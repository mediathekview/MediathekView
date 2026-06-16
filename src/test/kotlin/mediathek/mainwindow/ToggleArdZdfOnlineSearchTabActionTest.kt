package mediathek.mainwindow

import mediathek.gui.tabs.tab_livestreams.LivestreamPanel
import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Test
import javax.swing.JPanel
import javax.swing.JTabbedPane

class ToggleArdZdfOnlineSearchTabActionTest {
    @Test
    fun `reenabling Zapp tab keeps online search before Zapp`() {
        val tabbedPane = JTabbedPane().apply {
            addTab("Filme", JPanel())
            addTab("Downloads", JPanel())
            addTab("Onlinesuche", JPanel())
        }
        val action = ToggleZappLivestreamsTabAction(tabbedPane, LivestreamPanel())

        action.actionPerformed(null)

        assertEquals(
            listOf("Filme", "Downloads", "Onlinesuche", "zapp Livestreams"),
            tabbedPane.tabTitles(),
        )
    }
}

private fun JTabbedPane.tabTitles(): List<String> = (0 until tabCount).map(::getTitleAt)
