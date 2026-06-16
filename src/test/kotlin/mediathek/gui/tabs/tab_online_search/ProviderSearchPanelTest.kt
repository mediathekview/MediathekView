package mediathek.gui.tabs.tab_online_search

import com.formdev.flatlaf.FlatClientProperties
import org.junit.jupiter.api.Assertions.*
import org.junit.jupiter.api.Test
import javax.swing.JButton
import javax.swing.JComboBox
import javax.swing.JMenuItem

class ProviderSearchPanelTest {
    @Test
    fun `search is disabled for too short query`() {
        val panel = ProviderSearchPanel()

        panel.queryText = "ab"
        panel.updateRunningState(running = false)

        assertFalse(panel.searchButton.isEnabled)
    }

    @Test
    fun `search is enabled for long enough query`() {
        val panel = ProviderSearchPanel()

        panel.queryText = "tatort"
        panel.updateRunningState(running = false)

        assertTrue(panel.searchButton.isEnabled)
    }

    @Test
    fun `panel does not expose a manual continue button`() {
        val panel = ProviderSearchPanel()

        assertEquals(0, panel.components.count { it is JButton && it.text == "Weitersuchen" })
    }

    @Test
    fun `uses text fields with integrated clear buttons instead of combo boxes and leeren buttons`() {
        val panel = ProviderSearchPanel()

        assertTrue(panel.queryField.getClientProperty(FlatClientProperties.TEXT_FIELD_SHOW_CLEAR_BUTTON) as Boolean)
        assertTrue(panel.urlField.getClientProperty(FlatClientProperties.TEXT_FIELD_SHOW_CLEAR_BUTTON) as Boolean)
        assertEquals(0, panel.components.count { it is JComboBox<*> })
        assertEquals(0, panel.components.count { it is JButton && it.text == "Leeren" })
    }

    @Test
    fun `search fields do not grow wider than five hundred pixels`() {
        val panel = ProviderSearchPanel()

        panel.setSize(1_000, 300)
        panel.doLayout()

        assertTrue(panel.queryField.width <= 500)
        assertTrue(panel.urlField.width <= 500)
    }

    @Test
    fun `buttons follow directly after search fields`() {
        val panel = ProviderSearchPanel()

        panel.setSize(1_000, 300)
        panel.doLayout()

        assertTrue(panel.searchButton.x - (panel.queryField.x + panel.queryField.width) <= 12)
        assertTrue(panel.urlSearchButton.x - (panel.urlField.x + panel.urlField.width) <= 12)
    }

    @Test
    fun `running search enables cancel button and disables start buttons`() {
        val panel = ProviderSearchPanel()

        panel.queryText = "tatort"
        panel.urlText = "https://example.invalid/video"
        panel.updateRunningState(running = true)

        assertTrue(panel.cancelButton.isEnabled)
        assertFalse(panel.searchButton.isEnabled)
        assertFalse(panel.urlSearchButton.isEnabled)
    }

    @Test
    fun `selecting query history copies entry and notifies listener`() {
        val panel = ProviderSearchPanel()
        var selected: String? = null
        panel.setQueryHistory(listOf("tatort"))
        panel.addQueryHistorySelectionListener { selected = it }

        assertTrue(panel.queryField.selectHistoryEntry("tatort"))

        assertEquals("tatort", panel.queryText)
        assertEquals("tatort", selected)
    }

    @Test
    fun `selecting url history copies entry and notifies listener`() {
        val panel = ProviderSearchPanel()
        val url = "https://example.invalid/video"
        var selected: String? = null
        panel.setUrlHistory(listOf(url))
        panel.addUrlHistorySelectionListener { selected = it }

        assertTrue(panel.urlField.selectHistoryEntry(url))

        assertEquals(url, panel.urlText)
        assertEquals(url, selected)
    }

    @Test
    fun `history popup starts with clear and edit items`() {
        val panel = ProviderSearchPanel()
        panel.setQueryHistory(listOf("tatort"))

        val popup = panel.queryField.createHistoryPopup()

        assertEquals("Alles löschen", (popup.getComponent(0) as JMenuItem).text)
        assertEquals("Einträge bearbeiten", (popup.getComponent(1) as JMenuItem).text)
        assertEquals("tatort", (popup.getComponent(3) as JMenuItem).text)
    }

    @Test
    fun `clear history menu item removes entries and notifies listener`() {
        val panel = ProviderSearchPanel()
        var historyEntries: List<String>? = null
        panel.queryField.addHistoryChangeListener { historyEntries = it }
        panel.setQueryHistory(listOf("tatort"))

        val popup = panel.queryField.createHistoryPopup()
        (popup.getComponent(0) as JMenuItem).doClick()

        assertEquals(emptyList<String>(), historyEntries)
        assertFalse(panel.queryField.selectHistoryEntry("tatort"))
    }
}
