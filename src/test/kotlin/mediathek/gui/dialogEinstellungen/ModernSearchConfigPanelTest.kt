package mediathek.gui.dialogEinstellungen

import mediathek.config.application.ApplicationConfiguration
import mediathek.gui.dialogEinstellungen.allgemein.ModernSearchConfigPanel
import mediathek.gui.dialogEinstellungen.allgemein.SearchConfigPanel
import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Assertions.assertFalse
import org.junit.jupiter.api.Test
import javax.swing.JCheckBox
import javax.swing.SwingUtilities

internal class ModernSearchConfigPanelTest {
    @Test
    fun `history checkbox reflects and updates the persisted choice`() {
        val config = ApplicationConfiguration.getInstance()
        val previous = config.useLruSearchHistory
        try {
            for (initial in listOf(false, true)) {
                config.useLruSearchHistory = initial
                SwingUtilities.invokeAndWait {
                    val panel = SearchConfigPanel()
                    val modernSearch = panel.components.filterIsInstance<ModernSearchConfigPanel>().single()
                    assertFalse(modernSearch.components.any { it.name == "useLruSearchHistory" })
                    val checkbox = panel.components.filterIsInstance<JCheckBox>()
                        .single { it.name == "useLruSearchHistory" }
                    assertEquals(initial, checkbox.isSelected)
                    checkbox.doClick()
                    assertEquals(!initial, config.useLruSearchHistory)
                }
            }
        } finally {
            config.useLruSearchHistory = previous
        }
    }
}
