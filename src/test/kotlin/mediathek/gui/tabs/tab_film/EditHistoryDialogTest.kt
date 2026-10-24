package mediathek.gui.tabs.tab_film

import ca.odell.glazedlists.BasicEventList
import org.junit.jupiter.api.Assertions.*
import org.junit.jupiter.api.Assumptions.assumeFalse
import org.junit.jupiter.api.Test
import java.awt.Component
import java.awt.Container
import java.awt.GraphicsEnvironment
import javax.swing.*

internal class EditHistoryDialogTest {
    @Test
    fun `LRU mode hides move buttons but keeps deletion available`() = withDialog(useLru = true) { dialog, entries ->
        val buttons = dialog.componentsOfType<JButton>()
        assertFalse(buttons.single { it.toolTipText == "Element nach oben verschieben" }.isVisible)
        assertFalse(buttons.single { it.toolTipText == "Element nach unten verschieben" }.isVisible)

        dialog.componentsOfType<JList<*>>().single().selectedIndex = 0
        buttons.single { it.toolTipText == "Ausgewählte Einträge löschen" }.doClick()
        assertEquals(listOf("second"), entries.toList())
    }

    @Test
    fun `manual mode displays move buttons and allows rearranging entries`() = withDialog(useLru = false) { dialog, entries ->
        val buttons = dialog.componentsOfType<JButton>()
        val up = buttons.single { it.toolTipText == "Element nach oben verschieben" }
        val down = buttons.single { it.toolTipText == "Element nach unten verschieben" }
        assertTrue(up.isVisible)
        assertTrue(down.isVisible)

        dialog.componentsOfType<JList<*>>().single().selectedIndex = 1
        up.doClick()
        assertEquals(listOf("second", "first"), entries.toList())
    }

    private fun withDialog(useLru: Boolean, action: (EditHistoryDialog, BasicEventList<String>) -> Unit) {
        assumeFalse(GraphicsEnvironment.isHeadless(), "Swing dialog test requires a display")
        SwingUtilities.invokeAndWait {
            val owner = JFrame()
            val entries = BasicEventList<String>().apply { addAll(listOf("first", "second")) }
            val dialog = EditHistoryDialog(owner, JMenuItem(), entries, useLru)
            try {
                action(dialog, entries)
            } finally {
                dialog.dispose()
                owner.dispose()
                entries.dispose()
            }
        }
    }

    private inline fun <reified T> Container.componentsOfType(): List<T> = allComponents().filterIsInstance<T>()

    private fun Container.allComponents(): List<Component> = components.flatMap {
        listOf(it) + if (it is Container) it.allComponents() else emptyList()
    }
}
