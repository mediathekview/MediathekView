package ca.odell.glazedlists.swing

import ca.odell.glazedlists.BasicEventList
import org.junit.jupiter.api.Assertions.*
import org.junit.jupiter.api.Test
import javax.swing.SwingUtilities
import javax.swing.undo.UndoManager

internal class UndoSupportBehaviorTest {
    @Test
    fun defaultAdapterTracksUndoRedoAndUninstallStopsTracking() = onEdt {
        val source = BasicEventList<String>()
        val undoManager = UndoManager()
        val support = UndoSupport.install(undoManager, source)

        source += "value"
        assertTrue(undoManager.canUndo())
        undoManager.undo()
        assertEquals(emptyList<String>(), source)
        assertTrue(undoManager.canRedo())
        undoManager.redo()
        assertEquals(listOf("value"), source)

        support.uninstall()
        undoManager.discardAllEdits()
        source += "after uninstall"
        assertFalse(undoManager.canUndo())
    }

    @Test
    fun installationAndUninstallationRequireTheEventDispatchThread() {
        val source = BasicEventList<String>()
        val installFailure = assertThrows(IllegalStateException::class.java) {
            UndoSupport.install(UndoManager(), source)
        }
        assertEquals(
            "UndoRedoSupport must be accessed from the Swing Event Dispatch Thread, but was called on Thread \"${Thread.currentThread().name}\"",
            installFailure.message,
        )

        lateinit var support: UndoSupport<String>
        onEdt { support = UndoSupport.install(UndoManager(), source) }
        val uninstallFailure = assertThrows(IllegalStateException::class.java, support::uninstall)
        assertEquals(
            "UndoRedoSupport must be accessed from the Swing Event Dispatch Thread, but was called on Thread \"${Thread.currentThread().name}\"",
            uninstallFailure.message,
        )
        onEdt(support::uninstall)
    }

    private fun onEdt(action: () -> Unit) {
        if (SwingUtilities.isEventDispatchThread()) action() else SwingUtilities.invokeAndWait(action)
    }
}
