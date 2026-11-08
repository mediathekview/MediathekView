package mediathek.gui.dialog

import mediathek.daten.ProgramSetRepository
import mediathek.gui.dialog.reset.ResetSettingsDialog
import mediathek.gui.dialog.reset.ResetSettingsPanel
import mediathek.mainwindow.SettingsResetHost
import org.junit.jupiter.api.Assertions.*
import org.junit.jupiter.api.Assumptions.assumeFalse
import org.junit.jupiter.api.Test
import java.awt.BorderLayout
import java.awt.GraphicsEnvironment
import javax.swing.JDialog
import javax.swing.JFrame
import javax.swing.JPanel
import javax.swing.SwingUtilities

internal class StandardCloseDialogTest {
    @Test
    fun `supplied content is installed and close button disposes the dialog`() = onEdt {
        val content = JPanel()
        val dialog = StandardCloseDialog(null, "Test", true, content)
        try {
            val layout = dialog.contentPane.layout as BorderLayout
            assertSame(content, layout.getLayoutComponent(BorderLayout.CENTER))
            assertEquals(JDialog.DISPOSE_ON_CLOSE, dialog.defaultCloseOperation)
            assertTrue(dialog.isModal)
            assertTrue(dialog.isDisplayable)

            requireNotNull(dialog.rootPane.defaultButton).doClick()
            assertFalse(dialog.isDisplayable)
        } finally {
            dialog.dispose()
        }
    }

    @Test
    fun `reset dialog constructs its content with initialized dependencies`() = onEdt {
        val owner = JFrame()
        val host = object : SettingsResetHost {
            override fun ownerFrame(): JFrame = owner
            override fun requestSettingsResetOnQuit() = error("Construction must not request a reset")
            override fun quitApplication(): Boolean = error("Construction must not quit")
        }
        try {
            val dialog = ResetSettingsDialog(host, ProgramSetRepository()) { _, _ ->
                error("Construction must not export program sets")
            }
            try {
                val layout = dialog.contentPane.layout as BorderLayout
                assertInstanceOf(ResetSettingsPanel::class.java, layout.getLayoutComponent(BorderLayout.CENTER))
                assertSame(owner, dialog.owner)
                assertTrue(dialog.isModal)
                assertFalse(dialog.isResizable)
                assertFalse(dialog.isVisible)
            } finally {
                dialog.dispose()
            }
        } finally {
            owner.dispose()
        }
    }

    private fun onEdt(action: () -> Unit) {
        assumeFalse(GraphicsEnvironment.isHeadless(), "Swing dialog test requires a display")
        SwingUtilities.invokeAndWait(action)
    }
}
