package mediathek.audiothek.ui.main

import com.formdev.flatlaf.FlatDarkLaf
import com.formdev.flatlaf.FlatLightLaf
import mediathek.swing.IconUtils
import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Test
import org.kordamp.ikonli.swing.FontIcon
import javax.swing.JButton
import javax.swing.SwingUtilities
import javax.swing.UIManager

class AudiothekToolBarTest {
    @Test
    fun `reload and download icons follow look and feel changes`() {
        SwingUtilities.invokeAndWait {
            val previousLookAndFeel = UIManager.getLookAndFeel()
            try {
                UIManager.setLookAndFeel(FlatLightLaf())
                val toolBar = AudiothekToolBar()
                val toolbarIcons = toolBar.components
                    .filterIsInstance<JButton>()
                    .mapNotNull { it.icon as? FontIcon }

                assertEquals(2, toolbarIcons.size)
                toolbarIcons.forEach {
                    assertEquals(IconUtils.DEFAULT_LIGHT_COLOR, it.iconColor)
                }

                UIManager.setLookAndFeel(FlatDarkLaf())

                toolbarIcons.forEach {
                    assertEquals(IconUtils.DEFAULT_DARK_COLOR, it.iconColor)
                }
            } finally {
                UIManager.setLookAndFeel(previousLookAndFeel)
            }
        }
    }
}
