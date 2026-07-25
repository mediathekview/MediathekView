package mediathek.gui.tabs.tab_downloads

import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Assertions.assertInstanceOf
import org.junit.jupiter.api.Test
import javax.swing.ComboBoxModel
import javax.swing.DefaultComboBoxModel
import javax.swing.SwingUtilities

internal class DownloadsFilterControllerTest {
    @Test
    fun `install uses standard models for fixed filter choices`() {
        SwingUtilities.invokeAndWait {
            val toolBar = DownloadsDisplayFilterToolBar()

            DownloadsFilterController(toolBar) {}.install()

            assertInstanceOf(DefaultComboBoxModel::class.java, toolBar.displayCategoriesComboBox.model)
            assertEquals(
                listOf(DisplayFilter.ALL, DisplayFilter.DOWNLOADS_ONLY, DisplayFilter.ABOS_ONLY),
                toolBar.displayCategoriesComboBox.model.elements(),
            )
            assertInstanceOf(DefaultComboBoxModel::class.java, toolBar.viewComboBox.model)
            assertEquals(
                listOf(
                    ViewFilter.ALL,
                    ViewFilter.NOT_STARTED,
                    ViewFilter.STARTED,
                    ViewFilter.WAITING,
                    ViewFilter.RUN_ONLY,
                    ViewFilter.FINISHED_ONLY,
                ),
                toolBar.viewComboBox.model.elements(),
            )
        }
    }

    private fun ComboBoxModel<String>.elements(): List<String> =
        (0 until size).map(::getElementAt)
}
