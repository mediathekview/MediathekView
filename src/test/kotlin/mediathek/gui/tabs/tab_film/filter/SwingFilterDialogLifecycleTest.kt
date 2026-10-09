package mediathek.gui.tabs.tab_film.filter

import mediathek.gui.tabs.tab_film.filter.SwingFilterDialogTestFixture.assumeUiAvailable
import mediathek.gui.tabs.tab_film.filter.SwingFilterDialogTestFixture.createDialogSetup
import mediathek.gui.tabs.tab_film.filter.SwingFilterDialogTestFixture.onEdt
import org.junit.jupiter.api.Test

internal class SwingFilterDialogLifecycleTest {
    @Test
    fun `disposing dialog with immutable sender model is idempotent`() {
        assumeUiAvailable()
        val setup = createDialogSetup()

        onEdt {
            setup.dialog.dispose()
            setup.dialog.dispose()
            setup.model.close()
        }
    }
}
