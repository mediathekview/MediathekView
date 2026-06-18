package mediathek.mainwindow

import org.apache.logging.log4j.LogManager
import javax.swing.JMenu
import javax.swing.JTabbedPane

class MainWindowTabRegistry(
    private val tabbedPane: JTabbedPane,
) {
    private val tabs = mutableListOf<MainWindowTab>()

    fun register(tab: MainWindowTab) {
        tabs += tab
    }

    fun installVisibleTabs() {
        tabs.filter { it.visible.asBoolean }
            .forEach { tabbedPane.addTab(it.title, it.component) }
    }

    fun configureIcons(showIcons: Boolean) {
        tabs.forEach { tab ->
            val index = tabbedPane.indexOfComponent(tab.component)
            if (index >= 0) {
                tabbedPane.setIconAt(index, if (showIcons) tab.icon?.get() else null)
            }
        }
    }

    fun installViewMenuEntries(menu: JMenu) {
        tabs.mapNotNull(MainWindowTab::toggleAction)
            .forEach(menu::add)
    }

    fun disposeTabs() {
        tabs.forEach { tab ->
            try {
                tab.dispose.run()
            } catch (ex: RuntimeException) {
                logger.error("Could not dispose main window tab: {}", tab.title, ex)
            }
        }
    }

    companion object {
        private val logger = LogManager.getLogger(MainWindowTabRegistry::class.java)
    }
}
