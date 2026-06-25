package mediathek.mainwindow

import org.junit.jupiter.api.Assertions.*
import org.junit.jupiter.api.Test
import javax.swing.JPanel
import javax.swing.JTabbedPane

class MainWindowTabRegistryTest {
    @Test
    fun `visible tab can be installed with placeholder and materialized when selected`() {
        val tabbedPane = JTabbedPane()
        val registry = MainWindowTabRegistry(tabbedPane)
        val firstComponent = JPanel()
        val placeholder = JPanel()
        val realComponent = JPanel()
        var realComponentCreations = 0
        val firstTab = MainWindowTab("First", { firstComponent })
        val deferredTab = MainWindowTab(
            "Deferred",
            {
                realComponentCreations++
                realComponent
            },
            initialComponentFactory = { placeholder },
        )

        registry.register(firstTab)
        registry.register(deferredTab)
        registry.installVisibleTabs()

        assertSame(firstComponent, tabbedPane.getComponentAt(0))
        assertSame(placeholder, tabbedPane.getComponentAt(1))
        assertNull(deferredTab.existingComponent())
        assertEquals(0, realComponentCreations)

        tabbedPane.selectedIndex = 1
        registry.materializeSelectedTab()

        assertSame(realComponent, tabbedPane.getComponentAt(1))
        assertSame(realComponent, deferredTab.existingComponent())
        assertEquals(1, realComponentCreations)
    }

    @Test
    fun `selected tab materializer materializes deferred tab on selection change`() {
        val tabbedPane = JTabbedPane()
        val registry = MainWindowTabRegistry(tabbedPane)
        val placeholder = JPanel()
        val realComponent = JPanel()
        val firstTab = MainWindowTab("First", { JPanel() })
        val deferredTab = MainWindowTab("Deferred", { realComponent }, initialComponentFactory = { placeholder })

        registry.register(firstTab)
        registry.register(deferredTab)
        registry.installVisibleTabs()
        registry.installSelectedTabMaterializer()

        tabbedPane.selectedIndex = 1

        assertSame(realComponent, tabbedPane.getComponentAt(1))
    }

    @Test
    fun `selected tab materializer notifies deferred tab when real component is shown`() {
        val tabbedPane = JTabbedPane()
        val registry = MainWindowTabRegistry(tabbedPane)
        val placeholder = JPanel()
        val realComponent = JPanel()
        var selectedNotifications = 0
        val firstTab = MainWindowTab("First", { JPanel() })
        val deferredTab = MainWindowTab(
            "Deferred",
            { realComponent },
            initialComponentFactory = { placeholder },
            onComponentSelected = { selectedNotifications++ },
        )

        registry.register(firstTab)
        registry.register(deferredTab)
        registry.installVisibleTabs()
        registry.installSelectedTabMaterializer()

        tabbedPane.selectedIndex = 1

        assertSame(realComponent, tabbedPane.getComponentAt(1))
        assertEquals(1, selectedNotifications)
    }

    @Test
    fun `selected tab materializer notifies selected tab that is already materialized`() {
        val tabbedPane = JTabbedPane()
        val registry = MainWindowTabRegistry(tabbedPane)
        val realComponent = JPanel()
        var selectedNotifications = 0
        val tab = MainWindowTab(
            "Materialized",
            { realComponent },
            onComponentSelected = { selectedNotifications++ },
        )

        registry.register(tab)
        registry.installVisibleTabs()

        registry.materializeSelectedTab()

        assertSame(realComponent, tabbedPane.getComponentAt(0))
        assertEquals(2, selectedNotifications)
    }
}
