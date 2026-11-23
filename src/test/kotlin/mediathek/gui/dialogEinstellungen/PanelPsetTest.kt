package mediathek.gui.dialogEinstellungen

import ca.odell.glazedlists.swing.AdvancedTableModel
import mediathek.config.Daten
import mediathek.config.application.ApplicationConfiguration
import mediathek.daten.DatenProg
import mediathek.daten.DatenPset
import mediathek.daten.ProgramSetRepository
import mediathek.gui.dialogEinstellungen.pset.PanelPsetKurz
import mediathek.gui.dialogEinstellungen.pset.PanelPsetLang
import mediathek.mainwindow.SettingsDialogHost
import mediathek.swing.SwingDispatch
import mediathek.tool.ReplacementRules
import org.junit.jupiter.api.Assertions.*
import org.junit.jupiter.api.Assumptions.assumeFalse
import org.junit.jupiter.api.Test
import org.mockito.Mockito.mock
import java.awt.Component
import java.awt.Container
import java.awt.GraphicsEnvironment
import javax.swing.JCheckBox
import javax.swing.JTable
import javax.swing.JTree
import javax.swing.RowSorter
import javax.swing.SortOrder
import javax.swing.tree.DefaultMutableTreeNode
import javax.swing.tree.TreePath

internal class PanelPsetTest {
    @Test
    fun compactSettingsPanelIsShownWhenAllSettingsAreDisabled() {
        withShowAllSettings(false) {
            SwingDispatch.runAndWait("Inspect compact program-set settings") {
                val panel = createPanel()
                val child = panel.descendants().filterIsInstance<PanelPsetKurz>().single()
                try {
                    assertFalse(panel.showAllSettingsCheckBox().isSelected)
                } finally {
                    child.removeNotify()
                }
            }
        }
    }

    @Test
    fun detailedSettingsPanelIsShownWhenAllSettingsAreEnabled() {
        withShowAllSettings(true) {
            SwingDispatch.runAndWait("Inspect detailed program-set settings") {
                val panel = createPanel()
                val child = panel.descendants().filterIsInstance<PanelPsetLang>().single()
                try {
                    assertTrue(panel.showAllSettingsCheckBox().isSelected)
                } finally {
                    child.removeNotify()
                }
            }
        }
    }

    @Test
    fun checkboxClickPersistsSettingAndSwitchesVisiblePanel() {
        withShowAllSettings(false) { config ->
            SwingDispatch.runAndWait("Switch visible program-set settings panel") {
                val panel = createPanel()
                val compactPanel = panel.descendants().filterIsInstance<PanelPsetKurz>().single()
                val checkBox = panel.showAllSettingsCheckBox()
                try {
                    checkBox.doClick()
                    val detailedPanel = panel.descendants().filterIsInstance<PanelPsetLang>().single()
                    try {
                        assertTrue(config.programSetShowAllSettings)
                        assertTrue(checkBox.isSelected)
                    } finally {
                        detailedPanel.removeNotify()
                    }
                } finally {
                    compactPanel.removeNotify()
                }
            }
        }
    }

    @Test
    fun switchingAwayFromAdvancedSettingsDisposesTheProgramTableModel() {
        withShowAllSettings(true) {
            SwingDispatch.runAndWait {
                val repository = ProgramSetRepository()
                val programSet = DatenPset("Speichern").apply { addProg(DatenProg()) }
                repository.list.add(programSet)
                val panel = createPanel(repository)
                panel.addNotify()
                try {
                    val oldTable = panel.programTable()
                    val oldModel = oldTable.model as AdvancedTableModel<*>
                    var oldModelEvents = 0
                    oldModel.addTableModelListener { oldModelEvents++ }

                    panel.showAllSettingsCheckBox().doClick()
                    panel.showAllSettingsCheckBox().doClick()
                    val newTable = panel.programTable()
                    val initialRowCount = newTable.rowCount
                    programSet.addProg(DatenProg())

                    assertEquals(0, oldModelEvents)
                    assertThrows(IllegalStateException::class.java) { oldModel.rowCount }
                    assertNotSame(oldModel, oldTable.model)
                    assertNull(oldTable.rowSorter)
                    assertEquals(initialRowCount + 1, newTable.rowCount)
                } finally {
                    panel.removeNotify()
                }
            }
        }
    }

    @Test
    fun reopeningSettingsRebindsTheAdvancedProgramTable() {
        assumeFalse(GraphicsEnvironment.isHeadless(), "Swing dialog test requires a display")
        withShowAllSettings(true) {
            SwingDispatch.runAndWait {
                val daten = Daten()
                val program = DatenProg().apply { name = "Player" }
                val programSet = DatenPset("Abspielen").apply { addProg(program) }
                daten.programSets.list.add(DatenPset("Speichern"))
                daten.programSets.list.add(programSet)
                val dialog = DialogEinstellungen(
                    mock(SettingsDialogHost::class.java),
                    daten.programSets,
                    daten.filmCatalog,
                    daten.filmListLoader,
                    daten.blacklist,
                    daten.replacementRules,
                    daten.configurationPersistence,
                ) { _, _ -> }
                try {
                    dialog.isVisible = true
                    val tree = dialog.descendants().filterIsInstance<JTree>().single()
                    val page = (tree.model.root as DefaultMutableTreeNode).breadthFirstEnumeration()
                        .asSequence().single { it.toString() == "Set bearbeiten" } as DefaultMutableTreeNode
                    tree.selectionPath = TreePath(page.path)
                    val panel = dialog.descendants().filterIsInstance<PanelPset>().single()
                    val setTable = panel.advancedSettings().programSetTable()
                    setTable.setRowSelectionInterval(1, 1)
                    val table = panel.programTable()
                    val oldModel = table.model as AdvancedTableModel<*>
                    val sortKeys = listOf(RowSorter.SortKey(DatenProg.PROGRAMM_NAME, SortOrder.ASCENDING))
                    table.rowSorter.sortKeys = sortKeys

                    dialog.dispose()
                    assertThrows(IllegalStateException::class.java) { oldModel.rowCount }
                    programSet.addProg(DatenProg().apply { name = "Alternate player" })
                    dialog.isVisible = true

                    assertSame(table, dialog.descendants().filterIsInstance<PanelPset>().single().programTable())
                    assertNotSame(oldModel, table.model)
                    assertEquals(1, setTable.selectedRow)
                    assertEquals(sortKeys, table.rowSorter.sortKeys)
                    assertEquals(2, table.rowCount)
                    assertEquals("Player", table.model.getValueAt(table.convertRowIndexToModel(table.selectedRow), DatenProg.PROGRAMM_NAME))
                    programSet.addProg(DatenProg())
                    assertEquals(3, table.rowCount)
                } finally {
                    dialog.dispose()
                }
            }
        }
    }

    private fun createPanel(repository: ProgramSetRepository = ProgramSetRepository()): PanelPset =
        PanelPset(
            null,
            repository,
            ReplacementRules(),
        ) { _: Array<DatenPset>, _: String -> }

    private fun withShowAllSettings(
        enabled: Boolean,
        action: (ApplicationConfiguration) -> Unit,
    ) {
        val config = ApplicationConfiguration.getInstance()
        val previousValue = config.programSetShowAllSettings
        config.programSetShowAllSettings = enabled
        try {
            action(config)
        } finally {
            config.programSetShowAllSettings = previousValue
        }
    }

    private fun PanelPset.showAllSettingsCheckBox(): JCheckBox =
        descendants().filterIsInstance<JCheckBox>().single { it.name == PanelPsetComponentNames.SHOW_ALL_SETTINGS }

    private fun PanelPset.advancedSettings(): PanelPsetLang =
        descendants().filterIsInstance<PanelPsetLang>().single()

    private fun PanelPset.programTable(): JTable = advancedSettings().helperProgramTable()

    private fun Component.descendants(): Sequence<Component> = sequence {
        yield(this@descendants)
        if (this@descendants is Container) {
            this@descendants.components.forEach { child -> yieldAll(child.descendants()) }
        }
    }
}
