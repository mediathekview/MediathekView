package mediathek.gui.dialogEinstellungen

import mediathek.tool.ReplaceEntry
import mediathek.tool.ReplacementRules
import org.junit.jupiter.api.Assertions.*
import org.junit.jupiter.api.Test
import java.awt.Component
import java.awt.Container
import javax.swing.JButton
import javax.swing.JCheckBox
import javax.swing.JTextField
import javax.swing.SwingUtilities

internal class PanelDateinamenTest {
    @Test
    fun plusButtonKeepsNewReplacementRuleWhenAnotherRuleWasSelected() {
        val rules = ReplacementRules()
        rules.initDefaults()

        onEdt {
            val panel = panel(rules) { ReplaceEntry("alpha", "beta") }
            plusButton(panel).doClick()
        }

        assertEquals(listOf(" ", "alpha"), rules.entries().map { it.from })
        assertEquals(listOf("_", "beta"), rules.entries().map { it.to })
    }

    @Test
    fun plusButtonDoesNotChangeRulesWhenDialogIsCancelled() {
        val rules = ReplacementRules()
        rules.initDefaults()

        onEdt {
            val panel = panel(rules)
            plusButton(panel).doClick()
        }

        assertEquals(listOf(ReplaceEntry(" ", "_")), rules.entries())
    }

    @Test
    fun plusButtonIgnoresRejectedEmptyReplacementRule() {
        val rules = ReplacementRules()

        onEdt {
            val panel = panel(rules) { ReplaceEntry("", "beta") }
            plusButton(panel).doClick()
        }

        assertEquals(emptyList<ReplaceEntry>(), rules.entries())
    }

    @Test
    fun minusButtonRemovesSelectedReplacementRule() {
        val rules = ReplacementRules()
        rules.initDefaults()

        onEdt {
            val panel = panel(rules)
            button(panel, COMPONENT_REMOVE_RULE).doClick()
        }

        assertEquals(emptyList<ReplaceEntry>(), rules.entries())
    }

    @Test
    fun downButtonMovesSelectedReplacementRule() {
        val rules = ReplacementRules()
        rules.initDefaults()
        rules.add("alpha", "beta")

        onEdt {
            val panel = panel(rules)
            button(panel, COMPONENT_MOVE_RULE_DOWN).doClick()
        }

        assertEquals(listOf("alpha", " "), rules.entries().map { it.from })
        assertEquals(listOf("beta", "_"), rules.entries().map { it.to })
    }

    @Test
    fun resetButtonRestoresDefaultReplacementRules() {
        val rules = ReplacementRules()
        rules.add("alpha", "beta")

        onEdt {
            val panel = panel(rules)
            button(panel, COMPONENT_RESET_RULES).doClick()
        }

        assertEquals(listOf(ReplaceEntry(" ", "_")), rules.entries())
    }

    @Test
    fun editingToFieldUpdatesSelectedReplacementRule() {
        val rules = ReplacementRules()
        rules.initDefaults()

        onEdt {
            val panel = panel(rules)
            val field = textField(panel, COMPONENT_REPLACEMENT_TO)
            field.document.insertString(field.document.length, "suffix", null)
        }

        assertEquals(listOf(ReplaceEntry(" ", "_suffix")), rules.entries())
    }

    @Test
    fun editingFromFieldUpdatesSelectedReplacementRule() {
        val rules = ReplacementRules()
        rules.initDefaults()

        onEdt {
            val panel = panel(rules)
            val field = textField(panel, COMPONENT_REPLACEMENT_FROM)
            field.document.insertString(field.document.length, "x", null)
        }

        assertEquals(listOf(ReplaceEntry(" x", "_")), rules.entries())
    }

    @Test
    fun replacingFromFieldPersistsSoleReplacementRule() {
        val rules = ReplacementRules()
        rules.initDefaults()

        onEdt {
            val panel = panel(rules)
            textField(panel, COMPONENT_REPLACEMENT_FROM).apply {
                selectAll()
                replaceSelection("alpha")
            }
        }

        assertEquals(listOf(ReplaceEntry("alpha", "_")), rules.entries())
    }

    @Test
    fun replacingFromFieldDoesNotEditAdjacentReplacementRule() {
        val rules = ReplacementRules()
        rules.add("first", "1")
        rules.add("second", "2")

        onEdt {
            val panel = panel(rules)
            textField(panel, COMPONENT_REPLACEMENT_FROM).apply {
                selectAll()
                replaceSelection("changed")
            }
        }

        assertEquals(
            listOf(ReplaceEntry("changed", "1"), ReplaceEntry("second", "2")),
            rules.entries(),
        )
    }

    @Test
    fun clearingSoleFromFieldRemovesReplacementRule() {
        val rules = ReplacementRules()
        rules.initDefaults()

        onEdt {
            val panel = panel(rules)
            textField(panel, COMPONENT_REPLACEMENT_FROM).apply {
                selectAll()
                replaceSelection("")
            }
        }

        assertEquals(emptyList<ReplaceEntry>(), rules.entries())
    }

    @Test
    fun clearingFromFieldSelectsAndDisplaysAdjacentReplacementRule() {
        val rules = ReplacementRules()
        rules.add("first", "1")
        rules.add("second", "2")
        lateinit var panel: PanelDateinamen

        onEdt {
            panel = panel(rules)
            textField(panel, COMPONENT_REPLACEMENT_FROM).apply {
                selectAll()
                replaceSelection("")
            }
        }
        onEdt { }

        assertEquals(listOf(ReplaceEntry("second", "2")), rules.entries())
        onEdt {
            assertEquals("second", textField(panel, COMPONENT_REPLACEMENT_FROM).text)
            assertEquals("2", textField(panel, COMPONENT_REPLACEMENT_TO).text)
        }
    }

    @Test
    fun removingFinalRuleDisablesRuleEditors() {
        val rules = ReplacementRules()
        rules.initDefaults()

        onEdt {
            val panel = panel(rules)
            button(panel, COMPONENT_REMOVE_RULE).doClick()

            assertFalse(textField(panel, COMPONENT_REPLACEMENT_FROM).isEnabled)
            assertFalse(textField(panel, COMPONENT_REPLACEMENT_TO).isEnabled)
            assertFalse(button(panel, COMPONENT_MOVE_RULE_DOWN).isEnabled)
        }
    }

    @Test
    fun checkboxesReflectFilenameSettings() {
        val settings = TestFilenameSettings(
            useFilenameReplaceTable = true,
            onlyAsciiFilenames = false,
        )

        onEdt {
            val panel = panel(ReplacementRules(), settings = settings)

            assertTrue(checkBox(panel, COMPONENT_USE_REPLACEMENT_TABLE).isSelected)
            assertFalse(checkBox(panel, COMPONENT_ONLY_ASCII).isSelected)
        }
    }

    @Test
    fun checkboxClicksUpdateFilenameSettings() {
        val settings = TestFilenameSettings()

        onEdt {
            val panel = panel(ReplacementRules(), settings = settings)
            checkBox(panel, COMPONENT_USE_REPLACEMENT_TABLE).doClick()
            checkBox(panel, COMPONENT_ONLY_ASCII).doClick()
        }

        assertTrue(settings.useFilenameReplaceTable)
        assertTrue(settings.onlyAsciiFilenames)
    }

    private fun panel(
        rules: ReplacementRules,
        settings: FilenameSettings = TestFilenameSettings(),
        addDialog: (Component) -> ReplaceEntry? = { null },
    ): PanelDateinamen = PanelDateinamen(rules, settings, addDialog)

    private fun plusButton(panel: PanelDateinamen): JButton = button(panel, COMPONENT_ADD_RULE)

    private fun button(panel: PanelDateinamen, name: String): JButton = component(panel, name)

    private fun checkBox(panel: PanelDateinamen, name: String): JCheckBox = component(panel, name)

    private fun textField(panel: PanelDateinamen, name: String): JTextField = component(panel, name)

    private inline fun <reified T : Component> component(panel: PanelDateinamen, name: String): T =
        panel.descendants()
            .filterIsInstance<T>()
            .first { it.name == name }

    private fun Component.descendants(): Sequence<Component> = sequence {
        yield(this@descendants)
        if (this@descendants is Container) {
            this@descendants.components.forEach { child -> yieldAll(child.descendants()) }
        }
    }

    private fun onEdt(action: () -> Unit) {
        if (SwingUtilities.isEventDispatchThread()) {
            action()
        } else {
            SwingUtilities.invokeAndWait(action)
        }
    }

    private data class TestFilenameSettings(
        override var useFilenameReplaceTable: Boolean = false,
        override var onlyAsciiFilenames: Boolean = false,
    ) : FilenameSettings

    private companion object {
        private const val COMPONENT_RESET_RULES = PanelDateinamenComponentNames.RESET_RULES
        private const val COMPONENT_ADD_RULE = PanelDateinamenComponentNames.ADD_RULE
        private const val COMPONENT_REMOVE_RULE = PanelDateinamenComponentNames.REMOVE_RULE
        private const val COMPONENT_MOVE_RULE_DOWN = PanelDateinamenComponentNames.MOVE_RULE_DOWN
        private const val COMPONENT_REPLACEMENT_FROM = PanelDateinamenComponentNames.REPLACEMENT_FROM
        private const val COMPONENT_REPLACEMENT_TO = PanelDateinamenComponentNames.REPLACEMENT_TO
        private const val COMPONENT_USE_REPLACEMENT_TABLE = PanelDateinamenComponentNames.USE_REPLACEMENT_TABLE
        private const val COMPONENT_ONLY_ASCII = PanelDateinamenComponentNames.ONLY_ASCII
    }
}
