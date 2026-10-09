package mediathek.gui.dialogEinstellungen

import mediathek.config.application.ApplicationConfiguration
import mediathek.daten.DatenPset
import mediathek.daten.ProgramSetRepository
import mediathek.gui.dialogEinstellungen.pset.PanelPsetKurz
import mediathek.gui.dialogEinstellungen.pset.PanelPsetLang
import mediathek.swing.SwingDispatch
import mediathek.tool.ReplacementRules
import org.junit.jupiter.api.Assertions.assertFalse
import org.junit.jupiter.api.Assertions.assertTrue
import org.junit.jupiter.api.Test
import java.awt.Component
import java.awt.Container
import javax.swing.JCheckBox

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

    private fun createPanel(): PanelPset =
        PanelPset(
            null,
            ProgramSetRepository(),
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

    private fun Component.descendants(): Sequence<Component> = sequence {
        yield(this@descendants)
        if (this@descendants is Container) {
            this@descendants.components.forEach { child -> yieldAll(child.descendants()) }
        }
    }
}
