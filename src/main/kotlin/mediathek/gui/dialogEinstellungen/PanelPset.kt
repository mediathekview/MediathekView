/*
 * Copyright (c) 2026 derreisende77.
 * This code was developed as part of the MediathekView project https://github.com/mediathekview/MediathekView
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package mediathek.gui.dialogEinstellungen

import mediathek.config.application.ApplicationConfiguration
import mediathek.daten.DatenPset
import mediathek.daten.ProgramSetRepository
import mediathek.gui.dialogEinstellungen.pset.PanelPsetKurz
import mediathek.gui.dialogEinstellungen.pset.PanelPsetLang
import mediathek.tool.ReplacementRules
import java.awt.BorderLayout
import java.util.function.BiConsumer
import javax.swing.JFrame

class PanelPset(
    private val parentComponent: JFrame?,
    private val programSets: ProgramSetRepository,
    private val replacementRules: ReplacementRules,
    private val programSetExporter: BiConsumer<Array<DatenPset>, String>,
) : PanelPsetBase() {
    init {
        configureComponentMetadata()

        val config = ApplicationConfiguration.getInstance()
        jCheckBoxAlleEinstellungen.addActionListener {
            config.programSetShowAllSettings = jCheckBoxAlleEinstellungen.isSelected
            showSelectedProgramSetPanel()
        }
        jCheckBoxAlleEinstellungen.isSelected = config.programSetShowAllSettings
        showSelectedProgramSetPanel()
    }

    private fun configureComponentMetadata() {
        jCheckBoxAlleEinstellungen.name = PanelPsetComponentNames.SHOW_ALL_SETTINGS
        jPanelPset.name = PanelPsetComponentNames.SETTINGS_CONTENT
    }

    /**
     * Einstellungen zum Ansehen und Speichern der Filme anpassen.
     */
    private fun showSelectedProgramSetPanel() {
        jPanelPset.removeAll()
        val settingsPanel = if (jCheckBoxAlleEinstellungen.isSelected) {
            PanelPsetLang(
                parentComponent,
                programSets,
                programSets.list,
                programSetExporter,
                replacementRules,
            )
        } else {
            PanelPsetKurz(parentComponent, programSets.list)
        }
        jPanelPset.add(settingsPanel, BorderLayout.CENTER)
        jPanelPset.updateUI()
    }
}

internal object PanelPsetComponentNames {
    const val SHOW_ALL_SETTINGS = "PanelPset.showAllSettings"
    const val SETTINGS_CONTENT = "PanelPset.settingsContent"
}
