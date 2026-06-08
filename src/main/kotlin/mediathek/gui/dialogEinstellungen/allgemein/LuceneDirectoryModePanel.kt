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

package mediathek.gui.dialogEinstellungen.allgemein

import com.jidesoft.swing.MultilineLabel
import mediathek.daten.LuceneDirectoryMode
import mediathek.tool.ApplicationConfiguration
import net.miginfocom.layout.AC
import net.miginfocom.layout.CC
import net.miginfocom.layout.LC
import net.miginfocom.swing.MigLayout
import org.apache.commons.configuration2.Configuration
import java.awt.FlowLayout
import javax.swing.JComboBox
import javax.swing.JLabel
import javax.swing.JPanel
import javax.swing.SwingConstants
import javax.swing.UIManager

class LuceneDirectoryModePanel : JPanel() {
    private val config: Configuration = ApplicationConfiguration.getConfiguration()
    private val modeComboBox = JComboBox(LuceneDirectoryMode.entries.toTypedArray())
    private val descriptionLabel = JLabel()
    private val restartWarningIconLabel = JLabel(UIManager.getIcon("OptionPane.warningIcon"))
    private val restartWarningTextLabel = JLabel("<html><b>Neustart notwendig</b></html>")
    private val infoLabel = MultilineLabel(
        "Wählen Sie, welche Lucene-Directory-Implementierung verwendet wird.\n" +
            "Die Einstellung beeinflusst Dateiverhalten und Suchperformance.",
    )
    private var initializing = false

    init {
        initComponents()
        initializing = true
        setSelectedMode(config.getString(ApplicationConfiguration.LUCENE_DIRECTORY_MODE, "auto"))
        initializing = false
        updateDescription()
    }

    fun getSelectedMode(): LuceneDirectoryMode =
        modeComboBox.selectedItem as? LuceneDirectoryMode ?: LuceneDirectoryMode.AUTO

    fun setSelectedMode(mode: String?) {
        modeComboBox.selectedItem = LuceneDirectoryMode.fromConfigValue(mode)
    }

    private fun initComponents() {
        layout = MigLayout(
            LC().insets("5").hideMode(3),
            AC().align("right").gap().grow().fill(),
            AC(),
        )

        add(infoLabel, CC().cell(0, 0, 2, 1).growX())

        add(JLabel("Lucene-Directory-Anbieter:"), CC().cell(0, 1))
        add(modeComboBox, CC().cell(1, 1))

        descriptionLabel.verticalAlignment = SwingConstants.TOP
        add(descriptionLabel, CC().cell(0, 2, 2, 1).growX())

        restartWarningIconLabel.isVisible = false
        restartWarningTextLabel.isVisible = false
        val restartWarningPanel = JPanel(FlowLayout(FlowLayout.LEFT, 6, 0)).apply {
            isOpaque = false
            add(restartWarningIconLabel)
            add(restartWarningTextLabel)
        }
        add(restartWarningPanel, CC().cell(0, 3, 2, 1).growX().alignX("left"))

        modeComboBox.addActionListener {
            val selectedMode = getSelectedMode()
            updateDescription()
            if (initializing) {
                return@addActionListener
            }

            config.setProperty(ApplicationConfiguration.LUCENE_DIRECTORY_MODE, selectedMode.configValue)
            restartWarningIconLabel.isVisible = true
            restartWarningTextLabel.isVisible = true
        }
    }

    private fun updateDescription() {
        descriptionLabel.text = "<html>${getSelectedMode().description}</html>"
    }
}
