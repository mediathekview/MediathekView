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

import mediathek.config.MVColor
import mediathek.mainwindow.SettingsDialogHost
import mediathek.tool.MVC
import mediathek.tool.cellrenderer.CellRendererColor
import mediathek.tool.models.TModelColor
import java.awt.Frame
import java.awt.event.MouseAdapter
import java.awt.event.MouseEvent
import javax.swing.JColorChooser
import javax.swing.JTable
import javax.swing.SwingUtilities

class PanelEinstellungenColor(
    private val host: SettingsDialogHost,
) : PanelEinstellungenColorBase() {
    private val lightColorTableModel = TModelColor(isDarkMode = false)
    private val darkColorTableModel = TModelColor(isDarkMode = true)
    private val colorThemes = listOf(
        ColorTheme(
            tabIndex = 0,
            resetButtonText = "Helle Farben zurücksetzen",
            model = lightColorTableModel,
        ),
        ColorTheme(
            tabIndex = 1,
            resetButtonText = "Dunkle Farben zurücksetzen",
            model = darkColorTableModel,
        ),
    )

    init {
        configureTable(jTableLight, lightColorTableModel)
        configureTable(jTableDark, darkColorTableModel)
        jTabbedPane1.addChangeListener { updateResetButtonText() }
        jButtonReset.addActionListener { resetSelectedThemeColors() }
        updateResetButtonText()
    }

    /**
     * Force update of the user interface.
     */
    private fun updateGui() {
        try {
            host.setupAlternatingRowColors()
            SwingUtilities.updateComponentTreeUI(host.ownerFrame())
            for (frame in Frame.getFrames()) {
                SwingUtilities.updateComponentTreeUI(frame)
                for (window in frame.ownedWindows) {
                    SwingUtilities.updateComponentTreeUI(window)
                }
            }
        } catch (_: Exception) {
        }
    }

    private fun configureTable(table: JTable, model: TModelColor) {
        table.addMouseListener(ColorTableMouseListener(table, model))
        table.model = model
        table.columnModel
            .getColumn(MVColor.MVC_COLOR)
            .cellRenderer = CellRendererColor { model.isDarkMode }
    }

    private fun chooseColor(mvc: MVC, darkMode: Boolean) {
        val currentColor = mvc.getColor(darkMode)
        val selectedColor = JColorChooser.showDialog(this, "Farbe auswählen", currentColor)
        if (selectedColor != null && selectedColor != currentColor) {
            mvc.setColor(darkMode, selectedColor)
            saveAndRefreshColors()
        }
    }

    private fun saveAndRefreshColors() {
        lightColorTableModel.fireTableDataChanged()
        darkColorTableModel.fireTableDataChanged()
        updateGui()
        MVColor.save()
    }

    private fun resetSelectedThemeColors() {
        val darkMode = selectedTheme.model.isDarkMode
        for (mvc in MVColor.getColors()) {
            mvc.reset(darkMode)
        }
        saveAndRefreshColors()
    }

    private fun updateResetButtonText() {
        jButtonReset.text = selectedTheme.resetButtonText
    }

    private val selectedTheme: ColorTheme
        get() = colorThemes.firstOrNull { it.tabIndex == jTabbedPane1.selectedIndex } ?: colorThemes.first()

    private inner class ColorTableMouseListener(
        private val table: JTable,
        private val model: TModelColor,
    ) : MouseAdapter() {
        override fun mouseClicked(event: MouseEvent) {
            if (event.button != MouseEvent.BUTTON1 || event.clickCount != 1) {
                return
            }

            val row = table.rowAtPoint(event.point)
            val column = table.columnAtPoint(event.point)
            if (row < 0 || column < 0) {
                return
            }

            val mvc = model.getEntry(table.convertRowIndexToModel(row))
            if (table.convertColumnIndexToModel(column) == MVColor.MVC_COLOR) {
                chooseColor(mvc, model.isDarkMode)
            }
        }
    }

    private data class ColorTheme(
        val tabIndex: Int,
        val resetButtonText: String,
        val model: TModelColor,
    )
}
