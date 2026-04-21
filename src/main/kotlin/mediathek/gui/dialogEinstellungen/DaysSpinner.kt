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

import mediathek.tool.ApplicationConfiguration
import javax.swing.JSpinner
import javax.swing.SpinnerListModel

private const val ALL_DAYS_TEXT = " Alle "
private val DAY_OPTIONS = listOf(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 12, 14, 16, 18, 20, 25, 30, 60, 90, 180, 365)
private val DAY_OPTION_TEXTS = DAY_OPTIONS.map(::toDisplayText).toTypedArray()

class DaysSpinner : JSpinner() {

    init {
        model = SpinnerListModel(DAY_OPTION_TEXTS)
        configureEditor()
        installConfigSync()
        restoreSelection()
    }

    private fun configureEditor() {
        (editor as DefaultEditor).textField.isEditable = false
    }

    private fun installConfigSync() {
        addChangeListener {
            configuration.setProperty(ApplicationConfiguration.FilmList.LOAD_NUM_DAYS, selectedEntry.toConfigValue())
        }
    }

    private fun restoreSelection() {
        value = DayEntry.fromConfigValue(
            configuration.getInt(ApplicationConfiguration.FilmList.LOAD_NUM_DAYS, 0),
        ).displayText
    }

    private val selectedEntry: DayEntry
        get() = DayEntry.fromDisplayText(value.toString())

    private val configuration
        get() = ApplicationConfiguration.getConfiguration()
}

private data class DayEntry(val displayText: String, val configValue: Int) {
    fun toConfigValue(): Int = configValue

    companion object {
        private val entries = DAY_OPTIONS.map { DayEntry(toDisplayText(it), it) }

        fun fromConfigValue(configValue: Int): DayEntry = entries.firstOrNull { it.configValue == configValue } ?: entries.first()

        fun fromDisplayText(displayText: String): DayEntry = entries.firstOrNull { it.displayText == displayText } ?: entries.first()
    }
}

private fun toDisplayText(dayCount: Int): String = if (dayCount == 0) ALL_DAYS_TEXT else dayCount.toString()
