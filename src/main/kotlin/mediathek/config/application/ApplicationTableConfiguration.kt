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

package mediathek.config.application

import org.apache.commons.configuration2.XMLConfiguration
import org.apache.commons.configuration2.sync.LockMode

class ApplicationTableConfiguration(
    private val config: XMLConfiguration,
) {
    fun getGlazedTableSortKeys(configPrefix: String): String? =
        config.getString(tableSortKeysKey(configPrefix))

    fun setGlazedTableSortKeys(configPrefix: String, json: String) {
        config.setProperty(tableSortKeysKey(configPrefix), json)
    }

    fun getTableColumnSettings(configPrefix: String): String? =
        config.withLock(LockMode.READ) {
            getString(tableColumnSettingsKey(configPrefix))
        }

    fun setTableColumnSettings(configPrefix: String, json: String) {
        config.withLock(LockMode.WRITE) {
            setProperty(tableColumnSettingsKey(configPrefix), json)
        }
    }

    var filmTableLineBreak: Boolean
        get() = config.getBoolean(APPLICATION_UI_FILM_TABLE_LINEBREAK, false)
        set(newValue) {
            config.setProperty(APPLICATION_UI_FILM_TABLE_LINEBREAK, newValue)
        }

    var downloadTableLineBreak: Boolean
        get() = config.getBoolean(APPLICATION_UI_DOWNLOAD_TABLE_LINEBREAK, false)
        set(newValue) {
            config.setProperty(APPLICATION_UI_DOWNLOAD_TABLE_LINEBREAK, newValue)
        }

    var filmTableShowSenderIcons: Boolean
        get() = config.getBoolean(APPLICATION_UI_FILM_TABLE_SHOW_SENDER_ICONS, true)
        set(newValue) {
            config.setProperty(APPLICATION_UI_FILM_TABLE_SHOW_SENDER_ICONS, newValue)
        }

    var filmTableUseSmallSenderIcons: Boolean
        get() = config.getBoolean(APPLICATION_UI_FILM_TABLE_USE_SMALL_SENDER_ICONS, true)
        set(newValue) {
            config.setProperty(APPLICATION_UI_FILM_TABLE_USE_SMALL_SENDER_ICONS, newValue)
        }

    var downloadTableShowSenderIcons: Boolean
        get() = config.getBoolean(APPLICATION_UI_DOWNLOAD_TABLE_SHOW_SENDER_ICONS, true)
        set(newValue) {
            config.setProperty(APPLICATION_UI_DOWNLOAD_TABLE_SHOW_SENDER_ICONS, newValue)
        }

    var downloadTableUseSmallSenderIcons: Boolean
        get() = config.getBoolean(APPLICATION_UI_DOWNLOAD_TABLE_USE_SMALL_SENDER_ICONS, true)
        set(newValue) {
            config.setProperty(APPLICATION_UI_DOWNLOAD_TABLE_USE_SMALL_SENDER_ICONS, newValue)
        }

    var aboTableShowSenderIcons: Boolean
        get() = config.getBoolean(APPLICATION_UI_ABO_TABLE_SHOW_SENDER_ICONS, true)
        set(newValue) {
            config.setProperty(APPLICATION_UI_ABO_TABLE_SHOW_SENDER_ICONS, newValue)
        }

    var aboTableUseSmallSenderIcons: Boolean
        get() = config.getBoolean(APPLICATION_UI_ABO_TABLE_USE_SMALL_SENDER_ICONS, true)
        set(newValue) {
            config.setProperty(APPLICATION_UI_ABO_TABLE_USE_SMALL_SENDER_ICONS, newValue)
        }

    var filmTableColumnConfiguration: String
        get() = config.getString(APPLICATION_UI_FILM_TABLE_COLUMN_CONFIGURATION, "")
        set(newValue) {
            config.setProperty(APPLICATION_UI_FILM_TABLE_COLUMN_CONFIGURATION, newValue)
        }

    var downloadTableColumnConfiguration: String
        get() = config.getString(APPLICATION_UI_DOWNLOAD_TABLE_COLUMN_CONFIGURATION, "")
        set(newValue) {
            config.setProperty(APPLICATION_UI_DOWNLOAD_TABLE_COLUMN_CONFIGURATION, newValue)
        }

    var aboTableColumnConfiguration: String
        get() = config.getString(APPLICATION_UI_ABO_TABLE_COLUMN_CONFIGURATION, "")
        set(newValue) {
            config.setProperty(APPLICATION_UI_ABO_TABLE_COLUMN_CONFIGURATION, newValue)
        }

    private fun tableSortKeysKey(configPrefix: String): String =
        "$configPrefix$TABLE_SORT_KEYS_SUFFIX"

    private fun tableColumnSettingsKey(configPrefix: String): String =
        "$configPrefix$TABLE_COLUMN_SETTINGS_SUFFIX"

    private companion object {
        @field:ApplicationConfigKey
        private const val APPLICATION_UI_FILM_TABLE_LINEBREAK = "application.ui.film_table.linebreak"
        @field:ApplicationConfigKey
        private const val APPLICATION_UI_DOWNLOAD_TABLE_LINEBREAK = "application.ui.download_table.linebreak"
        @field:ApplicationConfigKey
        private const val APPLICATION_UI_FILM_TABLE_SHOW_SENDER_ICONS = "application.ui.film_table.sender_icons.show"
        @field:ApplicationConfigKey
        private const val APPLICATION_UI_FILM_TABLE_USE_SMALL_SENDER_ICONS = "application.ui.film_table.sender_icons.small"
        @field:ApplicationConfigKey
        private const val APPLICATION_UI_DOWNLOAD_TABLE_SHOW_SENDER_ICONS =
            "application.ui.download_table.sender_icons.show"
        @field:ApplicationConfigKey
        private const val APPLICATION_UI_DOWNLOAD_TABLE_USE_SMALL_SENDER_ICONS =
            "application.ui.download_table.sender_icons.small"
        @field:ApplicationConfigKey
        private const val APPLICATION_UI_ABO_TABLE_SHOW_SENDER_ICONS = "application.ui.abo_table.sender_icons.show"
        @field:ApplicationConfigKey
        private const val APPLICATION_UI_ABO_TABLE_USE_SMALL_SENDER_ICONS = "application.ui.abo_table.sender_icons.small"
        @field:ApplicationConfigKey
        private const val APPLICATION_UI_FILM_TABLE_COLUMN_CONFIGURATION =
            "application.ui.film_table.column_configuration"
        @field:ApplicationConfigKey
        private const val APPLICATION_UI_DOWNLOAD_TABLE_COLUMN_CONFIGURATION =
            "application.ui.download_table.column_configuration"
        @field:ApplicationConfigKey
        private const val APPLICATION_UI_ABO_TABLE_COLUMN_CONFIGURATION = "application.ui.abo_table.column_configuration"
        @field:ApplicationConfigKeyPattern(
            pattern = """^(ui\.bookmark-dialog|abo|abo-v3)\.(sortKeys|colummn-settings)$""",
            description = "Persisted table sorting and column settings keyed by known table prefixes.",
        )
        private const val TABLE_SORT_KEYS_SUFFIX = ".sortKeys"
        private const val TABLE_COLUMN_SETTINGS_SUFFIX = ".colummn-settings"
    }
}
