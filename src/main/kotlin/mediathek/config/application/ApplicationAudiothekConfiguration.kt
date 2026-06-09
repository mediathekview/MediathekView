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

class ApplicationAudiothekConfiguration(
    private val config: XMLConfiguration,
) {
    var audiothekTabVisible: Boolean
        get() = config.getBoolean(APPLICATION_UI_SHOW_AUDIOTHEK, true)
        set(newValue) {
            config.setProperty(APPLICATION_UI_SHOW_AUDIOTHEK, newValue)
        }

    var audiothekOnlineSearch: Boolean
        get() = config.getBoolean(APPLICATION_UI_AUDIOTHEK_ONLINE_SEARCH, true)
        set(newValue) {
            config.setProperty(APPLICATION_UI_AUDIOTHEK_ONLINE_SEARCH, newValue)
        }

    var audiothekSearchHistory: String
        get() = config.getString(APPLICATION_UI_AUDIOTHEK_SEARCH_HISTORY, "[]")
        set(newValue) {
            config.setProperty(APPLICATION_UI_AUDIOTHEK_SEARCH_HISTORY, newValue)
        }

    var audiothekTableState: String
        get() = config.getString(APPLICATION_UI_AUDIOTHEK_TABLE_STATE, "")
        set(newValue) {
            config.setProperty(APPLICATION_UI_AUDIOTHEK_TABLE_STATE, newValue)
        }

    private companion object {
        private const val APPLICATION_UI_SHOW_AUDIOTHEK = "application.ui.audiothek.show"
        private const val APPLICATION_UI_AUDIOTHEK_TABLE_STATE = "application.ui.audiothek.table.state"
        private const val APPLICATION_UI_AUDIOTHEK_ONLINE_SEARCH = "application.ui.audiothek.online_search"
        private const val APPLICATION_UI_AUDIOTHEK_SEARCH_HISTORY = "application.ui.audiothek.search.history"
    }
}
