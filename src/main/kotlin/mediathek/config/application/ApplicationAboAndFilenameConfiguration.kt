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
import org.apache.commons.lang3.SystemUtils

class ApplicationAboAndFilenameConfiguration(
    private val config: XMLConfiguration,
) {
    var defaultAboMinimumDurationMinutes: Int
        get() = config.getInt(ABO_DEFAULT_MINIMUM_DURATION_MINUTES, 0)
        set(newValue) {
            config.setProperty(ABO_DEFAULT_MINIMUM_DURATION_MINUTES, newValue)
        }

    var searchAbosImmediately: Boolean
        get() = config.getBoolean(ABO_SEARCH_IMMEDIATELY, true)
        set(newValue) {
            config.setProperty(ABO_SEARCH_IMMEDIATELY, newValue)
        }

    var useFilenameReplaceTable: Boolean
        get() = config.getBoolean(
            FILENAME_USE_REPLACE_TABLE,
            SystemUtils.IS_OS_LINUX || SystemUtils.IS_OS_MAC_OSX,
        )
        set(newValue) {
            config.setProperty(FILENAME_USE_REPLACE_TABLE, newValue)
        }

    var onlyAsciiFilenames: Boolean
        get() = config.getBoolean(FILENAME_ONLY_ASCII, false)
        set(newValue) {
            config.setProperty(FILENAME_ONLY_ASCII, newValue)
        }

    private companion object {
        @field:ApplicationConfigKey
        private const val ABO_DEFAULT_MINIMUM_DURATION_MINUTES = "abo.default_minimum_duration.minutes"
        @field:ApplicationConfigKey
        private const val ABO_SEARCH_IMMEDIATELY = "abo.search_immediately"
        @field:ApplicationConfigKey
        private const val FILENAME_USE_REPLACE_TABLE = "filename.use_replace_table"
        @field:ApplicationConfigKey
        private const val FILENAME_ONLY_ASCII = "filename.only_ascii"
    }
}
