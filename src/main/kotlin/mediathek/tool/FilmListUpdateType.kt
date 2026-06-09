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

package mediathek.tool

import mediathek.config.application.ApplicationConfiguration
enum class FilmListUpdateType(
    val configValue: Int,
) {
    MANUAL(0),
    AUTOMATIC(2),
    ;

    companion object {
        fun fromConfigValue(configValue: Int): FilmListUpdateType =
            entries.firstOrNull { it.configValue == configValue } ?: AUTOMATIC

        fun fromConfig(): FilmListUpdateType {
            val updateType = fromConfigValue(ApplicationConfiguration.getInstance().filmListUpdateType)
            if (updateType == AUTOMATIC) {
                AUTOMATIC.writeToConfig()
            }
            return updateType
        }
    }

    fun isConfigured(): Boolean = fromConfig() == this

    fun writeToConfig() {
        ApplicationConfiguration.getInstance().filmListUpdateType = configValue
    }
}
