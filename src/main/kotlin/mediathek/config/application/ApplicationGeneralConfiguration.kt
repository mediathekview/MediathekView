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

import mediathek.config.Konstanten
import mediathek.daten.Country
import org.apache.commons.configuration2.XMLConfiguration
import org.apache.logging.log4j.LogManager
import java.time.LocalDate

class ApplicationGeneralConfiguration(
    private val config: XMLConfiguration,
) {
    var geographicLocation: Country
        get() =
            try {
                parseCountry(config.getString(GEO_LOCATION))
            } catch (ex: Exception) {
                logger.error("Unable to parse country, resetting to GERMANY", ex)
                geographicLocation = Country.DE
                Country.DE
            }
        set(value) {
            config.setProperty(GEO_LOCATION, value.name)
        }

    fun ensureGeographicLocationDefault() {
        if (!config.containsKey(GEO_LOCATION)) {
            geographicLocation = Country.DE
        }
    }

    val userAgent: String
        get() = config.getString(APPLICATION_USER_AGENT, Konstanten.PROGRAMMNAME)

    fun setUserAgent(newValue: String?) {
        config.setProperty(APPLICATION_USER_AGENT, newValue)
    }

    fun setDefaultUserAgent() {
        setUserAgent(Konstanten.PROGRAMMNAME)
    }

    var darkMode: Boolean
        get() = config.getBoolean(APPLICATION_DARK_MODE, false)
        set(value) {
            config.setProperty(APPLICATION_DARK_MODE, value)
        }

    var useSystemDarkMode: Boolean
        get() = config.getBoolean(APPLICATION_USE_SYSTEM_DARK_MODE, false)
        set(value) {
            config.setProperty(APPLICATION_USE_SYSTEM_DARK_MODE, value)
        }

    var automaticUpdateCheck: Boolean
        get() = config.getBoolean(CONFIG_AUTOMATIC_UPDATE_CHECK, true)
        set(value) {
            config.setProperty(CONFIG_AUTOMATIC_UPDATE_CHECK, value)
        }

    var isNewFilmLengthActivationQuestionCompleted: Boolean
        get() = config.getBoolean(NEW_FILM_LENGTH_ACTIVATION_QUESTION_COMPLETED, false)
        set(value) {
            config.setProperty(NEW_FILM_LENGTH_ACTIVATION_QUESTION_COMPLETED, value)
        }

    var isNewSenderActivationQuestionCompleted: Boolean
        get() = config.getBoolean(NEW_SENDER_ACTIVATION_QUESTION_COMPLETED, false)
        set(value) {
            config.setProperty(NEW_SENDER_ACTIVATION_QUESTION_COMPLETED, value)
        }

    var programInformationDisplayedNumber: Int
        get() = config.getInt(PROGRAM_INFORMATION_DISPLAYED_NUMBER, -1)
        set(value) {
            config.setProperty(PROGRAM_INFORMATION_DISPLAYED_NUMBER, value)
        }

    var seenHistoryMaintenanceLastRun: LocalDate?
        get() = config.getString(SEEN_HISTORY_MAINTENANCE_LAST_RUN, null)?.let(LocalDate::parse)
        set(value) {
            if (value != null) {
                config.setProperty(SEEN_HISTORY_MAINTENANCE_LAST_RUN, value.toString())
            }
        }

    var showNotifications: Boolean
        get() = config.getBoolean(APPLICATION_SHOW_NOTIFICATIONS, true)
        set(value) {
            config.setProperty(APPLICATION_SHOW_NOTIFICATIONS, value)
        }

    var showOrfConfigHelp: Boolean
        get() = config.getBoolean(APPLICATION_SHOW_ORF_CONFIG_HELP, true)
        set(value) {
            config.setProperty(APPLICATION_SHOW_ORF_CONFIG_HELP, value)
        }

    private fun parseCountry(rawValue: String?): Country {
        var value = rawValue?.trim() ?: Country.DE.name
        if (value.length >= 2 && value.startsWith("\"") && value.endsWith("\"")) {
            value = value.substring(1, value.length - 1)
        }
        return Country.valueOf(value)
    }

    private companion object {
        private val logger = LogManager.getLogger()

        @field:ApplicationConfigKey
        private const val APPLICATION_DARK_MODE = "application.dark_mode"
        @field:ApplicationConfigKey
        private const val APPLICATION_USE_SYSTEM_DARK_MODE = "application.use_system_dark_mode"
        @field:ApplicationConfigKey
        private const val APPLICATION_USER_AGENT = "application.user_agent"
        @field:ApplicationConfigKey
        private const val NEW_SENDER_ACTIVATION_QUESTION_COMPLETED = "newSendersActivated.fourteen.three"
        @field:ApplicationConfigKey
        private const val NEW_FILM_LENGTH_ACTIVATION_QUESTION_COMPLETED = "newFilmlengthActivated.fourteen.three"
        @field:ApplicationConfigKey
        private const val PROGRAM_INFORMATION_DISPLAYED_NUMBER = "program_information.displayed_number"
        @field:ApplicationConfigKey
        private const val SEEN_HISTORY_MAINTENANCE_LAST_RUN = "database.seen_history.maintenance.lastRun"
        @field:ApplicationConfigKey
        private const val APPLICATION_SHOW_NOTIFICATIONS = "application.notifications.show"
        @field:ApplicationConfigKey
        private const val APPLICATION_SHOW_ORF_CONFIG_HELP = "application.orf.show_config_help"
        @field:ApplicationConfigKey
        private const val CONFIG_AUTOMATIC_UPDATE_CHECK = "application.automatic_update_check"
        @field:ApplicationConfigKey
        private const val GEO_LOCATION = "geo.location"
    }
}
