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

class ApplicationBlacklistConfiguration(
    private val config: XMLConfiguration,
) {
    var blacklistEnabled: Boolean
        get() = config.getBoolean(BLACKLIST_IS_ON, false)
        set(newValue) {
            config.setProperty(BLACKLIST_IS_ON, newValue)
        }

    val blacklistDuplicateFilteringEnabled: Boolean
        get() = config.getBoolean(BLACKLIST_FILTER_DUPLICATES, false)

    var blacklistDoNotShowGeoblockedFilms: Boolean
        get() = config.getBoolean(BLACKLIST_DO_NOT_SHOW_GEOBLOCKED_FILMS, false)
        set(newValue) {
            config.setProperty(BLACKLIST_DO_NOT_SHOW_GEOBLOCKED_FILMS, newValue)
        }

    var blacklistDoNotShowFutureFilms: Boolean
        get() = config.getBoolean(BLACKLIST_DO_NOT_SHOW_FUTURE_FILMS, false)
        set(newValue) {
            config.setProperty(BLACKLIST_DO_NOT_SHOW_FUTURE_FILMS, newValue)
        }

    var blacklistApplyToAbo: Boolean
        get() = config.getBoolean(BLACKLIST_APPLY_TO_ABO, false)
        set(newValue) {
            config.setProperty(BLACKLIST_APPLY_TO_ABO, newValue)
        }

    var blacklistWhitelistMode: Boolean
        get() = config.getBoolean(BLACKLIST_IS_WHITELIST, false)
        set(newValue) {
            config.setProperty(BLACKLIST_IS_WHITELIST, newValue)
        }

    var blacklistMinimumFilmLengthMinutes: Int
        get() = config.getInt(BLACKLIST_MINIMUM_FILM_LENGTH_MINUTES, 0)
        set(newValue) {
            config.setProperty(BLACKLIST_MINIMUM_FILM_LENGTH_MINUTES, newValue)
        }

    private companion object {
        private const val BLACKLIST_FILTER_DUPLICATES = "blacklist.filter_duplicates"
        private const val BLACKLIST_IS_ON = "blacklist.is_on"
        private const val BLACKLIST_DO_NOT_SHOW_GEOBLOCKED_FILMS = "blacklist.show_geoblocked"
        private const val BLACKLIST_DO_NOT_SHOW_FUTURE_FILMS = "blacklist.show_future_films.disable"
        private const val BLACKLIST_APPLY_TO_ABO = "blacklist.apply_to_abo"
        private const val BLACKLIST_IS_WHITELIST = "blacklist.is_whitelist"
        private const val BLACKLIST_MINIMUM_FILM_LENGTH_MINUTES = "blacklist.minimum_film_length.minutes"
    }
}
