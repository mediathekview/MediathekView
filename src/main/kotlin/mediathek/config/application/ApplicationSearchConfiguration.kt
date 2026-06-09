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

class ApplicationSearchConfiguration(
    private val config: XMLConfiguration,
) {
    var useModernSearch: Boolean
        get() = config.getBoolean(APPLICATION_USE_MODERN_SEARCH, false)
        set(newValue) {
            config.setProperty(APPLICATION_USE_MODERN_SEARCH, newValue)
        }

    var searchUseFilmDescriptions: Boolean
        get() = config.getBoolean(SEARCH_USE_FILM_DESCRIPTIONS, false)
        set(newValue) {
            config.setProperty(SEARCH_USE_FILM_DESCRIPTIONS, newValue)
        }

    fun getSearchHistoryItems(luceneSearch: Boolean): Any? =
        config.getProperty(searchHistoryItemsKey(luceneSearch))

    fun setSearchHistoryItems(luceneSearch: Boolean, json: String) {
        config.setProperty(searchHistoryItemsKey(luceneSearch), json)
    }

    private fun searchHistoryItemsKey(luceneSearch: Boolean): String =
        if (luceneSearch) "$SEARCH_HISTORY_ITEMS$SEARCH_HISTORY_LUCENE_SUFFIX" else SEARCH_HISTORY_ITEMS

    private companion object {
        private const val APPLICATION_USE_MODERN_SEARCH = "application.use.modern_search"
        private const val SEARCH_USE_FILM_DESCRIPTIONS = "searchfield.film.search_through_description"
        private const val SEARCH_HISTORY_ITEMS = "search.history.items"
        private const val SEARCH_HISTORY_LUCENE_SUFFIX = "_lucene"
    }
}
