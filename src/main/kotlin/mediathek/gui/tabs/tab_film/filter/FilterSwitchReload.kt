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

package mediathek.gui.tabs.tab_film.filter

internal object FilterSwitchReload {
    fun determine(
        previousState: FilmFilterState,
        currentState: FilmFilterState,
        requestReload: Boolean
    ): FilterSwitchReloadType {
        if (!requestReload || previousState.currentFilter == currentState.currentFilter) {
            return FilterSwitchReloadType.NONE
        }

        return if (previousState.zeitraum != currentState.zeitraum) {
            FilterSwitchReloadType.ZEITRAUM
        } else {
            FilterSwitchReloadType.TABLE
        }
    }

    fun apply(
        previousState: FilmFilterState,
        currentState: FilmFilterState,
        requestReload: Boolean,
        reloadRequester: FilmFilterController.ReloadRequester
    ): FilterSwitchReloadType {
        val reloadType = determine(previousState, currentState, requestReload)
        when (reloadType) {
            FilterSwitchReloadType.ZEITRAUM -> reloadRequester.requestZeitraumReload()
            FilterSwitchReloadType.TABLE -> reloadRequester.requestTableReload()
            FilterSwitchReloadType.NONE -> Unit
        }
        return reloadType
    }
}

internal enum class FilterSwitchReloadType {
    NONE,
    TABLE,
    ZEITRAUM
}
