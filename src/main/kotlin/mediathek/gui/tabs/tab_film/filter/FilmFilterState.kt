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

import mediathek.config.application.FilterConfiguration
import mediathek.tool.FilterDTO

data class FilmFilterState(
    val currentFilter: FilterDTO,
    val showNewOnly: Boolean,
    val showBookMarkedOnly: Boolean,
    val showHighQualityOnly: Boolean,
    val showSubtitlesOnly: Boolean,
    val showLivestreamsOnly: Boolean,
    val showUnseenOnly: Boolean,
    val dontShowAbos: Boolean,
    val dontShowSignLanguage: Boolean,
    val dontShowGeoblocked: Boolean,
    val dontShowTrailers: Boolean,
    val dontShowAudioVersions: Boolean,
    val dontShowDuplicates: Boolean,
    val checkedChannels: Set<String>,
    val thema: String,
    val filmLengthMin: Int,
    val filmLengthMax: Int,
    val zeitraum: String,
) {
    companion object {
        fun from(filterConfig: FilterConfiguration): FilmFilterState {
            return FilmFilterState(
                currentFilter = filterConfig.currentFilter,
                showNewOnly = filterConfig.isShowNewOnly,
                showBookMarkedOnly = filterConfig.isShowBookMarkedOnly,
                showHighQualityOnly = filterConfig.isShowHighQualityOnly,
                showSubtitlesOnly = filterConfig.isShowSubtitlesOnly,
                showLivestreamsOnly = filterConfig.isShowLivestreamsOnly,
                showUnseenOnly = filterConfig.isShowUnseenOnly,
                dontShowAbos = filterConfig.isDontShowAbos,
                dontShowSignLanguage = filterConfig.isDontShowSignLanguage,
                dontShowGeoblocked = filterConfig.isDontShowGeoblocked,
                dontShowTrailers = filterConfig.isDontShowTrailers,
                dontShowAudioVersions = filterConfig.isDontShowAudioVersions,
                dontShowDuplicates = filterConfig.isDontShowDuplicates,
                checkedChannels = filterConfig.checkedChannels,
                thema = filterConfig.thema,
                filmLengthMin = filterConfig.filmLengthMin.toInt(),
                filmLengthMax = filterConfig.filmLengthMax.toInt(),
                zeitraum = filterConfig.zeitraum,
            )
        }
    }
}
