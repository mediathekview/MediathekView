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

import mediathek.tool.FilmListUpdateType
import org.apache.commons.configuration2.XMLConfiguration

class ApplicationFilmListConfiguration(
    private val config: XMLConfiguration,
) {
    var evaluateFilmDuplicates: Boolean
        get() = config.getBoolean(FILM_EVALUATE_DUPLICATES, true)
        set(newValue) {
            config.setProperty(FILM_EVALUATE_DUPLICATES, newValue)
        }

    var filmDescriptionVisible: Boolean
        get() = config.getBoolean(FILM_SHOW_DESCRIPTION, true)
        set(newValue) {
            config.setProperty(FILM_SHOW_DESCRIPTION, newValue)
        }

    var luceneDirectoryMode: String
        get() = config.getString(LUCENE_DIRECTORY_MODE, DEFAULT_LUCENE_DIRECTORY_MODE)
        set(newValue) {
            config.setProperty(LUCENE_DIRECTORY_MODE, newValue)
        }

    fun setDefaultLuceneDirectoryMode() {
        luceneDirectoryMode = DEFAULT_LUCENE_DIRECTORY_MODE
    }

    fun ensureLuceneDirectoryModeDefault() {
        if (!config.containsKey(LUCENE_DIRECTORY_MODE)) {
            setDefaultLuceneDirectoryMode()
        }
    }

    var filmListLoadNumDays: Int
        get() = config.getInt(FILMLIST_LOAD_NUM_DAYS, 0)
        set(newValue) {
            config.setProperty(FILMLIST_LOAD_NUM_DAYS, newValue)
        }

    var approvedFilmlistLoadSenders: List<String>
        get() = config.getList(String::class.java, FILMLIST_APPROVED_FOR_LOAD).orEmpty()
        set(newValue) {
            config.setProperty(FILMLIST_APPROVED_FOR_LOAD, newValue)
        }

    fun setApprovedFilmlistLoadSenders(senders: Collection<String>) {
        config.setProperty(FILMLIST_APPROVED_FOR_LOAD, senders)
    }

    var filmListUpdateType: Int
        get() = config.getInt(FILMLIST_UPDATE_TYPE, FilmListUpdateType.AUTOMATIC.configValue)
        set(newValue) {
            config.setProperty(FILMLIST_UPDATE_TYPE, newValue)
        }

    var filmListManualImportUrl: String
        get() = config.getString(FILMLIST_MANUAL_IMPORT_URL, "")
        set(newValue) {
            config.setProperty(FILMLIST_MANUAL_IMPORT_URL, newValue)
        }

    var filmListLoadTrailer: Boolean
        get() = config.getBoolean(FILMLIST_LOAD_TRAILER, true)
        set(newValue) {
            config.setProperty(FILMLIST_LOAD_TRAILER, newValue)
        }

    var filmListLoadAudioDescription: Boolean
        get() = config.getBoolean(FILMLIST_LOAD_AUDIO_DESCRIPTION, true)
        set(newValue) {
            config.setProperty(FILMLIST_LOAD_AUDIO_DESCRIPTION, newValue)
        }

    var filmListLoadSignLanguage: Boolean
        get() = config.getBoolean(FILMLIST_LOAD_SIGN_LANGUAGE, true)
        set(newValue) {
            config.setProperty(FILMLIST_LOAD_SIGN_LANGUAGE, newValue)
        }

    var filmListLoadLivestreams: Boolean
        get() = config.getBoolean(FILMLIST_LOAD_LIVESTREAMS, true)
        set(newValue) {
            config.setProperty(FILMLIST_LOAD_LIVESTREAMS, newValue)
        }

    var extendOldFilmList: Boolean
        get() = config.getBoolean(FILMLIST_EXTEND_OLD_FILMLIST, false)
        set(newValue) {
            config.setProperty(FILMLIST_EXTEND_OLD_FILMLIST, newValue)
        }

    private companion object {
        private const val DEFAULT_LUCENE_DIRECTORY_MODE = "auto"
        private const val LUCENE_DIRECTORY_MODE = "lucene.directory.mode"
        private const val FILM_SHOW_DESCRIPTION = "film.show_description"
        private const val FILM_EVALUATE_DUPLICATES = "film.evaluate_duplicates"
        private const val FILMLIST_APPROVED_FOR_LOAD = "filmlist.approved_for_load"
        private const val FILMLIST_UPDATE_TYPE = "filmlist.update.type"
        private const val FILMLIST_MANUAL_IMPORT_URL = "filmlist.manual_import.url"
        private const val FILMLIST_LOAD_TRAILER = "filmlist.load.trailer"
        private const val FILMLIST_LOAD_AUDIO_DESCRIPTION = "filmlist.load.audio_description"
        private const val FILMLIST_LOAD_SIGN_LANGUAGE = "filmlist.load.sign_language"
        private const val FILMLIST_LOAD_NUM_DAYS = "filmlist.load.days"
        private const val FILMLIST_LOAD_LIVESTREAMS = "filmlist.load.livestreams"
        private const val FILMLIST_EXTEND_OLD_FILMLIST = "filmlist.extend_old_filmlist"
    }
}
