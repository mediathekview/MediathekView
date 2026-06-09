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
import org.apache.commons.configuration2.XMLConfiguration
import org.apache.commons.configuration2.sync.LockMode

class ApplicationDownloadConfiguration(
    private val config: XMLConfiguration,
) {
    var maxSimultaneousDownloads: Int
        get() = config.getInt(DOWNLOAD_MAX_SIMULTANEOUS_NUM, 1)
        set(newValue) {
            config.setProperty(DOWNLOAD_MAX_SIMULTANEOUS_NUM, newValue)
        }

    var fetchMissingDownloadFileSize: Boolean
        get() = config.getBoolean(DOWNLOAD_FETCH_FILE_SIZE, true)
        set(newValue) {
            config.setProperty(DOWNLOAD_FETCH_FILE_SIZE, newValue)
        }

    var downloadContinuationTime: Int
        get() = config.getInt(DOWNLOAD_CONTINUATION_TIME, Konstanten.DOWNLOAD_CONTINUATION_DEFAULT_TIME.toInt())
        set(newValue) {
            config.setProperty(DOWNLOAD_CONTINUATION_TIME, newValue)
        }

    var useCdnAwareDirectDownload: Boolean
        get() = config.getBoolean(
            DOWNLOAD_USE_CDN_AWARE_DIRECT_DOWNLOAD,
            DEFAULT_DOWNLOAD_USE_CDN_AWARE_DIRECT_DOWNLOAD,
        )
        set(newValue) {
            config.setProperty(DOWNLOAD_USE_CDN_AWARE_DIRECT_DOWNLOAD, newValue)
        }

    fun setDefaultCdnAwareDirectDownload() {
        config.setProperty(DOWNLOAD_USE_CDN_AWARE_DIRECT_DOWNLOAD, DEFAULT_DOWNLOAD_USE_CDN_AWARE_DIRECT_DOWNLOAD)
    }

    fun ensureCdnAwareDirectDownloadDefault() {
        if (!config.containsKey(DOWNLOAD_USE_CDN_AWARE_DIRECT_DOWNLOAD)) {
            setDefaultCdnAwareDirectDownload()
        }
    }

    var showLastUsedDownloadPath: Boolean
        get() = config.getBoolean(DOWNLOAD_SHOW_LAST_USED_PATH, true)
        set(newValue) {
            config.setProperty(DOWNLOAD_SHOW_LAST_USED_PATH, newValue)
        }

    var savedDownloadTargetPaths: String
        get() = config.getString(DOWNLOAD_SAVED_TARGET_PATHS, "")
        set(newValue) {
            config.setProperty(DOWNLOAD_SAVED_TARGET_PATHS, newValue)
        }

    var playSoundAfterDownload: Boolean
        get() = config.getBoolean(DOWNLOAD_SOUND_BEEP, false)
        set(newValue) {
            config.setProperty(DOWNLOAD_SOUND_BEEP, newValue)
        }

    var showDownloadErrorMessage: Boolean
        get() = config.getBoolean(DOWNLOAD_SHOW_ERROR_MESSAGE, true)
        set(newValue) {
            config.setProperty(DOWNLOAD_SHOW_ERROR_MESSAGE, newValue)
        }

    var startDownloadsImmediately: Boolean
        get() = config.getBoolean(DOWNLOAD_START_IMMEDIATELY, false)
        set(newValue) {
            config.setProperty(DOWNLOAD_START_IMMEDIATELY, newValue)
        }

    var showDownloadDescription: Boolean
        get() = config.getBoolean(DOWNLOAD_SHOW_DESCRIPTION, true)
        set(newValue) {
            config.setProperty(DOWNLOAD_SHOW_DESCRIPTION, newValue)
        }

    fun getDownloadDisplayFilter(defaultValue: String): String =
        config.getString(DOWNLOAD_DISPLAY_FILTER, defaultValue)

    fun setDownloadDisplayFilter(newValue: String) {
        config.setProperty(DOWNLOAD_DISPLAY_FILTER, newValue)
    }

    fun getDownloadViewFilter(defaultValue: String): String =
        config.getString(DOWNLOAD_VIEW_FILTER, defaultValue)

    fun setDownloadViewFilter(newValue: String) {
        config.setProperty(DOWNLOAD_VIEW_FILTER, newValue)
    }

    var downloadRateLimitActive: Boolean
        get() = config.getBoolean(DOWNLOAD_RATE_LIMIT_ACTIVE, false)
        set(newValue) {
            config.setProperty(DOWNLOAD_RATE_LIMIT_ACTIVE, newValue)
        }

    var downloadRateLimit: Long
        get() = config.getLong(DOWNLOAD_RATE_LIMIT, 0)
        set(newValue) {
            config.setProperty(DOWNLOAD_RATE_LIMIT, newValue)
        }

    fun getDownloadToolbarState(toolbarId: String, defaultOrientation: Int): ApplicationConfiguration.DownloadToolbarState =
        config.withLock(LockMode.READ) {
            ApplicationConfiguration.DownloadToolbarState(
                getBoolean(downloadToolbarStateKey(toolbarId, "floating"), false),
                getInt(downloadToolbarStateKey(toolbarId, "x"), 0),
                getInt(downloadToolbarStateKey(toolbarId, "y"), 0),
                getInt(downloadToolbarStateKey(toolbarId, "orientation"), defaultOrientation),
            )
        }

    fun setDownloadToolbarState(toolbarId: String, state: ApplicationConfiguration.DownloadToolbarState) {
        config.withLock(LockMode.WRITE) {
            setProperty(downloadToolbarStateKey(toolbarId, "floating"), state.floating)
            setProperty(downloadToolbarStateKey(toolbarId, "x"), state.x)
            setProperty(downloadToolbarStateKey(toolbarId, "y"), state.y)
            setProperty(downloadToolbarStateKey(toolbarId, "orientation"), state.orientation)
        }
    }

    private fun downloadToolbarStateKey(toolbarId: String, property: String): String =
        "$DOWNLOAD_TOOLBAR_STATE_PREFIX$toolbarId.$property"

    private companion object {
        private const val DEFAULT_DOWNLOAD_USE_CDN_AWARE_DIRECT_DOWNLOAD = false
        private const val DOWNLOAD_SHOW_LAST_USED_PATH = "download.path.last_used.show"
        private const val DOWNLOAD_SAVED_TARGET_PATHS = "download.path.saved_targets"
        private const val DOWNLOAD_SOUND_BEEP = "download.sound.beep"
        private const val DOWNLOAD_SHOW_ERROR_MESSAGE = "download.error_message.show"
        private const val DOWNLOAD_START_IMMEDIATELY = "download.start_immediately"
        private const val DOWNLOAD_SHOW_DESCRIPTION = "download.show_description"
        private const val DOWNLOAD_DISPLAY_FILTER = "download.display_filter"
        private const val DOWNLOAD_VIEW_FILTER = "download.view_filter"
        private const val DOWNLOAD_TOOLBAR_STATE_PREFIX = "download.toolbar.state."
        private const val DOWNLOAD_MAX_SIMULTANEOUS_NUM = "download.max_simultaneous.number"
        private const val DOWNLOAD_FETCH_FILE_SIZE = "download.fetch_file_size"
        private const val DOWNLOAD_CONTINUATION_TIME = "download.continuation.time"
        private const val DOWNLOAD_USE_CDN_AWARE_DIRECT_DOWNLOAD = "download.cdn_aware_direct_download.use"
        private const val DOWNLOAD_RATE_LIMIT = "download.rate.limit"
        private const val DOWNLOAD_RATE_LIMIT_ACTIVE = "download.rate.active"
    }
}
