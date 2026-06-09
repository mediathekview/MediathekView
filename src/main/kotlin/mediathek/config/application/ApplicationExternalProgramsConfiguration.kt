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
import mediathek.tool.GuiFunktionenProgramme
import org.apache.commons.configuration2.XMLConfiguration

class ApplicationExternalProgramsConfiguration(
    private val config: XMLConfiguration,
) {
    var jDownloaderUrl: String
        get() = config.getString(APPLICATION_JDOWNLOADER_URL, Konstanten.JDOWNLOADER_URL)
        set(newValue) {
            config.setProperty(APPLICATION_JDOWNLOADER_URL, newValue)
        }

    fun setDefaultJDownloaderUrl() {
        jDownloaderUrl = Konstanten.JDOWNLOADER_URL
    }

    fun ensureJDownloaderUrlDefault() {
        if (!config.containsKey(APPLICATION_JDOWNLOADER_URL)) {
            setDefaultJDownloaderUrl()
        }
    }

    var directoryOpenProgram: String
        get() = config.getString(APPLICATION_DIRECTORY_OPEN_PROGRAM, "")
        set(newValue) {
            config.setProperty(APPLICATION_DIRECTORY_OPEN_PROGRAM, newValue)
        }

    var videoPlayerProgram: String
        get() = config.getString(APPLICATION_VIDEO_PLAYER_PROGRAM, "")
        set(newValue) {
            config.setProperty(APPLICATION_VIDEO_PLAYER_PROGRAM, newValue)
        }

    var webBrowserProgram: String
        get() = config.getString(APPLICATION_WEB_BROWSER_PROGRAM, "")
        set(newValue) {
            config.setProperty(APPLICATION_WEB_BROWSER_PROGRAM, newValue)
        }

    var linuxShutdownCommand: String
        get() = config.getString(APPLICATION_LINUX_SHUTDOWN_COMMAND, Konstanten.SHUTDOWN_LINUX)
            .ifEmpty { Konstanten.SHUTDOWN_LINUX }
        set(newValue) {
            config.setProperty(APPLICATION_LINUX_SHUTDOWN_COMMAND, newValue)
        }

    var standardVlcPath: String
        get() =
            if (!config.containsKey(APPLICATION_STANDARD_VLC_PATH)) {
                GuiFunktionenProgramme.getMusterPfadVlc()
            } else {
                config.getString(APPLICATION_STANDARD_VLC_PATH, "")
            }
        set(newValue) {
            config.setProperty(APPLICATION_STANDARD_VLC_PATH, newValue)
        }

    var standardFFmpegPath: String
        get() =
            if (!config.containsKey(APPLICATION_STANDARD_FFMPEG_PATH)) {
                GuiFunktionenProgramme.getMusterPfadFFmpeg()
            } else {
                config.getString(APPLICATION_STANDARD_FFMPEG_PATH, "")
            }
        set(newValue) {
            config.setProperty(APPLICATION_STANDARD_FFMPEG_PATH, newValue)
        }

    var pyLoadUrl: String
        get() = config.getString(APPLICATION_PYLOAD_URL, "")
        set(newValue) {
            config.setProperty(APPLICATION_PYLOAD_URL, newValue)
        }

    var pyLoadUser: String
        get() = config.getString(APPLICATION_PYLOAD_USER, "")
        set(newValue) {
            config.setProperty(APPLICATION_PYLOAD_USER, newValue)
        }

    var pyLoadPassword: String
        get() = config.getString(APPLICATION_PYLOAD_PASSWORD, "")
        set(newValue) {
            config.setProperty(APPLICATION_PYLOAD_PASSWORD, newValue)
        }

    private companion object {
        private const val APPLICATION_JDOWNLOADER_URL = "application.jdownloader.url"
        private const val APPLICATION_DIRECTORY_OPEN_PROGRAM = "application.directory_open.program"
        private const val APPLICATION_VIDEO_PLAYER_PROGRAM = "application.video_player.program"
        private const val APPLICATION_WEB_BROWSER_PROGRAM = "application.web_browser.program"
        private const val APPLICATION_LINUX_SHUTDOWN_COMMAND = "application.linux.shutdown.command"
        private const val APPLICATION_STANDARD_VLC_PATH = "application.standard_programs.vlc.path"
        private const val APPLICATION_STANDARD_FFMPEG_PATH = "application.standard_programs.ffmpeg.path"
        private const val APPLICATION_PYLOAD_URL = "application.pyload.url"
        private const val APPLICATION_PYLOAD_USER = "application.pyload.user"
        private const val APPLICATION_PYLOAD_PASSWORD = "application.pyload.password"
    }
}
