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

package mediathek.gui.dialogEinstellungen

import mediathek.config.Konstanten
import mediathek.config.application.ApplicationConfiguration
import java.awt.Toolkit

class PanelDownload : PanelDownloadBase() {
    init {
        configureComponentMetadata()

        val config = ApplicationConfiguration.getInstance()

        cbkDownloadError.isSelected = config.showDownloadErrorMessage
        cbkDownloadError.addActionListener {
            config.showDownloadErrorMessage = cbkDownloadError.isSelected
        }

        jCheckBoxBeep.isSelected = config.playSoundAfterDownload
        jCheckBoxBeep.addActionListener {
            config.playSoundAfterDownload = jCheckBoxBeep.isSelected
        }

        cbFetchMissingFileSize.isSelected = config.fetchMissingDownloadFileSize
        cbFetchMissingFileSize.addActionListener {
            config.fetchMissingDownloadFileSize = cbFetchMissingFileSize.isSelected
        }

        jButtonBeep.addActionListener { Toolkit.getDefaultToolkit().beep() }

        val configuredContinuationTime = config.downloadContinuationTime
        spDefaultDownloadContinuation.value =
            if (configuredContinuationTime in 1..Konstanten.DOWNLOAD_CONTINUATION_DEFAULT_TIME) {
                configuredContinuationTime
            } else {
                Konstanten.DOWNLOAD_CONTINUATION_DEFAULT_TIME
            }
        spDefaultDownloadContinuation.addChangeListener {
            config.downloadContinuationTime = spDefaultDownloadContinuation.value as Int
        }
    }

    private fun configureComponentMetadata() {
        cbkDownloadError.name = PanelDownloadComponentNames.SHOW_DOWNLOAD_ERROR_MESSAGE
        jCheckBoxBeep.name = PanelDownloadComponentNames.PLAY_SOUND_AFTER_DOWNLOAD
        jButtonBeep.name = PanelDownloadComponentNames.TEST_SOUND
        cbFetchMissingFileSize.name = PanelDownloadComponentNames.FETCH_MISSING_FILE_SIZE
        spDefaultDownloadContinuation.name = PanelDownloadComponentNames.DOWNLOAD_CONTINUATION_TIME
    }
}

internal object PanelDownloadComponentNames {
    const val SHOW_DOWNLOAD_ERROR_MESSAGE = "PanelDownload.showDownloadErrorMessage"
    const val PLAY_SOUND_AFTER_DOWNLOAD = "PanelDownload.playSoundAfterDownload"
    const val TEST_SOUND = "PanelDownload.testSound"
    const val FETCH_MISSING_FILE_SIZE = "PanelDownload.fetchMissingFileSize"
    const val DOWNLOAD_CONTINUATION_TIME = "PanelDownload.downloadContinuationTime"
}
