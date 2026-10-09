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

package mediathek.mainwindow

import mediathek.controller.starter.DownloadProgressSnapshot
import mediathek.controller.starter.DownloadServices
import mediathek.gui.messages.DownloadInfoUpdateAvailableEvent
import mediathek.tool.FileSize
import mediathek.tool.MessageBus
import net.engio.mbassy.listener.Handler
import javax.swing.JLabel
import javax.swing.SwingUtilities

class DownloadInformationLabel(
    private val downloads: DownloadServices,
) : JLabel() {
    init {
        MessageBus.messageBus.subscribe(this)
    }

    @Suppress("UNUSED_PARAMETER")
    @Handler
    private fun handleDownloadInfoUpdate(event: DownloadInfoUpdateAvailableEvent) {
        SwingUtilities.invokeLater(::setInfoFilme)
    }

    private fun setInfoFilme() {
        text = buildDownloadInfoText(downloads)
    }

    private fun buildDownloadInfoText(downloads: DownloadServices): String {
        val info = downloads.startInfo()
        return buildString {
            append(totalDownloadsText(info.total_num_download_list_entries))

            if (info.hasValues()) {
                append(": ")
                append(activeDownloadsText(info.running))
                appendRunningDetails(info.running, downloads.progressSnapshot())
                append(waitingDownloadsText(info.initialized))
                appendFinishedDownloads(info.finished)
                appendFailedDownloads(info.error)
            }
        }
    }

    private fun StringBuilder.appendRunningDetails(runningDownloads: Int, progress: DownloadProgressSnapshot) {
        if (runningDownloads <= 0) {
            return
        }

        appendBandwidth(progress.bandwidthText)
        appendDownloadSize(progress.activeBytes, progress.totalBytes)
    }

    private fun StringBuilder.appendBandwidth(bandwidth: String) {
        if (bandwidth.isNotEmpty()) {
            append(" ($bandwidth)")
        }
    }

    private fun StringBuilder.appendDownloadSize(byteAktDownloads: Long, byteAlleDownloads: Long) {
        if (byteAlleDownloads <= 0 && byteAktDownloads <= 0) {
            return
        }

        append(" (Größe: ")
        if (byteAktDownloads > 0) {
            append(FileSize.convertSize(byteAktDownloads))
                .append(" von ")
                .append(FileSize.convertSize(byteAlleDownloads))
                .append(" MByte)")
        } else {
            append(FileSize.convertSize(byteAlleDownloads))
                .append(" MByte)")
        }
    }

    private fun StringBuilder.appendFinishedDownloads(finishedDownloads: Int) {
        if (finishedDownloads > 0) {
            append(if (finishedDownloads == 1) ", 1 fertig" else ", $finishedDownloads fertig")
        }
    }

    private fun StringBuilder.appendFailedDownloads(failedDownloads: Int) {
        if (failedDownloads > 0) {
            append(if (failedDownloads == 1) ", 1 fehlerhaft" else ", $failedDownloads fehlerhaft")
        }
    }

    private fun totalDownloadsText(downloads: Int): String = if (downloads == 1) {
        "1 Download"
    } else {
        "$downloads Downloads"
    }

    private fun activeDownloadsText(downloads: Int): String = if (downloads == 1) {
        "1 läuft"
    } else {
        "$downloads laufen"
    }

    private fun waitingDownloadsText(downloads: Int): String = if (downloads == 1) {
        ", 1 wartet"
    } else {
        ", $downloads warten"
    }
}
