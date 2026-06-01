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

package mediathek.daten

import kotlinx.serialization.Serializable

@Serializable
internal data class DownloadConfig(
    val aboName: String = "",
    val sender: String = "",
    val topic: String = "",
    val title: String = "",
    val sizeInMiB: Long = 0,
    val date: String = "",
    val time: String = "",
    val duration: String = "",
    val interrupted: Boolean = false,
    val filmUrl: String = "",
    val historyUrl: String = "",
    val url: String = "",
    val rtmpUrl: String = "",
    val subtitleUrl: String = "",
    val programSet: String = "",
    val program: String = "",
    val programInvocation: String = "",
    val programInvocationArray: String = "",
    val restart: Boolean = false,
    val targetFileName: String = "",
    val targetPath: String = "",
    val targetPathFileName: String = "",
    val type: DownloadType = DownloadType.DIRECT,
    val source: DownloadSource = DownloadSource.ALL,
    val deferred: Boolean = false,
    val infoFile: Boolean = false,
    val spotlight: Boolean = false,
    val subtitle: Boolean = false,
    val downloadManager: Boolean = false,
)
