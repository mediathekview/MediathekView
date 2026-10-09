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

package mediathek.controller.starter

import mediathek.daten.DatenProg
import mediathek.daten.DownloadType
import mediathek.daten.FilmResolution
import mediathek.tool.ArteHlsQualitySelector

internal data class DownloadInvocation(
    val command: String,
    val commandArray: String,
)

internal data class DownloadInvocationRequest(
    val downloadUrl: String,
    val rtmpUrl: String,
    val targetPath: String,
    val targetFileName: String,
    val targetPathFileName: String,
    val websiteUrl: String,
    val selectedResolution: FilmResolution.Enum? = null,
)

internal object DownloadProgramInvocationBuilder {
    fun build(
        downloadType: DownloadType,
        program: DatenProg,
        request: DownloadInvocationRequest,
    ): DownloadInvocation =
        if (downloadType == DownloadType.DIRECT) {
            DownloadInvocation(command = "", commandArray = "")
        } else {
            DownloadInvocation(
                command = replaceExec(program.programmAufruf.withArteHlsQualityMap(request, " "), request),
                commandArray = replaceExec(
                    program.programmAufrufArray.withArteHlsQualityMap(request, RuntimeExec.TRENNER_PROG_ARRAY),
                    request,
                ),
            )
        }

    private fun replaceExec(command: String, request: DownloadInvocationRequest): String =
        command.replace("**", request.targetPathFileName)
            .replace("%f", request.downloadUrl)
            .replace("%F", request.rtmpUrl)
            .replace("%a", request.targetPath)
            .replace("%b", request.targetFileName)
            .replace("%w", request.websiteUrl)

    private fun String.withArteHlsQualityMap(request: DownloadInvocationRequest, separator: String): String {
        val programId = request.arteHlsProgramId() ?: return this
        val inputMarker = "-i${separator}%f"
        if (!contains(inputMarker) || contains("${separator}-map${separator}")) {
            return this
        }
        return replace(
            inputMarker,
            "$inputMarker${separator}-map${separator}p:$programId${separator}-map${separator}-0:s",
        )
    }

    private fun DownloadInvocationRequest.arteHlsProgramId(): Int? =
        ArteHlsQualitySelector.programId(downloadUrl, selectedResolution)
}
