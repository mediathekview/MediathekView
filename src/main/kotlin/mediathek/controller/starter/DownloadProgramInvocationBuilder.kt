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
import mediathek.tool.ProcessCommandUtils

internal data class DownloadInvocation(
    val command: String,
    val commandArray: String,
    val automaticArteHlsProgramMap: Boolean = false,
)

internal data class DownloadInvocationRequest(
    val downloadUrl: String,
    val rtmpUrl: String,
    val targetPath: String,
    val targetFileName: String,
    val targetPathFileName: String,
    val websiteUrl: String,
    val subtitleUrl: String,
    val selectedResolution: FilmResolution.Enum? = null,
)

internal object DownloadProgramInvocationBuilder {
    private const val ARTE_HLS_WITH_SUBTITLES = "%arteHlsWithSubtitles"

    fun build(
        downloadType: DownloadType,
        program: DatenProg,
        request: DownloadInvocationRequest,
    ): DownloadInvocation {
        if (downloadType == DownloadType.DIRECT) return DownloadInvocation(command = "", commandArray = "")
        val separator = RuntimeExec.TRENNER_PROG_ARRAY
        val switches = program.switches.split(' ')
        val subtitleOption = switches.indexOf(ARTE_HLS_WITH_SUBTITLES)
        val includeSubtitles = subtitleOption >= 2 &&
            switches.subList(subtitleOption - 2, subtitleOption) == listOf("-i", "%f") &&
            switches.count { it == "-i" } == 1 &&
            !program.isDownloadManager && ProcessCommandUtils.isFfmpeg(program.programPath)
        val originalArray = program.programmAufrufArray.let {
            if (subtitleOption >= 0) it.withoutSubtitleOption(separator) else it
        }
        val originalCommand = program.programmAufruf.let {
            if (subtitleOption >= 0) it.withoutSubtitleOption(" ") else it
        }
        val mappedArray = originalArray.withArteHlsQualityMap(request, separator, includeSubtitles)
        return DownloadInvocation(
            command = replaceExec(originalCommand.withArteHlsQualityMap(request, " ", includeSubtitles), request),
            commandArray = replaceExec(mappedArray, request),
            automaticArteHlsProgramMap = mappedArray != originalArray,
        )
    }

    private fun replaceExec(command: String, request: DownloadInvocationRequest): String =
        command.replace("**", request.targetPathFileName)
            .replace("%f", request.downloadUrl)
            .replace("%F", request.rtmpUrl)
            .replace("%a", request.targetPath)
            .replace("%b", request.targetFileName)
            .replace("%w", request.websiteUrl)
            .replace("%u", request.subtitleUrl)

    private fun String.withoutSubtitleOption(separator: String): String =
        split(separator).filterNot { it == ARTE_HLS_WITH_SUBTITLES }.joinToString(separator)

    private fun String.withArteHlsQualityMap(
        request: DownloadInvocationRequest,
        separator: String,
        includeSubtitles: Boolean,
    ): String {
        val programId = request.arteHlsProgramId() ?: return this
        val inputMarker = "-i${separator}%f"
        if (!contains(inputMarker) || contains("${separator}-map${separator}")) {
            return this
        }
        val mapping = if (includeSubtitles) listOf("-map", "0:p:$programId")
        else listOf("-map", "p:$programId", "-map", "-0:s")
        return replace(inputMarker, inputMarker + separator + mapping.joinToString(separator))
    }

    private fun DownloadInvocationRequest.arteHlsProgramId(): Int? =
        ArteHlsQualitySelector.fallbackProgramId(downloadUrl, selectedResolution)
}
