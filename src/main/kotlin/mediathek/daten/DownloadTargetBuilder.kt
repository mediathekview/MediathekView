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

import mediathek.config.Konstanten
import mediathek.config.MVConfig
import mediathek.config.StandardLocations
import mediathek.daten.abo.DatenAbo
import mediathek.tool.FileSpecifier
import mediathek.tool.FileUtils
import mediathek.tool.FilenameUtils
import mediathek.tool.GuiFunktionen
import okhttp3.HttpUrl.Companion.toHttpUrlOrNull
import java.io.File
import java.time.LocalDate
import java.time.LocalTime
import java.time.format.DateTimeFormatter
import kotlin.math.abs

internal data class DownloadTarget(
    val fileName: String,
    val path: String,
    val pathFileName: String?,
    val aboName: String? = null,
)

internal data class DownloadTargetRequest(
    val pSet: DatenPset,
    val film: DatenFilm,
    val abo: DatenAbo?,
    val requestedFileName: String,
    val requestedPath: String,
    val downloadUrl: String,
    val topic: String,
    val title: String,
)

internal object DownloadTargetBuilder {
    private val HHMMSS = DateTimeFormatter.ofPattern("HHmmss")
    private val HH_MM_SS = DateTimeFormatter.ofPattern("HH:mm:ss")
    private val YYYYMMDD = DateTimeFormatter.ofPattern("yyyyMMdd")
    private val DATUM_FORMAT = DateTimeFormatter.ofPattern("dd.MM.yyyy")
    private const val TWO_LETTER_YEAR_PARAMETER = "%3_2"
    private const val FOUR_LETTER_YEAR_PARAMETER = "%3"

    fun build(request: DownloadTargetRequest): DownloadTarget {
        val pSet = request.pSet
        var name: String
        var path: String
        val cleanupOptions = FilenameCleanupOptions.current()
        if (!pSet.progsContainPath()) {
            return DownloadTarget(fileName = "", path = "", pathFileName = null)
        }

        if (request.requestedFileName.isNotEmpty()) {
            name = request.requestedFileName
        } else {
            name = pSet.getZielDateiname(request.downloadUrl)
                .ifEmpty { currentDateCompact() + '_' + request.topic + '-' + request.title + ".mp4" }
            name = replaceString(name, request, cleanupOptions)

            var suffix = ""
            if (name.contains(".")) {
                suffix = name.substring(name.lastIndexOf('.'))
                if (suffix.length in 2..4) {
                    name = name.substring(0, name.lastIndexOf('.'))
                } else {
                    suffix = ""
                }
            }

            name = FilenameUtils.replaceLeerDateiname(
                name,
                false,
                cleanupOptions.useReplaceTable,
                cleanupOptions.onlyAscii,
            )
            name += suffix

            if (name.length > 8) {
                val suffix1 = name.substring(name.length - 8, name.length - 4)
                val suffix2 = name.substring(name.length - 4)
                if (suffix1.startsWith(".") && suffix2.startsWith(".") && suffix1.equals(suffix2, ignoreCase = true)) {
                    name = name.substring(0, name.length - 4)
                }
            }

            if (pSet.isLaengeBeschraenken) {
                val length = pSet.maxLaenge ?: Konstanten.LAENGE_DATEINAME
                name = GuiFunktionen.cutName(name, length)
            }
        }

        var aboName: String? = null
        if (request.requestedPath.isNotEmpty()) {
            path = request.requestedPath
        } else {
            path = pSet.zielPfad.ifEmpty {
                StandardLocations.getStandardDownloadPath()
            }

            if (request.abo != null) {
                aboName = request.abo.name
                if (pSet.isThemaAnlegen) {
                    path = GuiFunktionen.addsPfad(
                        path,
                        FilenameUtils.removeIllegalCharacters(request.abo.zielpfad, true),
                    )
                }
            } else if (pSet.isThemaAnlegen) {
                path = GuiFunktionen.addsPfad(
                    path,
                    FilenameUtils.replaceLeerDateiname(
                        request.topic,
                        true,
                        cleanupOptions.useReplaceTable,
                        cleanupOptions.onlyAscii,
                    ),
                )
            }

            path = replaceString(path, request, cleanupOptions)
        }

        if (path.endsWith(File.separator)) {
            path = path.substring(0, path.length - 1)
        }

        path = path.ifEmpty { StandardLocations.getStandardDownloadPath() }
        name = name.ifEmpty { currentDateCompact() + '_' + request.topic + '-' + request.title + ".mp4" }

        val fileSpecifier = FileSpecifier(path, name)
        fileSpecifier.checkLength()

        return DownloadTarget(
            fileName = fileSpecifier.fileName,
            path = fileSpecifier.path,
            pathFileName = GuiFunktionen.addsPfad(fileSpecifier.path, fileSpecifier.fileName),
            aboName = aboName,
        )
    }

    private fun replaceResolutionParameter(replacement: String, film: DatenFilm, downloadUrl: String): String {
        val resolution = when (downloadUrl) {
            film.getUrlFuerAufloesung(FilmResolution.Enum.NORMAL) -> "H"
            film.getUrlFuerAufloesung(FilmResolution.Enum.HIGH_QUALITY) -> "HD"
            film.getUrlFuerAufloesung(FilmResolution.Enum.LOW) -> "L"
            else -> ""
        }
        return replacement.replace("%q", resolution)
    }

    private fun replaceYearParameter(replacement: String, film: DatenFilm): String {
        val date = film.sendeDatum.ifEmpty { currentDate() }
        var year = getDMY(DMYTag.YEAR, date)
        return if (replacement.contains(TWO_LETTER_YEAR_PARAMETER)) {
            year = year.substring(2)
            replacement.replace(TWO_LETTER_YEAR_PARAMETER, year)
        } else {
            replacement.replace(FOUR_LETTER_YEAR_PARAMETER, year)
        }
    }

    private fun currentDate(): String =
        LocalDate.now().format(DATUM_FORMAT)

    private fun replaceString(
        replacement: String,
        request: DownloadTargetRequest,
        cleanupOptions: FilenameCleanupOptions,
    ): String {
        var result = replacement
        val pSet = request.pSet
        val film = request.film
        val fieldLength = if (pSet.isLaengeFieldBeschraenken) {
            pSet.maxLaengeField ?: Konstanten.LAENGE_FELD
        } else {
            -1
        }

        result = result.replace("%t", getField(film.thema, fieldLength, cleanupOptions))
            .replace("%T", getField(film.title, fieldLength, cleanupOptions))
            .replace("%s", getField(film.sender, fieldLength, cleanupOptions))

        result = if (request.downloadUrl.endsWith(".m3u8") && request.downloadUrl.contains(".at")) {
            var field = getField(GuiFunktionen.getDateiName(request.downloadUrl), fieldLength, cleanupOptions)

            val url = request.downloadUrl.toHttpUrlOrNull()
            if (url != null) {
                val segments = url.pathSegments
                val segment = segments.getOrNull(segments.lastIndex - 1)
                if (segment != null) {
                    field = getField(GuiFunktionen.getDateiName(segment), fieldLength, cleanupOptions)
                    field = FileUtils.removeExtension(field)
                }
            }

            result.replace("%N", field)
        } else {
            result.replace("%N", getField(GuiFunktionen.getDateiName(request.downloadUrl), fieldLength, cleanupOptions))
        }

        result = result.replace(
            "%D",
            if (film.sendeDatum.isEmpty()) currentDateCompact() else stripDotsAndColons(rotateDate(film.sendeDatum)),
        ).replace(
            "%d",
            film.sendeZeit.ifEmpty { currentTimeCompact() }.let { stripDotsAndColons(it) },
        ).replace("%H", currentDateCompact())
            .replace("%h", currentTimeCompact())
            .replace("%1", getDMY(DMYTag.DAY, film.sendeDatum.ifEmpty { currentDate() }))
            .replace("%2", getDMY(DMYTag.MONTH, film.sendeDatum.ifEmpty { currentDate() }))

        result = replaceYearParameter(result, film)

        result = result.replace("%4", getHMS(HMSTag.HOUR, film.sendeZeit.ifEmpty { currentTime() }))
            .replace("%5", getHMS(HMSTag.MINUTE, film.sendeZeit.ifEmpty { currentTime() }))
            .replace("%6", getHMS(HMSTag.SECOND, film.sendeZeit.ifEmpty { currentTime() }))
            .replace("%i", System.currentTimeMillis().toString())

        result = replaceResolutionParameter(result, film, request.downloadUrl)

        result = result.replace("%S", GuiFunktionen.getSuffixFromUrl(request.downloadUrl))
            .replace("%Z", getHash(request.downloadUrl))
            .replace("%z", getHash(request.downloadUrl) + '.' + GuiFunktionen.getSuffixFromUrl(request.downloadUrl))

        return result
    }

    private fun getDMY(tag: DMYTag, date: String): String =
        if (date.length == 10) {
            when (tag) {
                DMYTag.DAY -> date.substring(0, 2)
                DMYTag.MONTH -> date.substring(3, 5)
                DMYTag.YEAR -> date.substring(6)
            }
        } else {
            ""
        }

    private fun getHMS(tag: HMSTag, time: String): String =
        if (time.length == 8) {
            when (tag) {
                HMSTag.HOUR -> time.substring(0, 2)
                HMSTag.MINUTE -> time.substring(3, 5)
                HMSTag.SECOND -> time.substring(6)
            }
        } else {
            ""
        }

    private fun stripDotsAndColons(date: String): String =
        date.replace(":", "").replace(".", "")

    private fun rotateDate(date: String): String =
        if (date.length == 10) {
            date.substring(6) + '.' + date.substring(3, 5) + '.' + date.substring(0, 2)
        } else {
            ""
        }

    private fun getHash(path: String): String {
        val hash = abs(path.hashCode())
        val paddedHash = StringBuilder(hash.toString())
        while (paddedHash.length < 10) {
            paddedHash.insert(0, '0')
        }
        return paddedHash.toString()
    }

    private fun getField(name: String, length: Int, cleanupOptions: FilenameCleanupOptions): String {
        var result = FilenameUtils.replaceLeerDateiname(
            name,
            false,
            cleanupOptions.useReplaceTable,
            cleanupOptions.onlyAscii,
        )

        if (length < 0) {
            return result
        }

        if (result.length > length) {
            result = result.substring(0, length)
        }
        return result
    }

    private fun currentTimeCompact(): String =
        LocalTime.now().format(HHMMSS)

    private fun currentTime(): String =
        LocalTime.now().format(HH_MM_SS)

    private fun currentDateCompact(): String =
        LocalDate.now().format(YYYYMMDD)

    private enum class DMYTag {
        DAY,
        MONTH,
        YEAR,
    }

    private enum class HMSTag {
        HOUR,
        MINUTE,
        SECOND,
    }

    private data class FilenameCleanupOptions(
        val useReplaceTable: Boolean,
        val onlyAscii: Boolean,
    ) {
        companion object {
            fun current(): FilenameCleanupOptions =
                FilenameCleanupOptions(
                    useReplaceTable = MVConfig.get(MVConfig.Configs.SYSTEM_USE_REPLACETABLE).toBoolean(),
                    onlyAscii = MVConfig.get(MVConfig.Configs.SYSTEM_ONLY_ASCII).toBoolean(),
                )
        }
    }
}
