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

import mediathek.config.Konstanten
import mediathek.config.StandardLocations
import mediathek.config.application.ApplicationConfiguration
import mediathek.daten.DatenFilm
import mediathek.daten.DatenPset
import mediathek.daten.FilmResolution
import mediathek.daten.abo.DatenAbo
import mediathek.tool.*
import okhttp3.HttpUrl.Companion.toHttpUrlOrNull
import org.apache.logging.log4j.LogManager
import java.io.File
import java.time.LocalDate
import java.time.LocalTime
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
    val replacementRules: ReplacementRules? = null,
)

internal object DownloadTargetBuilder {
    private const val TWO_LETTER_YEAR_PARAMETER = "%3_2"
    private const val FOUR_LETTER_YEAR_PARAMETER = "%3"
    private val logger = LogManager.getLogger(DownloadTargetBuilder::class.java)

    fun build(request: DownloadTargetRequest): DownloadTarget {
        val pSet = request.pSet
        var name: String
        var path: String
        val cleanupOptions = FilenameCleanupOptions.current(request.replacementRules)
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

            name = FilenameUtils.replaceEmptyFilename(
                name,
                false,
                cleanupOptions.replacementRules,
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
                    FilenameUtils.replaceEmptyFilename(
                        request.topic,
                        true,
                        cleanupOptions.replacementRules,
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

    private fun replaceYearParameter(replacement: String, dateParts: CachedValue<DateParts>): String {
        if (!replacement.contains(FOUR_LETTER_YEAR_PARAMETER)) {
            return replacement
        }

        var year = dateParts.value.yearString
        return if (replacement.contains(TWO_LETTER_YEAR_PARAMETER)) {
            year = year.substring(2)
            replacement.replace(TWO_LETTER_YEAR_PARAMETER, year)
        } else {
            replacement.replace(FOUR_LETTER_YEAR_PARAMETER, year)
        }
    }

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

        val dateParts = CachedValue { DateParts.fromFilmOrCurrent(film) }
        val timeParts = CachedValue { TimeParts.fromFilmOrCurrent(film) }
        result = result.replaceParameter("%D") { dateParts.value.compact }
            .replaceParameter("%d") { timeParts.value.compact }
            .replaceParameter("%H") { DateParts.current().compact }
            .replaceParameter("%h") { TimeParts.current().compact }
            .replaceParameter("%1") { dateParts.value.dayString }
            .replaceParameter("%2") { dateParts.value.monthString }

        result = replaceYearParameter(result, dateParts)

        result = result.replaceParameter("%4") { timeParts.value.hourString }
            .replaceParameter("%5") { timeParts.value.minuteString }
            .replaceParameter("%6") { timeParts.value.secondString }
            .replaceParameter("%i") { System.currentTimeMillis().toString() }

        result = replaceResolutionParameter(result, film, request.downloadUrl)

        result = result.replace("%S", getSuffixFromUrl(request.downloadUrl))
            .replace("%Z", getHash(request.downloadUrl))
            .replace("%z", getHash(request.downloadUrl) + '.' + getSuffixFromUrl(request.downloadUrl))

        return result
    }

    private fun getHash(path: String): String {
        val hash = abs(path.hashCode())
        val paddedHash = StringBuilder(hash.toString())
        while (paddedHash.length < 10) {
            paddedHash.insert(0, '0')
        }
        return paddedHash.toString()
    }

    private fun getSuffixFromUrl(path: String): String {
        var result = if (path.isNotEmpty() && path.contains('.')) {
            path.substring(path.lastIndexOf('.') + 1)
        } else {
            ""
        }
        if (result.isEmpty()) {
            logger.error("getSuffixFromUrl({})", path)
        }
        if (result.contains("?")) {
            result = result.substring(0, result.indexOf('?'))
        }
        if (result.length > 5) {
            result = "---"
            logger.error("getSuffixFromUrl({})", path)
        }
        return result
    }

    private fun getField(name: String, length: Int, cleanupOptions: FilenameCleanupOptions): String {
        var result = FilenameUtils.replaceEmptyFilename(
            name,
            false,
            cleanupOptions.replacementRules,
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

    private inline fun String.replaceParameter(parameter: String, replacement: () -> String): String =
        if (contains(parameter)) {
            replace(parameter, replacement())
        } else {
            this
        }

    private class CachedValue<T>(
        private val initializer: () -> T,
    ) {
        private var cachedValue: T? = null

        val value: T
            get() {
                cachedValue?.let { return it }
                return initializer().also { cachedValue = it }
            }
    }

    private data class DateParts(
        val year: Int,
        val month: Int,
        val day: Int,
    ) {
        val compact: String
            get() = buildString(8) {
                appendFourDigits(year)
                appendTwoDigits(month)
                appendTwoDigits(day)
            }

        val dayString: String
            get() = twoDigits(day)

        val monthString: String
            get() = twoDigits(month)

        val yearString: String
            get() = buildString(4) {
                appendFourDigits(year)
            }

        companion object {
            fun fromFilmOrCurrent(film: DatenFilm): DateParts =
                film.sendeDatumEpochDay?.let { epochDay ->
                    fromLocalDate(LocalDate.ofEpochDay(epochDay.toLong()))
                } ?: current()

            fun current(): DateParts =
                fromLocalDate(LocalDate.now())

            private fun fromLocalDate(date: LocalDate): DateParts =
                DateParts(date.year, date.monthValue, date.dayOfMonth)
        }
    }

    private data class TimeParts(
        val hour: Int,
        val minute: Int,
        val second: Int,
    ) {
        val compact: String
            get() = buildString(6) {
                appendTwoDigits(hour)
                appendTwoDigits(minute)
                appendTwoDigits(second)
            }

        val hourString: String
            get() = twoDigits(hour)

        val minuteString: String
            get() = twoDigits(minute)

        val secondString: String
            get() = twoDigits(second)

        companion object {
            fun fromFilmOrCurrent(film: DatenFilm): TimeParts =
                film.sendeZeitSecondOfDay?.let(::fromSecondOfDay) ?: current()

            fun current(): TimeParts =
                LocalTime.now().let { time ->
                    TimeParts(time.hour, time.minute, time.second)
                }

            private fun fromSecondOfDay(secondOfDay: Int): TimeParts =
                TimeParts(
                    hour = secondOfDay / 3600,
                    minute = secondOfDay % 3600 / 60,
                    second = secondOfDay % 60,
                )
        }
    }

    private fun twoDigits(value: Int): String =
        buildString(2) {
            appendTwoDigits(value)
        }

    private fun StringBuilder.appendTwoDigits(value: Int) {
        append(('0'.code + value / 10).toChar())
        append(('0'.code + value % 10).toChar())
    }

    private fun StringBuilder.appendFourDigits(value: Int) {
        append(('0'.code + value / 1000 % 10).toChar())
        append(('0'.code + value / 100 % 10).toChar())
        append(('0'.code + value / 10 % 10).toChar())
        append(('0'.code + value % 10).toChar())
    }

    private fun currentDateCompact(): String =
        DateParts.current().compact

    private data class FilenameCleanupOptions(
        val replacementRules: ReplacementRules?,
        val onlyAscii: Boolean,
    ) {
        companion object {
            fun current(replacementRules: ReplacementRules?): FilenameCleanupOptions =
                ApplicationConfiguration.getInstance().let { applicationConfiguration ->
                    FilenameCleanupOptions(
                        replacementRules = replacementRules.takeIf { applicationConfiguration.useFilenameReplaceTable },
                        onlyAscii = applicationConfiguration.onlyAsciiFilenames,
                    )
                }
        }
    }
}
