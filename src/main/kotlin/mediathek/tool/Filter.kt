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

package mediathek.tool

import com.github.benmanes.caffeine.cache.Caffeine
import com.github.benmanes.caffeine.cache.LoadingCache
import mediathek.daten.DatenFilm
import org.apache.logging.log4j.LogManager
import java.util.*
import java.util.concurrent.ConcurrentHashMap
import java.util.regex.Pattern
import kotlin.time.Duration.Companion.minutes
import kotlin.time.toJavaDuration

object Filter {
    private val logger = LogManager.getLogger()

    /**
     * Stores the regexp strings that were rejected as invalid.
     */
    private val regExpErrorList: MutableSet<String> = ConcurrentHashMap.newKeySet()

    /**
     * The cache for already compiled RegExp.
     * Entries will be removed if the haven´t been accessed for more than 5 minutes.
     */
    private val cache: LoadingCache<String, Pattern> = Caffeine.newBuilder()
        .expireAfterAccess(5.minutes.toJavaDuration())
        .build { pattern -> compilePattern(pattern) }

    fun filterAufFilmPruefen(
        senderSuchen: String,
        themaSuchen: String,
        titelSuchen: Array<String>,
        themaTitelSuchen: Array<String>,
        irgendwoSuchen: Array<String>,
        film: DatenFilm,
    ): Boolean {
        // prüfen ob xxxSuchen im String imXxx enthalten ist, themaTitelSuchen wird mit Thema u. Titel verglichen
        // senderSuchen exakt mit sender
        // themaSuchen exakt mit thema
        // titelSuchen muss im Titel nur enthalten sein
        val thema = film.thema
        val title = film.title

        return senderConditionExists(senderSuchen, film) &&
            conditionExists(themaSuchen, thema) &&
            titleConditionExists(titelSuchen, title) &&
            themaTitelConditionExists(themaTitelSuchen, thema, title) &&
            irgendwoConditionExists(film, irgendwoSuchen, thema, title)
    }

    private fun irgendwoConditionExists(
        film: DatenFilm,
        irgendwoSuchen: Array<String>,
        thema: String,
        title: String,
    ): Boolean =
        irgendwoSuchen.isEmpty() ||
            pruefen(irgendwoSuchen, film.description) ||
            pruefen(irgendwoSuchen, thema) ||
            pruefen(irgendwoSuchen, title)

    private fun themaTitelConditionExists(themaTitelSuchen: Array<String>, thema: String, title: String): Boolean =
        themaTitelSuchen.isEmpty() ||
            pruefen(themaTitelSuchen, thema) ||
            pruefen(themaTitelSuchen, title)

    private fun titleConditionExists(titelSuchen: Array<String>, title: String): Boolean {
        // performance bottleneck
        return titelSuchen.isEmpty() || pruefen(titelSuchen, title)
    }

    private fun conditionExists(obj1: String, obj2: String): Boolean =
        obj1.isEmpty() || obj2.equals(obj1, ignoreCase = true)

    private fun senderConditionExists(senderSuchen: String, film: DatenFilm): Boolean {
        // performance bottleneck
        return senderSuchen.isEmpty() || film.sender.compareTo(senderSuchen) == 0
    }

    fun lengthCheck(filterLaengeInMinuten: Int, filmLaenge: Long): Boolean =
        filterLaengeInMinuten == 0 || filmLaenge == 0L

    private fun checkLengthNoMin(filterLaengeInMinuten: Int, filmLaenge: Long): Boolean {
        val filterLength = filterLaengeInMinuten * 60

        return lengthCheck(filterLaengeInMinuten, filmLaenge) || filmLaenge < filterLength
    }

    fun checkLengthWithMin(filterLaengeInMinuten: Int, filmLaenge: Long): Boolean {
        val filterLength = filterLaengeInMinuten * 60

        return lengthCheck(filterLaengeInMinuten, filmLaenge) || filmLaenge > filterLength
    }

    fun laengePruefen(filterLaengeInMinuten: Int, filmLaenge: Long, min: Boolean): Boolean =
        if (min) {
            checkLengthWithMin(filterLaengeInMinuten, filmLaenge)
        } else {
            checkLengthNoMin(filterLaengeInMinuten, filmLaenge)
        }

    fun pruefen(filter: Array<String>, im: String): Boolean {
        // wenn einer passt, dann ists gut
        val strFilter = filter[0]
        if (filter.size == 1) {
            if (strFilter.isEmpty()) {
                return true // Filter ist leer, das wars
            }

            makePattern(strFilter)?.let { pattern ->
                // dann ists eine RegEx
                return pattern.matcher(im).matches()
            }
        }

        return checkLowercase(filter, im.lowercase(Locale.getDefault()))
    }

    /**
     * @param filter the filters array
     * @param im checked String IN LOWERCASE!!!!!
     * @return true or false
     */
    fun checkLowercase(filter: Array<String>, im: String): Boolean =
        filter.any { token -> im.contains(token) }

    @JvmStatic
    fun isPattern(textSuchen: String): Boolean =
        textSuchen.startsWith("#:")

    /**
     * Compile a regexp pattern if it doesn´t exist in the pattern cache.
     *
     * @param regExpStr regexp to be compiled
     * @return the compiled regexp or null on error.
     */
    fun makePattern(regExpStr: String): Pattern? {
        if (!isPattern(regExpStr)) {
            return null
        }

        return try {
            cache[regExpStr]
        } catch (_: Exception) {
            logger.error("!!!!")
            logger.error("INVALID REGEX PATTERN DETECTED: {}", regExpStr)
            logger.error("!!! Please review your config files !!!")
            logger.error("!!!!")
            regExpErrorList.add(regExpStr)
            null
        }
    }

    /**
     * Create pattern without using the cache.
     * Used for interactive search field where cache pollution is not wanted.
     *
     * @param regExpStr the regexp pattern
     * @return Pattern if successful, otherwise null.
     */
    @JvmStatic
    fun makePatternNoCache(regExpStr: String): Pattern? {
        if (!isPattern(regExpStr)) {
            return null
        }

        return try {
            val regexPattern = regExpStr.substring(2)
            Pattern.compile(regexPattern, Pattern.CASE_INSENSITIVE or Pattern.UNICODE_CASE or Pattern.DOTALL)
        } catch (_: IllegalArgumentException) {
            null
        }
    }

    @JvmStatic
    fun drainRegExpErrors(): Set<String> {
        val errors = ConcurrentHashMap.newKeySet<String>()
        regExpErrorList.removeIf(errors::add)
        return errors
    }

    private fun compilePattern(pattern: String): Pattern {
        logger.trace("COMPILING PATTERN: {}", pattern)
        val regexPattern = pattern.substring(2)

        return Pattern.compile(regexPattern, Pattern.CASE_INSENSITIVE or Pattern.UNICODE_CASE or Pattern.DOTALL)
    }
}
