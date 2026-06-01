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

import org.apache.logging.log4j.LogManager
import java.time.Duration
import java.time.Instant
import java.time.LocalDate
import java.time.LocalDateTime
import java.time.LocalTime
import java.time.ZoneId
import java.time.ZoneOffset
import java.time.ZonedDateTime
import java.time.format.DateTimeFormatter

class FilmListMetaData {
    var id: String = ""

    /**
     * Creation date/time of the filmlist stored in UTC.
     */
    private var creationDateTime: ZonedDateTime? = null

    var datum: String
        get() = FORMATTER.format(checkNotNull(creationDateTime))
        set(datum) {
            creationDateTime = LocalDateTime.parse(datum, FORMATTER).atZone(ZoneOffset.UTC)
        }

    val version: String
        get() = "3"

    val generationDateTimeAsString: String
        get() = try {
            FORMATTER.format(checkNotNull(creationDateTime).withZoneSameInstant(ZoneId.systemDefault()))
        } catch (_: Exception) {
            "0"
        }

    val ageInSeconds: Long
        get() = try {
            checkNotNull(age).seconds
        } catch (_: Exception) {
            0
        }

    private val age: Duration?
        get() = Duration.between(creationDateTime, Instant.now().atZone(ZoneId.systemDefault()))

    /**
     * Check if list is older than specified parameter.
     */
    fun isOlderThan(sekunden: Long): Boolean {
        val age = ageInSeconds
        if (age != 0L) {
            logger.info("Die Filmliste ist {} Minuten alt", age / 60)
        }
        return age > sekunden
    }

    /**
     * Check if Filmlist was created after today´s date 07:00Z so we can try to use diff lists.
     */
    fun canUseDiffList(): Boolean {
        val creationDateTime = creationDateTime ?: return false
        val firstPossibleDiffTime = ZonedDateTime.of(LocalDate.now(), LocalTime.of(7, 0), ZoneOffset.UTC)
        return creationDateTime.isAfter(firstPossibleDiffTime)
    }

    companion object {
        private val FORMATTER: DateTimeFormatter = DateTimeFormatter.ofPattern("dd.MM.yyyy, HH:mm")
        private val logger = LogManager.getLogger()
    }
}
