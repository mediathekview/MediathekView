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
package mediathek.controller

import mediathek.daten.abo.AboTags
import mediathek.daten.abo.DatenAbo
import mediathek.daten.abo.FilmLengthState
import mediathek.tool.datum.DateUtil
import org.apache.logging.log4j.LogManager
import java.time.LocalDate
import java.time.format.DateTimeParseException
import javax.xml.stream.XMLStreamConstants
import javax.xml.stream.XMLStreamException
import javax.xml.stream.XMLStreamReader

object LegacyAboRuleXml {
    const val TAG = "Abonnement"
    private val logger = LogManager.getLogger()

    @Throws(XMLStreamException::class)
    fun readAbo(parser: XMLStreamReader): DatenAbo {
        val abo = DatenAbo()
        while (parser.hasNext()) {
            val event = parser.next()
            if (event == XMLStreamConstants.END_ELEMENT && parser.localName == TAG) {
                break
            }

            if (event == XMLStreamConstants.START_ELEMENT) {
                AboTags.fromXmlTag(parser.localName).ifPresent { tag ->
                    readAboField(parser, tag, abo)
                }
            }
        }
        return abo
    }

    private fun readAboField(parser: XMLStreamReader, tag: AboTags, abo: DatenAbo) {
        try {
            val text = parser.elementText
            when (tag) {
                AboTags.EINGESCHALTET -> abo.isActive = text.toBoolean()
                AboTags.MIN -> abo.filmLengthState = if (text.toBoolean()) {
                    FilmLengthState.MINIMUM
                } else {
                    FilmLengthState.MAXIMUM
                }

                AboTags.NAME -> abo.name = text
                AboTags.SENDER -> abo.sender = text
                AboTags.THEMA -> abo.thema = text
                AboTags.TITEL -> abo.title = text
                AboTags.THEMA_TITEL -> abo.themaTitel = text
                AboTags.IRGENDWO -> abo.irgendwo = text
                AboTags.MINDESTDAUER -> abo.mindestDauerMinuten = parseMinimumDuration(text)
                AboTags.ZIELPFAD -> abo.zielpfad = text
                AboTags.DOWN_DATUM -> abo.downloadDate = parseDownloadDate(text)
                AboTags.PSET -> abo.psetName = text
                AboTags.DO_NOT_START_AUTOMATICALLY -> abo.isDoNotStartAutomatically = text.toBoolean()
            }
        } catch (ex: XMLStreamException) {
            logger.error("Error reading abo entry", ex)
        } catch (ex: RuntimeException) {
            logger.error("Error reading abo entry", ex)
        }
    }

    private fun parseMinimumDuration(text: String): Int =
        try {
            text.toInt()
        } catch (ex: NumberFormatException) {
            logger.error("Invalid Mindestdauer value: {}", text, ex)
            0
        }

    private fun parseDownloadDate(text: String?): LocalDate? {
        if (text.isNullOrBlank()) {
            return null
        }

        return try {
            LocalDate.parse(text, DateUtil.FORMATTER)
        } catch (ex: DateTimeParseException) {
            logger.error("Invalid download date: {}", text, ex)
            null
        }
    }
}
