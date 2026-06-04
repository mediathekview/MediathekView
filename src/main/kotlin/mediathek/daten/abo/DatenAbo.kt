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

package mediathek.daten.abo

import mediathek.tool.GermanStringSorter
import mediathek.tool.datum.DateUtil
import mediathek.tool.table.ColumnVisibilityStore
import org.apache.logging.log4j.LogManager
import java.time.LocalDate
import java.time.format.DateTimeParseException
import javax.xml.stream.XMLStreamConstants
import javax.xml.stream.XMLStreamException
import javax.xml.stream.XMLStreamReader
import javax.xml.stream.XMLStreamWriter

class DatenAbo : Comparable<DatenAbo> {
    var mindestDauerMinuten: Int = 0
        set(value) {
            field = value.coerceAtLeast(0)
        }

    /**
     * Used internally for display in table.
     * Should NOT be used in code logic!!
     */
    var nr: Int = 0

    /**
     * Stores the active state of the abo.
     * On by default.
     */
    var isActive: Boolean = true

    /**
     * The display name.
     */
    var name: String = ""
    var sender: String = ""
    var thema: String = ""
    var title: String = ""
    var themaTitel: String = ""
    var irgendwo: String = ""
    var zielpfad: String = ""
    var downDatum: LocalDate? = null
    var psetName: String = ""
    var isDoNotStartAutomatically: Boolean = false

    /**
     * Whether or not to use minimum film length or maximum film length.
     */
    var filmLengthState: FilmLengthState = FilmLengthState.MINIMUM

    val downDatumText: String
        get() = downDatum?.format(DateUtil.FORMATTER).orEmpty()

    val isInvalid: Boolean
        get() = isInvalidFilter(sender, thema, title, themaTitel, irgendwo)

    fun setDownDatum(datum: String?) {
        if (datum.isNullOrBlank()) {
            downDatum = null
            return
        }

        downDatum = try {
            LocalDate.parse(datum, DateUtil.FORMATTER)
        } catch (ex: DateTimeParseException) {
            logger.error("Invalid down date: {}", datum, ex)
            null
        }
    }

    /**
     * Write all data to config.
     *
     * @param writer the writer used.
     */
    @Throws(XMLStreamException::class)
    fun writeToConfig(writer: XMLStreamWriter) {
        writer.writeStartElement(TAG)
        writer.writeCharacters("\n")

        // never write ABO_NR
        writeElement(writer, AboTags.EINGESCHALTET.xmlName, isActive.toString())
        writeElement(writer, AboTags.NAME.xmlName, name)
        writeElement(writer, AboTags.SENDER.xmlName, sender)
        writeElement(writer, AboTags.THEMA.xmlName, thema)
        writeElement(writer, AboTags.TITEL.xmlName, title)
        writeElement(writer, AboTags.THEMA_TITEL.xmlName, themaTitel)
        writeElement(writer, AboTags.IRGENDWO.xmlName, irgendwo)
        writeElement(writer, AboTags.MINDESTDAUER.xmlName, mindestDauerMinuten.toString())
        writeElement(writer, AboTags.MIN.xmlName, (filmLengthState == FilmLengthState.MINIMUM).toString())
        writeElement(writer, AboTags.ZIELPFAD.xmlName, zielpfad)
        writeElement(writer, AboTags.DOWN_DATUM.xmlName, downDatumText)
        writeElement(writer, AboTags.PSET.xmlName, psetName)
        writeElement(writer, AboTags.DO_NOT_START_AUTOMATICALLY.xmlName, isDoNotStartAutomatically.toString())

        writer.writeEndElement()
        writer.writeCharacters("\n")
    }

    @Throws(XMLStreamException::class)
    fun readFromConfig(parser: XMLStreamReader) {
        while (parser.hasNext()) {
            val event = parser.next()
            if (event == XMLStreamConstants.END_ELEMENT && parser.localName == TAG) {
                break
            }
            if (event == XMLStreamConstants.START_ELEMENT) {
                readElement(parser)
            }
        }
    }

    override fun compareTo(other: DatenAbo): Int =
        GermanStringSorter.compare(name, other.name)

    private fun readMindestdauer(text: String) {
        mindestDauerMinuten = try {
            text.toInt()
        } catch (ex: NumberFormatException) {
            logger.error("Invalid Mindestdauer value: {}", text, ex)
            0
        }
    }

    private fun readElement(parser: XMLStreamReader) {
        val tag = AboTags.fromXmlTag(parser.localName).orElse(null) ?: return

        try {
            val text = parser.elementText
            when (tag) {
                AboTags.EINGESCHALTET -> isActive = text.toBoolean()
                AboTags.MIN -> filmLengthState = if (text.toBoolean()) FilmLengthState.MINIMUM else FilmLengthState.MAXIMUM
                AboTags.NAME -> name = text
                AboTags.SENDER -> sender = text
                AboTags.THEMA -> thema = text
                AboTags.TITEL -> title = text
                AboTags.THEMA_TITEL -> themaTitel = text
                AboTags.IRGENDWO -> irgendwo = text
                AboTags.MINDESTDAUER -> readMindestdauer(text)
                AboTags.ZIELPFAD -> zielpfad = text
                AboTags.DOWN_DATUM -> setDownDatum(text)
                AboTags.PSET -> psetName = text
                AboTags.DO_NOT_START_AUTOMATICALLY -> isDoNotStartAutomatically = text.toBoolean()
                AboTags.NR -> Unit
            }
        } catch (ex: XMLStreamException) {
            logger.error("Error reading abo entry", ex)
        } catch (ex: RuntimeException) {
            logger.error("Error reading abo entry", ex)
        }
    }

    companion object {
        const val ABO_NR: Int = 0
        const val ABO_EINGESCHALTET: Int = 1
        const val ABO_NAME: Int = 2
        const val ABO_SENDER: Int = 3
        const val ABO_THEMA: Int = 4
        const val ABO_TITEL: Int = 5
        const val ABO_THEMA_TITEL: Int = 6
        const val ABO_IRGENDWO: Int = 7
        const val ABO_MINDESTDAUER: Int = 8
        const val ABO_MIN: Int = 9
        const val ABO_ZIELPFAD: Int = 10
        const val ABO_DOWN_DATUM: Int = 11
        const val ABO_PSET: Int = 12
        const val ABO_DO_NOT_START_AUTOMATICALLY: Int = 13
        const val ABO_REF: Int = 14
        const val MAX_ELEM: Int = 15
        const val TAG: String = "Abonnement"

        private val logger = LogManager.getLogger(DatenAbo::class.java)
        private val columnVisibilityStore = ColumnVisibilityStore.create(MAX_ELEM)

        fun anzeigen(i: Int): Boolean = columnVisibilityStore.isVisible(i)

        fun getColumnVisibilityStore(): ColumnVisibilityStore = columnVisibilityStore

        fun isInvalidFilter(sender: String, thema: String, title: String, themaTitel: String, irgendwo: String): Boolean =
            sender.isEmpty() && thema.isEmpty() && title.isEmpty() && themaTitel.isEmpty() && irgendwo.isEmpty()

        private fun writeElement(writer: XMLStreamWriter, tagName: String, content: String) {
            writer.writeCharacters("\t")
            writer.writeStartElement(tagName)
            writer.writeCharacters(content)
            writer.writeEndElement()
            writer.writeCharacters("\n")
        }
    }
}
