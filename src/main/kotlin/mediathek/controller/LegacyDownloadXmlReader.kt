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

import mediathek.daten.DatenDownload
import javax.xml.stream.XMLStreamConstants
import javax.xml.stream.XMLStreamException
import javax.xml.stream.XMLStreamReader

internal object LegacyDownloadXmlReader {
    private val indexByXmlName: Map<String, Int> = listOf(
        "Nr",
        "Filmnr",
        "Abo",
        "Sender",
        "Thema",
        "Titel",
        "Button-Start",
        "Button-Del",
        "Fortschritt",
        "Restzeit",
        "Geschwindigkeit",
        "Groesse",
        "Datum",
        "Zeit",
        "Dauer",
        "HD",
        "UT",
        "Pause",
        "Geo",
        "Film-URL",
        "History-URL",
        "URL",
        "URL-rtmp",
        "URL-Untertitel",
        "Programmset",
        "Programm",
        "Programmaufruf_",
        "Programmaufruf",
        "Restart",
        "Dateiname",
        "Pfad",
        "Pfad-Dateiname",
        "Art",
        "Quelle",
        "Zurueckgestellt",
        "Infodatei",
        "Spotlight",
        "Untertitel",
        "Remote-Download",
        "Ref",
    ).withIndex().associate { (index, name) -> name to index }

    @Throws(XMLStreamException::class)
    fun read(parser: XMLStreamReader): DatenDownload {
        val download = DatenDownload()

        while (parser.hasNext()) {
            val event = parser.next()
            if (event == XMLStreamConstants.END_ELEMENT && parser.localName == DatenDownload.TAG) {
                break
            }
            if (event == XMLStreamConstants.START_ELEMENT) {
                val index = indexByXmlName[parser.localName] ?: -1
                if (index in 0 until DownloadColumn.COUNT) {
                    download.applyLegacyColumn(index, parser.elementText)
                }
            }
        }

        download.init()
        return download
    }
}
