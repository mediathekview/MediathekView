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

import mediathek.daten.blacklist.BlacklistRule
import mediathek.daten.blacklist.BlacklistTags
import mediathek.daten.blacklist.BlacklistTags.Companion.fromXmlTag
import org.apache.logging.log4j.LogManager
import javax.xml.stream.XMLStreamConstants
import javax.xml.stream.XMLStreamException
import javax.xml.stream.XMLStreamReader

object LegacyBlacklistRuleXml {
    const val TAG = "Blacklist"
    private val logger = LogManager.getLogger()

    @Throws(XMLStreamException::class)
    fun readRule(parser: XMLStreamReader): BlacklistRule {
        val rule = BlacklistRule()
        while (parser.hasNext()) {
            val event = parser.next()
            if (event == XMLStreamConstants.END_ELEMENT && parser.localName == TAG) {
                break
            }

            if (event == XMLStreamConstants.START_ELEMENT) {
                fromXmlTag(parser.localName).ifPresent { tag ->
                    readRuleField(parser, tag, rule)
                }
            }
        }
        return rule
    }

    private fun readRuleField(parser: XMLStreamReader, tag: BlacklistTags, rule: BlacklistRule) {
        try {
            val text = parser.elementText
            when (tag) {
                BlacklistTags.SENDER -> rule.sender = text
                BlacklistTags.THEMA -> rule.thema = text
                BlacklistTags.TITEL -> rule.titel = text
                BlacklistTags.THEMA_TITEL -> rule.thema_titel = text
            }
        } catch (ex: XMLStreamException) {
            logger.error("Error reading blacklist rule entry", ex)
        }
    }
}
