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

package mediathek.tool.subtitles.ttml2

import mediathek.tool.subtitles.SubtitleDocument.*
import org.w3c.dom.Element
import org.w3c.dom.Node
import javax.xml.XMLConstants

/**
 * Parses TTML regions from head/layout/region.
 * Supports tts:origin and tts:extent with px and %.
 */
internal object RegionIndex {
    private const val NS_TTS = "http://www.w3.org/ns/ttml#styling"

    fun parseRegions(tt: Element, styles: StyleIndex): Map<String, Region> {
        val regions = HashMap<String, Region>()
        val head = XmlUtil.firstChild(tt, "head") ?: return regions
        val layout = XmlUtil.firstChild(head, "layout") ?: return regions

        val children = layout.childNodes
        for (index in 0 until children.length) {
            val node = children.item(index)
            if (node.nodeType != Node.ELEMENT_NODE) {
                continue
            }
            val region = node as Element
            if (region.localName != "region") {
                continue
            }

            val id = XmlUtil.attr(region, XMLConstants.XML_NS_URI, "id") ?: continue
            val cueStyle = styles.resolveCueStyle(region)
            val origin = parseLength2(XmlUtil.attr(region, NS_TTS, "origin"))
            val extent = parseLength2(XmlUtil.attr(region, NS_TTS, "extent"))

            regions[id] = Region(id, origin, extent, cueStyle.displayAlign, cueStyle.textAlign)
        }
        return regions
    }

    fun parseLength2(value: String?): Length2? {
        val parts = value?.trim()?.split(Regex("\\s+")) ?: return null
        if (parts.size != 2) {
            return null
        }
        return Length2(parseLength(parts[0]), parseLength(parts[1]))
    }

    fun parseLength(token: String): Length {
        val trimmed = token.trim()
        if (trimmed.endsWith("px")) {
            return Px(trimmed.dropLast(2).toDouble())
        }
        if (trimmed.endsWith("%")) {
            return Percent(trimmed.dropLast(1).toDouble())
        }
        throw IllegalArgumentException("Unsupported length (only px and % supported): $token")
    }
}
