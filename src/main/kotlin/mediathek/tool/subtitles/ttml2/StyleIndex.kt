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

import mediathek.tool.subtitles.SubtitleDocument.CueStyle
import mediathek.tool.subtitles.SubtitleDocument.TextStyle
import org.w3c.dom.Element
import org.w3c.dom.Node
import javax.xml.XMLConstants

/**
 * Resolves referential and inline styling.
 * Pragmatic cascade for subtitle export:
 * - bold/italic/underline are OR-merged
 * - colors are last-specified wins (inline overrides referenced)
 */
internal class StyleIndex private constructor(
    private val textStylesById: Map<String, TextStyle>,
    private val cueStylesById: Map<String, CueStyle>,
) {
    fun resolveTextStyle(element: Element): TextStyle {
        var style = TextStyle.EMPTY

        XmlUtil.attr(element, null, "style")?.trim()?.split(Regex("\\s+"))?.forEach { id ->
            textStylesById[id]?.let { referencedStyle ->
                style = mergeText(style, referencedStyle)
            }
        }

        return mergeText(style, parseTextStyle(element))
    }

    fun resolveCueStyle(element: Element): CueStyle {
        var style = CueStyle.EMPTY

        XmlUtil.attr(element, null, "style")?.trim()?.split(Regex("\\s+"))?.forEach { id ->
            cueStylesById[id]?.let { referencedStyle ->
                style = style.merge(referencedStyle)
            }
        }

        return style.merge(parseCueStyle(element))
    }

    companion object {
        private const val NS_TTS = "http://www.w3.org/ns/ttml#styling"

        fun build(tt: Element): StyleIndex {
            val textStyles = HashMap<String, TextStyle>()
            val cueStyles = HashMap<String, CueStyle>()

            val head = XmlUtil.firstChild(tt, "head") ?: return StyleIndex(textStyles, cueStyles)
            val styling = XmlUtil.firstChild(head, "styling") ?: return StyleIndex(textStyles, cueStyles)

            val children = styling.childNodes
            for (index in 0 until children.length) {
                val node = children.item(index)
                if (node.nodeType != Node.ELEMENT_NODE) {
                    continue
                }
                val element = node as Element
                if (element.localName != "style") {
                    continue
                }

                val id = XmlUtil.attr(element, XMLConstants.XML_NS_URI, "id") ?: continue

                textStyles[id] = parseTextStyle(element)
                cueStyles[id] = parseCueStyle(element)
            }

            return StyleIndex(textStyles, cueStyles)
        }

        private fun mergeText(base: TextStyle, over: TextStyle?): TextStyle =
            if (over == null) {
                base
            } else {
                TextStyle(
                    bold = base.bold || over.bold,
                    italic = base.italic || over.italic,
                    underline = base.underline || over.underline,
                    color = over.color ?: base.color,
                    backgroundColor = over.backgroundColor ?: base.backgroundColor,
                )
            }

        fun parseTextStyle(element: Element): TextStyle {
            val bold = XmlUtil.attr(element, NS_TTS, "fontWeight").equals("bold", ignoreCase = true)
            val italic = XmlUtil.attr(element, NS_TTS, "fontStyle").equals("italic", ignoreCase = true)
            val decoration = XmlUtil.attr(element, NS_TTS, "textDecoration")
            val underline = decoration?.lowercase()?.contains("underline") == true

            return TextStyle(
                bold = bold,
                italic = italic,
                underline = underline,
                color = XmlUtil.attr(element, NS_TTS, "color")?.let(Ttml2Color::parse),
                backgroundColor = XmlUtil.attr(element, NS_TTS, "backgroundColor")?.let(Ttml2Color::parse),
            )
        }

        fun parseCueStyle(element: Element): CueStyle =
            CueStyle(
                displayAlign = XmlUtil.attr(element, NS_TTS, "displayAlign"),
                textAlign = XmlUtil.attr(element, NS_TTS, "textAlign"),
            )
    }
}
