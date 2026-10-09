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

import org.w3c.dom.Element
import org.w3c.dom.Node
import javax.xml.XMLConstants

internal object XmlUtil {
    fun attr(element: Element, namespace: String?, localName: String): String? =
        if (namespace == null) {
            if (element.hasAttribute(localName)) {
                emptyToNull(element.getAttribute(localName))
            } else {
                null
            }
        } else if (element.hasAttributeNS(namespace, localName)) {
            emptyToNull(element.getAttributeNS(namespace, localName))
        } else {
            null
        }

    fun emptyToNull(value: String?): String? = value?.trim()?.takeIf { it.isNotEmpty() }

    fun firstChild(parent: Element, localName: String): Element? {
        val children = parent.childNodes
        for (index in 0 until children.length) {
            val node = children.item(index)
            if (node.nodeType == Node.ELEMENT_NODE) {
                val element = node as Element
                if (localName == element.localName) {
                    return element
                }
            }
        }
        return null
    }

    fun xmlSpacePreserve(element: Element): Boolean =
        attr(element, XMLConstants.XML_NS_URI, "space").equals("preserve", ignoreCase = true)
}
