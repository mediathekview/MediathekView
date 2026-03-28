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

package mediathek.tool.subtitles.ttml2;

import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

import javax.xml.XMLConstants;

final class XmlUtil {
    private XmlUtil() {
    }

    static String attr(Element el, String ns, String local) {
        if (ns == null) {
            return el.hasAttribute(local) ? emptyToNull(el.getAttribute(local)) : null;
        }
        return el.hasAttributeNS(ns, local) ? emptyToNull(el.getAttributeNS(ns, local)) : null;
    }

    static String emptyToNull(String s) {
        if (s == null)
            return null;
        s = s.trim();
        return s.isEmpty() ? null : s;
    }

    static Element firstChild(Element parent, String localName) {
        NodeList kids = parent.getChildNodes();
        for (int i = 0; i < kids.getLength(); i++) {
            Node n = kids.item(i);
            if (n.getNodeType() == Node.ELEMENT_NODE) {
                Element e = (Element) n;
                if (localName.equals(e.getLocalName()))
                    return e;
            }
        }
        return null;
    }

    static boolean xmlSpacePreserve(Element el) {
        String v = attr(el, XMLConstants.XML_NS_URI, "space");
        return v != null && v.equalsIgnoreCase("preserve");
    }
}
