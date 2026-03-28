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

import mediathek.tool.subtitles.ttml2.SubtitleDocument.*;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

import javax.xml.XMLConstants;
import java.util.HashMap;
import java.util.Map;

/**
 * Parses TTML regions from head/layout/region.
 * Supports tts:origin and tts:extent with px and %.
 */
final class RegionIndex {

    private static final String NS_TTS = "http://www.w3.org/ns/ttml#styling";

    static Map<String, Region> parseRegions(Element tt, StyleIndex styles) {
        Map<String, Region> out = new HashMap<>();
        var head = XmlUtil.firstChild(tt, "head");
        if (head == null)
            return out;
        var layout = XmlUtil.firstChild(head, "layout");
        if (layout == null)
            return out;

        NodeList kids = layout.getChildNodes();
        for (int i = 0; i < kids.getLength(); i++) {
            var n = kids.item(i);
            if (n.getNodeType() != Node.ELEMENT_NODE)
                continue;
            var r = (Element) n;
            if (!"region".equals(r.getLocalName()))
                continue;

            String id = XmlUtil.attr(r, XMLConstants.XML_NS_URI, "id");
            if (id == null)
                continue;

            var cueStyle = styles.resolveCueStyle(r);

            var origin = parseLength2(XmlUtil.attr(r, NS_TTS, "origin"));
            var extent = parseLength2(XmlUtil.attr(r, NS_TTS, "extent"));

            out.put(id, new Region(id, origin, extent, cueStyle.displayAlign(), cueStyle.textAlign()));
        }
        return out;
    }

    static Length2 parseLength2(String v) {
        if (v == null)
            return null;
        String[] parts = v.trim().split("\\s+");
        if (parts.length != 2)
            return null;
        return new Length2(parseLength(parts[0]), parseLength(parts[1]));
    }

    static Length parseLength(String token) {
        token = token.trim();
        if (token.endsWith("px")) {
            return new Px(Double.parseDouble(token.substring(0, token.length() - 2)));
        }
        if (token.endsWith("%")) {
            return new Percent(Double.parseDouble(token.substring(0, token.length() - 1)));
        }
        throw new IllegalArgumentException("Unsupported length (only px and % supported): " + token);
    }
}
