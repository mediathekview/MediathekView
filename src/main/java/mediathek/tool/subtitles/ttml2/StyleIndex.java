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

import mediathek.tool.subtitles.ttml2.SubtitleDocument.CueStyle;
import mediathek.tool.subtitles.ttml2.SubtitleDocument.TextStyle;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

import javax.xml.XMLConstants;
import java.util.HashMap;
import java.util.Locale;
import java.util.Map;

/**
 * Resolves referential and inline styling.
 * Pragmatic cascade for subtitle export:
 * - bold/italic/underline are OR-merged
 * - colors are last-specified wins (inline overrides referenced)
 */
final class StyleIndex {

    private static final String NS_TTS = "http://www.w3.org/ns/ttml#styling";

    private final Map<String, TextStyle> textStylesById;
    private final Map<String, CueStyle> cueStylesById;

    private StyleIndex(Map<String, TextStyle> text, Map<String, CueStyle> cue) {
        this.textStylesById = text;
        this.cueStylesById = cue;
    }

    static StyleIndex build(Element tt) {
        Map<String, TextStyle> text = new HashMap<>();
        Map<String, CueStyle> cue = new HashMap<>();

        Element head = XmlUtil.firstChild(tt, "head");
        if (head == null)
            return new StyleIndex(text, cue);
        Element styling = XmlUtil.firstChild(head, "styling");
        if (styling == null)
            return new StyleIndex(text, cue);

        NodeList kids = styling.getChildNodes();
        for (int i = 0; i < kids.getLength(); i++) {
            Node n = kids.item(i);
            if (n.getNodeType() != Node.ELEMENT_NODE)
                continue;
            Element e = (Element) n;
            if (!"style".equals(e.getLocalName()))
                continue;

            String id = XmlUtil.attr(e, XMLConstants.XML_NS_URI, "id");
            if (id == null)
                continue;

            text.put(id, parseTextStyle(e));
            cue.put(id, parseCueStyle(e));
        }

        return new StyleIndex(text, cue);
    }

    private static TextStyle mergeText(TextStyle base, TextStyle over) {
        if (over == null)
            return base;
        return new TextStyle(
                base.bold() || over.bold(),
                base.italic() || over.italic(),
                base.underline() || over.underline(),
                over.color() != null ? over.color() : base.color(),
                over.backgroundColor() != null ? over.backgroundColor() : base.backgroundColor()
        );
    }

    static TextStyle parseTextStyle(Element el) {
        boolean bold = "bold".equalsIgnoreCase(XmlUtil.attr(el, NS_TTS, "fontWeight"));
        boolean italic = "italic".equalsIgnoreCase(XmlUtil.attr(el, NS_TTS, "fontStyle"));

        String deco = XmlUtil.attr(el, NS_TTS, "textDecoration");
        boolean underline = deco != null && deco.toLowerCase(Locale.ROOT).contains("underline");

        Rgba color = null;
        Rgba bg = null;

        String c = XmlUtil.attr(el, NS_TTS, "color");
        if (c != null)
            color = Ttml2Color.parse(c);

        String bc = XmlUtil.attr(el, NS_TTS, "backgroundColor");
        if (bc != null)
            bg = Ttml2Color.parse(bc);

        return new TextStyle(bold, italic, underline, color, bg);
    }

    static CueStyle parseCueStyle(Element el) {
        String displayAlign = XmlUtil.attr(el, NS_TTS, "displayAlign");
        String textAlign = XmlUtil.attr(el, NS_TTS, "textAlign");
        return new CueStyle(displayAlign, textAlign);
    }

    TextStyle resolveTextStyle(Element el) {
        TextStyle s = TextStyle.EMPTY;

        String refs = XmlUtil.attr(el, null, "style");
        if (refs != null) {
            for (String id : refs.trim().split("\\s+")) {
                TextStyle r = textStylesById.get(id);
                if (r != null)
                    s = mergeText(s, r);
            }
        }

        s = mergeText(s, parseTextStyle(el));
        return s;
    }

    CueStyle resolveCueStyle(Element el) {
        CueStyle s = CueStyle.EMPTY;

        String refs = XmlUtil.attr(el, null, "style");
        if (refs != null) {
            for (String id : refs.trim().split("\\s+")) {
                CueStyle r = cueStylesById.get(id);
                if (r != null)
                    s = s.merge(r);
            }
        }

        s = s.merge(parseCueStyle(el));
        return s;
    }
}
