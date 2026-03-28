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
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

import javax.xml.XMLConstants;
import javax.xml.parsers.DocumentBuilderFactory;
import java.io.InputStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.time.Duration;
import java.util.*;

/**
 * TTML2 parser focused on subtitle conversion.
 * Features:
 *  - Time parsing (clock-time + offset-time)
 *  - Basic timeContainer support on body/div (par/seq; default par)
 *  - Mixed span styling into runs (bold/italic/underline/color/backgroundColor)
 *  - Region parsing (origin/extent) for ASS placement
 * Notes:
 *  - wallclock() time expressions are rejected (not meaningful for SRT/ASS export).
 *  - Region units supported: px and %.
 */
public final class Ttml2Parser {

    private static final java.util.regex.Pattern BR_TEXT = java.util.regex.Pattern.compile("(?i)<br\\s*/?>");

    public SubtitleDocument parse(Path path) throws Exception {
        Document doc = parseXml(path);
        Element tt = doc.getDocumentElement();
        if (tt == null || !"tt".equals(tt.getLocalName())) {
            throw new IllegalArgumentException("Not a TTML document (missing <tt>)");
        }

        var timeCtx = TtmlTime.readTimeContext(tt);
        var styleIndex = StyleIndex.build(tt);
        Map<String, Region> regions = RegionIndex.parseRegions(tt, styleIndex);

        Element body = XmlUtil.firstChild(tt, "body");
        if (body == null) return new SubtitleDocument(regions, List.of());

        List<Cue> cues = new ArrayList<>();
        TimingScope root = new TimingScope(Duration.ZERO, null, "par", null, CueStyle.EMPTY);

        walkContainer(body, timeCtx, styleIndex, regions, root, cues);

        cues.sort(Comparator.comparing(Cue::start).thenComparing(Cue::end));
        return new SubtitleDocument(regions, cues);
    }

    private void walkContainer(Element container,
                               TtmlTime.TimeContext timeCtx,
                               StyleIndex styleIndex,
                               Map<String, Region> regions,
                               TimingScope parent,
                               List<Cue> out) {

        String timeContainer = XmlUtil.attr(container, null, "timeContainer");
        if (timeContainer == null) timeContainer = parent.timeContainer;
        if (timeContainer == null) timeContainer = "par";

        Duration begin = resolveBegin(container, parent.begin, timeCtx);
        Duration end = resolveEnd(container, begin, parent.end, timeCtx);

        String regionId = XmlUtil.attr(container, null, "region");
        if (regionId == null) regionId = parent.regionId;

        CueStyle cueStyle = parent.cueStyle.merge(styleIndex.resolveCueStyle(container));

        TimingScope scope = new TimingScope(begin, end, timeContainer, regionId, cueStyle);

        Duration seqCursor = scope.begin;

        NodeList kids = container.getChildNodes();
        for (int i = 0; i < kids.getLength(); i++) {
            Node n = kids.item(i);
            if (n.getNodeType() != Node.ELEMENT_NODE) continue;
            Element el = (Element) n;
            String ln = el.getLocalName();

            if ("div".equals(ln) || "body".equals(ln)) {
                if ("seq".equalsIgnoreCase(scope.timeContainer)) {
                    Duration childBegin = hasAttr(el, "begin")
                            ? TtmlTime.parseTimeExpression(XmlUtil.attr(el, null, "begin"), timeCtx)
                            : seqCursor;

                    TimingScope seqParent = new TimingScope(childBegin, scope.end, XmlUtil.attr(el, null, "timeContainer"), scope.regionId, scope.cueStyle);
                    walkContainer(el, timeCtx, styleIndex, regions, seqParent, out);

                    Duration adv = resolveEnd(el, childBegin, scope.end, timeCtx);
                    if (adv != null && adv.compareTo(seqCursor) > 0) seqCursor = adv;
                } else {
                    walkContainer(el, timeCtx, styleIndex, regions, scope, out);
                }
                continue;
            }

            if ("p".equals(ln)) {
                Cue cue = parseCue(el, timeCtx, styleIndex, scope, seqCursor);
                if (cue != null) {
                    out.add(cue);
                    if ("seq".equalsIgnoreCase(scope.timeContainer)) {
                        if (cue.end().compareTo(seqCursor) > 0) seqCursor = cue.end();
                    }
                }
            }
        }
    }

    private Cue parseCue(Element p,
                         TtmlTime.TimeContext timeCtx,
                         StyleIndex styleIndex,
                         TimingScope scope,
                         Duration seqCursor) {

        Duration begin = ("seq".equalsIgnoreCase(scope.timeContainer) && !hasAttr(p, "begin"))
                ? seqCursor
                : resolveBegin(p, scope.begin, timeCtx);

        Duration end = resolveEnd(p, begin, scope.end, timeCtx);
        if (end == null) return null;

        String regionId = XmlUtil.attr(p, null, "region");
        if (regionId == null) regionId = scope.regionId;

        CueStyle cueStyle = scope.cueStyle.merge(styleIndex.resolveCueStyle(p));

        boolean preserve = XmlUtil.xmlSpacePreserve(p);
        List<StyledRun> runs = new ArrayList<>();
        Deque<TextStyle> stack = new ArrayDeque<>();

        TextStyle base = styleIndex.resolveTextStyle(p);
        stack.push(base);
        collectRuns(p, styleIndex, stack, runs, preserve);

        runs = mergeAdjacent(runs);
        if (!preserve) runs = normalizeRunsWhitespace(runs);

        boolean anyText = runs.stream().anyMatch(r -> !r.text().isBlank() && !r.text().equals("\\n"));
        if (!anyText) return null;

        return new Cue(begin, end, regionId, runs, cueStyle);
    }

    private static void collectRuns(Node node,
                                    StyleIndex styles,
                                    Deque<TextStyle> stack,
                                    List<StyledRun> out,
                                    boolean preserveWhitespace) {

        NodeList kids = node.getChildNodes();
        for (int i = 0; i < kids.getLength(); i++) {
            Node n = kids.item(i);
            switch (n.getNodeType()) {
                case Node.TEXT_NODE -> {
                    String t = n.getNodeValue();
                    if (t == null || t.isEmpty()) break;

                    // Some TTML generators embed line breaks as escaped text, e.g. "&lt;br/&gt;".
                    // After XML entity decoding this becomes literal "<br/>" in the text node.
                    // Split such sequences into explicit newline runs.
                    int pos = 0;
                    java.util.regex.Matcher m = BR_TEXT.matcher(t);
                    while (m.find()) {
                        String before = t.substring(pos, m.start());
                        if (!before.isEmpty()) out.add(new StyledRun(before, stack.peek()));
                        out.add(new StyledRun("\\n", stack.peek()));
                        pos = m.end();
                    }
                    String tail = t.substring(pos);
                    if (!tail.isEmpty()) out.add(new StyledRun(tail, stack.peek()));
                }
                case Node.ELEMENT_NODE -> {
                    Element el = (Element) n;
                    String ln = el.getLocalName();

                    if ("br".equals(ln)) {
                        out.add(new StyledRun("\\n", stack.peek()));
                        continue;
                    }

                    if ("span".equals(ln)) {
                        TextStyle merged = stack.peek().merge(styles.resolveTextStyle(el));
                        stack.push(merged);
                        collectRuns(el, styles, stack, out, preserveWhitespace || XmlUtil.xmlSpacePreserve(el));
                        stack.pop();
                        continue;
                    }

                    collectRuns(el, styles, stack, out, preserveWhitespace || XmlUtil.xmlSpacePreserve(el));
                }
                default -> {}
            }
        }
    }

    private static List<StyledRun> mergeAdjacent(List<StyledRun> runs) {
        if (runs.isEmpty()) return runs;
        List<StyledRun> out = new ArrayList<>();
        StyledRun cur = runs.getFirst();
        StringBuilder sb = new StringBuilder(cur.text());

        for (int i = 1; i < runs.size(); i++) {
            StyledRun r = runs.get(i);
            if (Objects.equals(r.style(), cur.style())) {
                sb.append(r.text());
            } else {
                out.add(new StyledRun(sb.toString(), cur.style()));
                cur = r;
                sb = new StringBuilder(cur.text());
            }
        }
        out.add(new StyledRun(sb.toString(), cur.style()));
        return out;
    }

    private static List<StyledRun> normalizeRunsWhitespace(List<StyledRun> runs) {
        // Preserve explicit line breaks (\\n), normalize other whitespace.
        List<StyledRun> out = new ArrayList<>(runs.size());
        for (var r : runs) {
            String t = r.text().replace("\\n", "\u0000"); // protect newline sentinel
            t = t.replaceAll("[ \\t\\x0B\\f\\r]+", " ");
            t = t.replace("\u0000", "\\n");
            out.add(new StyledRun(t, r.style()));
        }

        // Trim spaces around newlines, and overall strip per run
        for (int i = 0; i < out.size(); i++) {
            var r = out.get(i);
            String t = r.text().replaceAll(" *\\\\n *", "\\\\n").strip();
            out.set(i, new StyledRun(t, r.style()));
        }
        return out;
    }

    private static Duration resolveBegin(Element el, Duration parentBegin, TtmlTime.TimeContext ctx) {
        String b = XmlUtil.attr(el, null, "begin");
        if (b == null) return parentBegin;
        return TtmlTime.parseTimeExpression(b, ctx);
    }

    private static Duration resolveEnd(Element el, Duration begin, Duration parentEnd, TtmlTime.TimeContext ctx) {
        String e = XmlUtil.attr(el, null, "end");
        String d = XmlUtil.attr(el, null, "dur");

        if (e != null) return TtmlTime.parseTimeExpression(e, ctx);
        if (d != null) return begin.plus(TtmlTime.parseTimeExpression(d, ctx));
        return parentEnd;
    }

    private static boolean hasAttr(Element el, String name) {
        return el.hasAttribute(name) && !el.getAttribute(name).isBlank();
    }

    private static Document parseXml(Path path) throws Exception {
        var dbf = DocumentBuilderFactory.newInstance();
        dbf.setNamespaceAware(true);
        dbf.setFeature(XMLConstants.FEATURE_SECURE_PROCESSING, true);
        dbf.setAttribute(XMLConstants.ACCESS_EXTERNAL_DTD, "");
        dbf.setAttribute(XMLConstants.ACCESS_EXTERNAL_SCHEMA, "");
        try (InputStream in = Files.newInputStream(path)) {
            return dbf.newDocumentBuilder().parse(in);
        }
    }

    private record TimingScope(Duration begin, Duration end, String timeContainer, String regionId, CueStyle cueStyle) {}
}
