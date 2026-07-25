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

import mediathek.tool.subtitles.SubtitleDocument
import mediathek.tool.subtitles.SubtitleDocument.*
import org.w3c.dom.Document
import org.w3c.dom.Element
import org.w3c.dom.Node
import java.io.ByteArrayInputStream
import java.io.InputStream
import java.nio.charset.StandardCharsets
import java.nio.file.Files
import java.nio.file.Path
import java.time.Duration
import java.util.*
import java.util.regex.Pattern
import javax.xml.XMLConstants
import javax.xml.parsers.DocumentBuilderFactory

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
class Ttml2Parser {
    fun parse(path: Path): SubtitleDocument {
        val doc = Files.newInputStream(path).use(::parseXml)
        return parse(doc)
    }

    fun parse(content: String): SubtitleDocument =
        ByteArrayInputStream(content.toByteArray(StandardCharsets.UTF_8)).use { inputStream ->
            parse(inputStream)
        }

    fun parse(inputStream: InputStream): SubtitleDocument {
        val doc = parseXml(inputStream)
        return parse(doc)
    }

    private fun parse(doc: Document): SubtitleDocument {
        val tt = requireNotNull(doc.documentElement) { "Not a TTML document (missing <tt>)" }
        require(tt.localName == "tt") { "Not a TTML document (missing <tt>)" }

        val timeCtx = TtmlTime.readTimeContext(tt)
        val styleIndex = StyleIndex.build(tt)
        val regions = RegionIndex.parseRegions(tt, styleIndex)

        val body = XmlUtil.firstChild(tt, "body") ?: return SubtitleDocument(regions, emptyList())

        val cues = ArrayList<Cue>()
        val root = TimingScope(Duration.ZERO, null, "par", null, CueStyle.EMPTY)

        walkContainer(body, timeCtx, styleIndex, regions, root, cues)

        cues.sortWith(compareBy(Cue::start).thenBy(Cue::end))
        return SubtitleDocument(regions, cues)
    }

    private fun walkContainer(
        container: Element,
        timeCtx: TtmlTime.TimeContext,
        styleIndex: StyleIndex,
        regions: Map<String, Region>,
        parent: TimingScope,
        out: MutableList<Cue>,
    ) {
        val timeContainer = XmlUtil.attr(container, null, "timeContainer") ?: parent.timeContainer ?: "par"
        val begin = resolveBegin(container, parent.begin, timeCtx)
        val end = resolveEnd(container, begin, parent.end, timeCtx)
        val regionId = XmlUtil.attr(container, null, "region") ?: parent.regionId
        val cueStyle = parent.cueStyle.merge(styleIndex.resolveCueStyle(container))
        val scope = TimingScope(begin, end, timeContainer, regionId, cueStyle)

        var seqCursor = scope.begin

        val kids = container.childNodes
        for (i in 0 until kids.length) {
            val node = kids.item(i)
            if (node.nodeType != Node.ELEMENT_NODE) {
                continue
            }

            val element = node as Element
            when (element.localName) {
                "div", "body" -> {
                    if (scope.timeContainer.equals("seq", ignoreCase = true)) {
                        val childBegin =
                            if (hasBeginAttribute(element)) {
                                TtmlTime.parseTimeExpression(XmlUtil.attr(element, null, "begin"), timeCtx)
                                    ?: seqCursor
                            } else {
                                seqCursor
                            }

                        val seqParent =
                            TimingScope(
                                childBegin,
                                scope.end,
                                XmlUtil.attr(element, null, "timeContainer"),
                                scope.regionId,
                                scope.cueStyle,
                            )
                        walkContainer(element, timeCtx, styleIndex, regions, seqParent, out)

                        val advance = resolveEnd(element, childBegin, scope.end, timeCtx)
                        if (advance != null && advance > seqCursor) {
                            seqCursor = advance
                        }
                    } else {
                        walkContainer(element, timeCtx, styleIndex, regions, scope, out)
                    }
                }

                "p" -> {
                    val cue = parseCue(element, timeCtx, styleIndex, scope, seqCursor)
                    if (cue != null) {
                        out.add(cue)
                        if (scope.timeContainer.equals("seq", ignoreCase = true) && cue.end > seqCursor) {
                            seqCursor = cue.end
                        }
                    }
                }
            }
        }
    }

    private fun parseCue(
        paragraph: Element,
        timeCtx: TtmlTime.TimeContext,
        styleIndex: StyleIndex,
        scope: TimingScope,
        seqCursor: Duration,
    ): Cue? {
        val begin =
            if (scope.timeContainer.equals("seq", ignoreCase = true) && !hasBeginAttribute(paragraph)) {
                seqCursor
            } else {
                resolveBegin(paragraph, scope.begin, timeCtx)
            }

        val end = resolveEnd(paragraph, begin, scope.end, timeCtx) ?: return null
        val regionId = XmlUtil.attr(paragraph, null, "region") ?: scope.regionId
        val cueStyle = scope.cueStyle.merge(styleIndex.resolveCueStyle(paragraph))
        val preserve = XmlUtil.xmlSpacePreserve(paragraph)
        val runs = ArrayList<StyledRun>()
        val stack: Deque<TextStyle> = ArrayDeque()

        stack.push(styleIndex.resolveTextStyle(paragraph))
        collectRuns(paragraph, styleIndex, stack, runs, preserve)

        var mergedRuns = mergeAdjacent(runs)
        if (!preserve) {
            mergedRuns = normalizeRunsWhitespace(mergedRuns)
        }

        val anyText = mergedRuns.any { it.text.isNotBlank() && it.text != "\\n" }
        if (!anyText) {
            return null
        }

        return Cue(begin, end, regionId, mergedRuns, cueStyle)
    }

    private fun collectRuns(
        node: Node,
        styles: StyleIndex,
        stack: Deque<TextStyle>,
        out: MutableList<StyledRun>,
        preserveWhitespace: Boolean,
    ) {
        val kids = node.childNodes
        for (i in 0 until kids.length) {
            when (val child = kids.item(i)) {
                is Element -> {
                    when (child.localName) {
                        "br" -> out.add(StyledRun("\\n", stack.peek()))
                        "span" -> {
                            val merged = stack.peek().merge(styles.resolveTextStyle(child))
                            stack.push(merged)
                            collectRuns(child, styles, stack, out, preserveWhitespace || XmlUtil.xmlSpacePreserve(child))
                            stack.pop()
                        }

                        else -> collectRuns(child, styles, stack, out, preserveWhitespace || XmlUtil.xmlSpacePreserve(child))
                    }
                }

                else -> {
                    if (child.nodeType != Node.TEXT_NODE) {
                        continue
                    }

                    val text = child.nodeValue
                    if (text.isNullOrEmpty()) {
                        continue
                    }

                    // Some TTML generators embed line breaks as escaped text, e.g. "&lt;br/&gt;".
                    // After XML entity decoding this becomes literal "<br/>" in the text node.
                    // Split such sequences into explicit newline runs.
                    var position = 0
                    val matcher = BR_TEXT.matcher(text)
                    while (matcher.find()) {
                        val before = text.substring(position, matcher.start())
                        if (before.isNotEmpty()) {
                            out.add(StyledRun(before, stack.peek()))
                        }
                        out.add(StyledRun("\\n", stack.peek()))
                        position = matcher.end()
                    }
                    val tail = text.substring(position)
                    if (tail.isNotEmpty()) {
                        out.add(StyledRun(tail, stack.peek()))
                    }
                }
            }
        }
    }

    private fun mergeAdjacent(runs: List<StyledRun>): List<StyledRun> {
        if (runs.isEmpty()) {
            return runs
        }

        val out = ArrayList<StyledRun>()
        var current = runs.first()
        var text = StringBuilder(current.text)

        for (run in runs.drop(1)) {
            if (run.style == current.style) {
                text.append(run.text)
            } else {
                out.add(StyledRun(text.toString(), current.style))
                current = run
                text = StringBuilder(current.text)
            }
        }

        out.add(StyledRun(text.toString(), current.style))
        return out
    }

    private fun normalizeRunsWhitespace(runs: List<StyledRun>): List<StyledRun> {
        val out = ArrayList<StyledRun>(runs.size)
        for (run in runs) {
            val normalized =
                run.text
                    .replace("\\n", "\u0000")
                    .replace(Regex("[ \\t\\x0B\\f\\r]+"), " ")
                    .replace("\u0000", "\\n")
            out.add(StyledRun(normalized, run.style))
        }

        for (i in out.indices) {
            val run = out[i]
            val normalized = run.text.replace(Regex(" *\\\\n *"), "\\\\n").trim()
            out[i] = StyledRun(normalized, run.style)
        }
        return out
    }

    private fun resolveBegin(el: Element, parentBegin: Duration, ctx: TtmlTime.TimeContext): Duration {
        val begin = XmlUtil.attr(el, null, "begin") ?: return parentBegin
        return TtmlTime.parseTimeExpression(begin, ctx) ?: parentBegin
    }

    private fun resolveEnd(
        el: Element,
        begin: Duration,
        parentEnd: Duration?,
        ctx: TtmlTime.TimeContext,
    ): Duration? {
        val end = XmlUtil.attr(el, null, "end")
        val duration = XmlUtil.attr(el, null, "dur")

        return when {
            end != null -> TtmlTime.parseTimeExpression(end, ctx)
            duration != null -> TtmlTime.parseTimeExpression(duration, ctx)?.let { begin.plus(it) }
            else -> parentEnd
        }
    }

    private fun hasBeginAttribute(element: Element): Boolean =
        element.hasAttribute("begin") && element.getAttribute("begin").isNotBlank()

    private fun parseXml(inputStream: InputStream): Document {
        val documentBuilderFactory = DocumentBuilderFactory.newInstance()
        documentBuilderFactory.isNamespaceAware = true
        documentBuilderFactory.setFeature(XMLConstants.FEATURE_SECURE_PROCESSING, true)
        documentBuilderFactory.setAttribute(XMLConstants.ACCESS_EXTERNAL_DTD, "")
        documentBuilderFactory.setAttribute(XMLConstants.ACCESS_EXTERNAL_SCHEMA, "")

        return documentBuilderFactory.newDocumentBuilder().parse(inputStream)
    }

    private data class TimingScope(
        val begin: Duration,
        val end: Duration?,
        val timeContainer: String?,
        val regionId: String?,
        val cueStyle: CueStyle,
    )

    private companion object {
        val BR_TEXT: Pattern = Pattern.compile("(?i)<br\\s*/?>")
    }
}
