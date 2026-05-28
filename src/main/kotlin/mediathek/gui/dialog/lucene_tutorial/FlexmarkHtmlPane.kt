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
package mediathek.gui.dialog.lucene_tutorial

import org.jsoup.Jsoup
import java.awt.Color
import java.io.StringReader
import javax.swing.JEditorPane
import javax.swing.UIManager
import javax.swing.text.Document
import javax.swing.text.html.HTMLDocument
import javax.swing.text.html.HTMLEditorKit
import javax.swing.text.html.StyleSheet
import kotlin.math.roundToInt

internal class FlexmarkHtmlPane : JEditorPane() {
    private val editorKit: HTMLEditorKit = createEditorKit()

    init {
        isEditable = false
        contentType = "text/html"
        putClientProperty(HONOR_DISPLAY_PROPERTIES, true)
        setEditorKit(editorKit)
        background = resolveColor("TextPane.background", "Panel.background", Color.WHITE)
        foreground = resolveColor("TextPane.foreground", "Label.foreground", Color.BLACK)
    }

    fun setHtml(html: String) {
        val document = createDocument()
        setDocument(document)

        try {
            StringReader(normalizeHtmlForSwing(html)).use { reader ->
                editorKit.read(reader, document, 0)
            }
            caretPosition = 0
        } catch (ex: Exception) {
            throw IllegalArgumentException("Failed to parse rendered HTML", ex)
        }
    }

    private fun createEditorKit(): HTMLEditorKit {
        val background = resolveColor("TextPane.background", "Panel.background", Color.WHITE)
        val foreground = resolveColor("TextPane.foreground", "Label.foreground", Color.BLACK)
        val border = resolveColor("Component.borderColor", "Separator.foreground", Color(0xD9D9D9))
        val headerBackground = blend(background, foreground, 0.06f)
        val codeBackground = blend(background, foreground, 0.04f)
        val preBackground = blend(background, foreground, 0.03f)
        val link = resolveColor("Component.linkColor", "Hyperlink.linkColor", Color(0x0B57D0))

        val styleSheet = StyleSheet().apply {
            addRule(
                """
                body {
                  font-family: Arial, Helvetica, sans-serif;
                  font-size: 12pt;
                  margin: 14px;
                  line-height: 1.45;
                  color: ${foreground.toCssColor()};
                  background-color: ${background.toCssColor()};
                }
                """.trimIndent()
            )
            addRule("h1 { font-size: 28pt; font-weight: 700; margin-top: 8px; margin-bottom: 14px; }")
            addRule("h2 { font-size: 20pt; font-weight: 700; margin-top: 18px; margin-bottom: 10px; }")
            addRule("h3 { font-size: 15pt; font-weight: 700; margin-top: 16px; margin-bottom: 8px; }")
            addRule("p { margin-top: 8px; margin-bottom: 14px; }")
            addRule("ul, ol { margin-left: 24px; margin-top: 8px; margin-bottom: 14px; }")
            addRule("li { margin-bottom: 6px; }")
            addRule("table { margin-top: 10px; margin-bottom: 18px; }")
            addRule("th { font-weight: 700; background-color: ${headerBackground.toCssColor()}; text-align: center; }")
            addRule("td { text-align: left; vertical-align: top; }")
            addRule("th, td { border: 1px solid ${border.toCssColor()}; padding: 12px; }")
            addRule("code, pre { font-family: Monospaced; }")
            addRule(
                "pre { background-color: ${preBackground.toCssColor()}; border: 1px solid ${border.toCssColor()}; " +
                    "padding: 10px; margin-top: 10px; margin-bottom: 16px; }"
            )
            addRule(
                ".inline-code { font-family: Monospaced; background-color: ${codeBackground.toCssColor()}; " +
                    "border: 1px solid ${border.toCssColor()}; padding: 2px 6px; }"
            )
            addRule("a { color: ${link.toCssColor()}; text-decoration: underline; }")
        }

        return HTMLEditorKit().apply {
            setStyleSheet(styleSheet)
        }
    }

    private fun createDocument(): Document {
        val document = editorKit.createDefaultDocument() as HTMLDocument
        document.putProperty("IgnoreCharsetDirective", true)
        document.asynchronousLoadPriority = -1
        document.tokenThreshold = Int.MAX_VALUE
        return document
    }

    private fun normalizeHtmlForSwing(html: String): String {
        val doc = Jsoup.parse(html)
        doc.outputSettings().syntax(org.jsoup.nodes.Document.OutputSettings.Syntax.xml)

        doc.select("table").forEach { table ->
            table.attr("width", "100%")
            table.attr("cellspacing", "0")
            table.attr("cellpadding", "12")
            table.attr("border", "1")
        }

        doc.select("h1, h2, h3").forEach { heading ->
            heading.attr("align", "left")
        }

        doc.select("th").forEach { headerCell ->
            headerCell.removeAttr("bgcolor")
        }

        doc.select("p code, li code, td code, th code").forEach { code ->
            code.tagName("span")
            code.addClass("inline-code")
        }

        return doc.outerHtml()
    }

    private fun resolveColor(primaryKey: String, fallbackKey: String, defaultColor: Color): Color =
        UIManager.getColor(primaryKey) ?: UIManager.getColor(fallbackKey) ?: defaultColor

    private fun Color.toCssColor(): String = "#%02x%02x%02x".format(red, green, blue)

    private fun blend(base: Color, overlay: Color, ratio: Float): Color {
        val boundedRatio = ratio.coerceIn(0f, 1f)
        val inverse = 1f - boundedRatio
        return Color(
            (base.red * inverse + overlay.red * boundedRatio).roundToInt(),
            (base.green * inverse + overlay.green * boundedRatio).roundToInt(),
            (base.blue * inverse + overlay.blue * boundedRatio).roundToInt(),
        )
    }
}
