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
package mediathek.gui.dialog.lucene_tutorial;

import org.jetbrains.annotations.NotNull;
import org.jsoup.Jsoup;
import org.jsoup.nodes.Element;

import javax.swing.*;
import javax.swing.text.html.HTMLDocument;
import javax.swing.text.html.HTMLEditorKit;
import javax.swing.text.html.StyleSheet;
import java.awt.*;
import java.io.Reader;
import java.io.StringReader;

final class FlexmarkHtmlPane extends JEditorPane {
    private final HTMLEditorKit editorKit;

    FlexmarkHtmlPane() {
        editorKit = createEditorKit();
        setEditable(false);
        setContentType("text/html");
        putClientProperty(JEditorPane.HONOR_DISPLAY_PROPERTIES, Boolean.TRUE);
        setEditorKit(editorKit);
        setBackground(resolveColor("TextPane.background", "Panel.background", Color.WHITE));
        setForeground(resolveColor("TextPane.foreground", "Label.foreground", Color.BLACK));
    }

    void setHtml(@NotNull String html) {
        var document = createDocument();
        setDocument(document);

        try (Reader reader = new StringReader(normalizeHtmlForSwing(html))) {
            editorKit.read(reader, document, 0);
            setCaretPosition(0);
        } catch (Exception ex) {
            throw new IllegalArgumentException("Failed to parse rendered HTML", ex);
        }
    }

    private @NotNull HTMLEditorKit createEditorKit() {
        var background = resolveColor("TextPane.background", "Panel.background", Color.WHITE);
        var foreground = resolveColor("TextPane.foreground", "Label.foreground", Color.BLACK);
        var border = resolveColor("Component.borderColor", "Separator.foreground", new Color(0xD9D9D9));
        var headerBackground = blend(background, foreground, 0.06f);
        var codeBackground = blend(background, foreground, 0.04f);
        var preBackground = blend(background, foreground, 0.03f);
        var link = resolveColor("Component.linkColor", "Hyperlink.linkColor", new Color(0x0B57D0));

        var styleSheet = new StyleSheet();
        styleSheet.addRule("""
                body {
                  font-family: Arial, Helvetica, sans-serif;
                  font-size: 12pt;
                  margin: 14px;
                  line-height: 1.45;
                  color: %s;
                  background-color: %s;
                }
                """.formatted(toCssColor(foreground), toCssColor(background)));
        styleSheet.addRule("h1 { font-size: 28pt; font-weight: 700; margin-top: 8px; margin-bottom: 14px; }");
        styleSheet.addRule("h2 { font-size: 20pt; font-weight: 700; margin-top: 18px; margin-bottom: 10px; }");
        styleSheet.addRule("h3 { font-size: 15pt; font-weight: 700; margin-top: 16px; margin-bottom: 8px; }");
        styleSheet.addRule("p { margin-top: 8px; margin-bottom: 14px; }");
        styleSheet.addRule("ul, ol { margin-left: 24px; margin-top: 8px; margin-bottom: 14px; }");
        styleSheet.addRule("li { margin-bottom: 6px; }");
        styleSheet.addRule("table { margin-top: 10px; margin-bottom: 18px; }");
        styleSheet.addRule("th { font-weight: 700; background-color: %s; text-align: center; }".formatted(toCssColor(headerBackground)));
        styleSheet.addRule("td { text-align: left; vertical-align: top; }");
        styleSheet.addRule("th, td { border: 1px solid %s; padding: 12px; }".formatted(toCssColor(border)));
        styleSheet.addRule("code, pre { font-family: Monospaced; }");
        styleSheet.addRule("pre { background-color: %s; border: 1px solid %s; padding: 10px; margin-top: 10px; margin-bottom: 16px; }"
                .formatted(toCssColor(preBackground), toCssColor(border)));
        styleSheet.addRule(".inline-code { font-family: Monospaced; background-color: %s; border: 1px solid %s; padding: 2px 6px; }"
                .formatted(toCssColor(codeBackground), toCssColor(border)));
        styleSheet.addRule("a { color: %s; text-decoration: underline; }".formatted(toCssColor(link)));

        var editorKit = new HTMLEditorKit();
        editorKit.setStyleSheet(styleSheet);
        return editorKit;
    }

    private @NotNull javax.swing.text.Document createDocument() {
        var document = (HTMLDocument) editorKit.createDefaultDocument();
        document.putProperty("IgnoreCharsetDirective", Boolean.TRUE);
        document.setAsynchronousLoadPriority(-1);
        document.setTokenThreshold(Integer.MAX_VALUE);
        return document;
    }

    private @NotNull String normalizeHtmlForSwing(@NotNull String html) {
        var doc = Jsoup.parse(html);
        doc.outputSettings().syntax(org.jsoup.nodes.Document.OutputSettings.Syntax.xml);

        for (Element table : doc.select("table")) {
            table.attr("width", "100%");
            table.attr("cellspacing", "0");
            table.attr("cellpadding", "12");
            table.attr("border", "1");
        }

        for (Element heading : doc.select("h1, h2, h3")) {
            heading.attr("align", "left");
        }

        for (Element th : doc.select("th")) {
            th.removeAttr("bgcolor");
        }

        for (Element code : doc.select("p code, li code, td code, th code")) {
            code.tagName("span");
            code.addClass("inline-code");
        }

        return doc.outerHtml();
    }

    private @NotNull Color resolveColor(@NotNull String primaryKey, @NotNull String fallbackKey, @NotNull Color defaultColor) {
        var color = UIManager.getColor(primaryKey);
        if (color != null) {
            return color;
        }
        color = UIManager.getColor(fallbackKey);
        return color != null ? color : defaultColor;
    }

    private @NotNull String toCssColor(@NotNull Color color) {
        return "#%02x%02x%02x".formatted(color.getRed(), color.getGreen(), color.getBlue());
    }

    private @NotNull Color blend(@NotNull Color base, @NotNull Color overlay, float ratio) {
        ratio = Math.max(0f, Math.min(1f, ratio));
        var inverse = 1f - ratio;
        return new Color(
                Math.round(base.getRed() * inverse + overlay.getRed() * ratio),
                Math.round(base.getGreen() * inverse + overlay.getGreen() * ratio),
                Math.round(base.getBlue() * inverse + overlay.getBlue() * ratio)
        );
    }
}
