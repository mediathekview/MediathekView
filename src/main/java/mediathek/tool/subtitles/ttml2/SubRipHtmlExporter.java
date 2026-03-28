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

import mediathek.tool.subtitles.ttml2.SubtitleDocument.Cue;
import mediathek.tool.subtitles.ttml2.SubtitleDocument.StyledRun;
import mediathek.tool.subtitles.ttml2.SubtitleDocument.TextStyle;

import java.time.Duration;

/**
 * SubRip exporter with HTML-like style tags:
 * - <b>, <i>, <u>
 * - <font color="#RRGGBB"> (alpha ignored; SRT has no standard alpha support)
 */
public final class SubRipHtmlExporter {

    private static String openTags(TextStyle s) {
        if (s == null)
            return "";
        StringBuilder sb = new StringBuilder();
        if (s.bold())
            sb.append("<b>");
        if (s.italic())
            sb.append("<i>");
        if (s.underline())
            sb.append("<u>");
        if (s.color() != null && !s.color().isTransparent()) {
            sb.append("<font color=\"").append(String.format("#%02X%02X%02X", s.color().r(), s.color().g(), s.color().b())).append("\">");
        }
        return sb.toString();
    }

    private static String closeTags(TextStyle s) {
        if (s == null)
            return "";
        StringBuilder sb = new StringBuilder();
        if (s.color() != null && !s.color().isTransparent())
            sb.append("</font>");
        if (s.underline())
            sb.append("</u>");
        if (s.italic())
            sb.append("</i>");
        if (s.bold())
            sb.append("</b>");
        return sb.toString();
    }

    private static String escapeHtml(String s) {
        return s.replace("&", "&amp;").replace("<", "&lt;").replace(">", "&gt;");
    }

    private static String fmt(Duration d) {
        long ms = Math.max(0, d.toMillis());
        long h = ms / 3_600_000;
        ms %= 3_600_000;
        long m = ms / 60_000;
        ms %= 60_000;
        long s = ms / 1_000;
        long r = ms % 1_000;
        return String.format("%02d:%02d:%02d,%03d", h, m, s, r);
    }

    public String export(SubtitleDocument doc) {
        StringBuilder sb = new StringBuilder();
        int idx = 1;

        for (Cue cue : doc.cues()) {
            sb.append(idx++).append('\n');
            sb.append(fmt(cue.start())).append(" --> ").append(fmt(cue.end())).append('\n');

            TextStyle prev = null;
            for (StyledRun run : cue.runs()) {
                TextStyle cur = run.style();

                if (prev != null && !prev.equals(cur))
                    sb.append(closeTags(prev));
                if (prev == null || !prev.equals(cur))
                    sb.append(openTags(cur));

                sb.append(escapeHtml(run.text()).replace("\\n", "\n"));
                prev = cur;
            }
            if (prev != null)
                sb.append(closeTags(prev));

            sb.append("\n\n");
        }
        return sb.toString();
    }
}
