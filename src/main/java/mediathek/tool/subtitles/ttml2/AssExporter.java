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

import mediathek.config.Konstanten;
import mediathek.tool.subtitles.ttml2.SubtitleDocument.Cue;
import mediathek.tool.subtitles.ttml2.SubtitleDocument.Region;
import mediathek.tool.subtitles.ttml2.SubtitleDocument.StyledRun;
import mediathek.tool.subtitles.ttml2.SubtitleDocument.TextStyle;

import java.time.Duration;
import java.util.Objects;

/**
 * Advanced SubStation Alpha (ASS) exporter with:
 * - mixed-span styling (\\b, \\i, \\u, \\1c/\\1a for color+alpha)
 * - region-based placement via \\pos(x,y) and \\an
 * Background color has no perfect ASS equivalent; optional approximation via outline.
 */
public final class AssExporter {

    public record Options(int playResX, int playResY, boolean approximateBackground) {
        public Options {
            if (playResX <= 0 || playResY <= 0)
                throw new IllegalArgumentException("playRes must be > 0");
        }

        public static Options defaults() {
            return new Options(384, 288, false);
        }
    }

    private final Options opt;

    public AssExporter(Options opt) {
        this.opt = (opt == null) ? Options.defaults() : opt;
    }

    public String export(SubtitleDocument doc) {
        StringBuilder sb = new StringBuilder();
        sb.append(header(opt.playResX, opt.playResY));
        for (Cue cue : doc.cues())
            sb.append(dialogue(doc, cue)).append('\n');
        return sb.toString();
    }

    private String dialogue(SubtitleDocument doc, Cue cue) {
        String start = fmtAss(cue.start());
        String end = fmtAss(cue.end());

        Region region = cue.regionId() != null ? doc.regions().get(cue.regionId()) : null;
        Rect rect = (region != null) ? resolveRect(region) : new Rect(0, 0, opt.playResX, opt.playResY);

        String textAlign = firstNonNull(cue.cueStyle().textAlign(), region != null ? region.textAlign() : null);
        String displayAlign = firstNonNull(cue.cueStyle().displayAlign(), region != null ? region.displayAlign() : null);
        Anchor a = anchor(rect, textAlign, displayAlign);

        StringBuilder text = new StringBuilder();
        text.append("{\\pos(").append(a.x).append(',').append(a.y).append(")\\an").append(a.an).append('}');

        TextStyle prev = TextStyle.EMPTY;
        for (StyledRun run : cue.runs()) {
            TextStyle cur = run.style();
            if (!cur.equals(prev)) {
                text.append("{").append(styleDelta(prev, cur)).append("}");
                prev = cur;
            }
            text.append(escapeAss(run.text()).replace("\\n", "\\N"));
        }

        return "Dialogue: 0," + start + "," + end + ",Default,,0,0,0,," + text;
    }

    private String styleDelta(TextStyle prev, TextStyle cur) {
        StringBuilder sb = new StringBuilder();

        if (prev.bold() != cur.bold())
            sb.append(cur.bold() ? "\\b1" : "\\b0");
        if (prev.italic() != cur.italic())
            sb.append(cur.italic() ? "\\i1" : "\\i0");
        if (prev.underline() != cur.underline())
            sb.append(cur.underline() ? "\\u1" : "\\u0");

        if (!Objects.equals(prev.color(), cur.color())) {
            if (cur.color() != null) {
                sb.append(String.format("\\1c&H%02X%02X%02X&", cur.color().b(), cur.color().g(), cur.color().r()));
                sb.append(String.format("\\1a&H%02X&", 255 - cur.color().a()));
            }
            else {
                sb.append("\\1a&H00&");
            }
        }

        if (opt.approximateBackground && !Objects.equals(prev.backgroundColor(), cur.backgroundColor())) {
            if (cur.backgroundColor() != null) {
                sb.append(String.format("\\3c&H%02X%02X%02X&", cur.backgroundColor().b(), cur.backgroundColor().g(), cur.backgroundColor().r()));
                sb.append(String.format("\\3a&H%02X&", 255 - cur.backgroundColor().a()));
                sb.append("\\bord3\\shad0");
            }
            else {
                sb.append("\\bord2");
            }
        }

        return sb.toString();
    }

    private Rect resolveRect(Region r) {
        int x = 0, y = 0, w = opt.playResX, h = opt.playResY;
        if (r.origin() != null) {
            x = (int) Math.round(r.origin().x().resolve(opt.playResX));
            y = (int) Math.round(r.origin().y().resolve(opt.playResY));
        }
        if (r.extent() != null) {
            w = (int) Math.round(r.extent().x().resolve(opt.playResX));
            h = (int) Math.round(r.extent().y().resolve(opt.playResY));
        }
        return new Rect(x, y, w, h);
    }

    private static Anchor anchor(Rect rect, String textAlign, String displayAlign) {
        int x;
        int col;
        String ta = (textAlign == null) ? "center" : textAlign.toLowerCase();
        if (ta.contains("end") || ta.contains("right")) {
            x = rect.x + rect.w;
            col = 3;
        }
        else if (ta.contains("start") || ta.contains("left")) {
            x = rect.x;
            col = 1;
        }
        else {
            x = rect.x + rect.w / 2;
            col = 2;
        }

        int y;
        int rowBase;
        String da = (displayAlign == null) ? "after" : displayAlign.toLowerCase();
        if (da.contains("before") || da.contains("top")) {
            y = rect.y;
            rowBase = 6;
        }
        else if (da.contains("center") || da.contains("middle")) {
            y = rect.y + rect.h / 2;
            rowBase = 3;
        }
        else {
            y = rect.y + rect.h;
            rowBase = 0;
        }

        int an = rowBase + col;
        return new Anchor(x, y, an);
    }

    private static String header(int x, int y) {
        return ("[Script Info]\n" +
                "; Script generated by MediathekView " + Konstanten.MVVERSION + "\n" +
                "ScriptType: v4.00+\n" +
                "Collisions: Normal\n" +
                "PlayResX: " + x + "\n" +
                "PlayResY: " + y + "\n" +
                "WrapStyle: 0\n" +
                "ScaledBorderAndShadow: yes\n\n" +
                "[V4+ Styles]\n" +
                "Format: Name, Fontname, Fontsize, PrimaryColour, SecondaryColour, OutlineColour, BackColour, Bold, Italic, Underline, StrikeOut, ScaleX, ScaleY, Spacing, Angle, BorderStyle, Outline, Shadow, Alignment, MarginL, MarginR, MarginV, Encoding\n" +
                "Style: Default,Arial,24,&H00FFFFFF,&H000000FF,&H00000000,&H64000000,0,0,0,0,100,100,0,0,1,2,1,2,20,20,20,1\n\n" +
                "[Events]\n" +
                "Format: Layer, Start, End, Style, Name, MarginL, MarginR, MarginV, Effect, Text\n");
    }

    private static String fmtAss(Duration d) {
        long ms = Math.max(0, d.toMillis());
        long h = ms / 3_600_000;
        ms %= 3_600_000;
        long m = ms / 60_000;
        ms %= 60_000;
        long s = ms / 1_000;
        long cs = (ms % 1_000) / 10;
        return String.format("%d:%02d:%02d.%02d", h, m, s, cs);
    }

    private static String escapeAss(String s) {
        return s.replace("{", "｛").replace("}", "｝");
    }

    private static String firstNonNull(String a, String b) {
        return a != null ? a : b;
    }

    private record Rect(int x, int y, int w, int h) {
    }

    private record Anchor(int x, int y, int an) {
    }
}
