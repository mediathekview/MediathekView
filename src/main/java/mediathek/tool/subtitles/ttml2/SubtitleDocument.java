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

import java.time.Duration;
import java.util.List;
import java.util.Map;
import java.util.Objects;

/**
 * Intermediate subtitle model with region layout and per-run styling.
 */
public record SubtitleDocument(Map<String, Region> regions, List<Cue> cues) {

    public SubtitleDocument {
        regions = Map.copyOf(Objects.requireNonNull(regions, "regions"));
        cues = List.copyOf(Objects.requireNonNull(cues, "cues"));
    }

    public record Cue(Duration start, Duration end, String regionId, List<StyledRun> runs, CueStyle cueStyle) {
        public Cue {
            Objects.requireNonNull(start, "start");
            Objects.requireNonNull(end, "end");
            regionId = (regionId == null || regionId.isBlank()) ? null : regionId;
            runs = List.copyOf(Objects.requireNonNull(runs, "runs"));
            cueStyle = (cueStyle == null) ? CueStyle.EMPTY : cueStyle;
            if (end.compareTo(start) < 0) throw new IllegalArgumentException("Cue end < start: " + start + " .. " + end);
        }

        public String plainText() {
            var sb = new StringBuilder();
            for (var r : runs) sb.append(r.text());
            return sb.toString();
        }
    }

    public record StyledRun(String text, TextStyle style) {
        public StyledRun {
            Objects.requireNonNull(text, "text");
            style = (style == null) ? TextStyle.EMPTY : style;
        }
    }

    /**
     * Region rectangle (origin/extent) plus align hints.
     * origin/extent may be null to indicate defaults.
     */
    public record Region(String id, Length2 origin, Length2 extent, String displayAlign, String textAlign) {
        public Region {
            Objects.requireNonNull(id, "id");
        }
    }

    /**
     * Cue-level layout hints (may be inherited from region).
     */
    public record CueStyle(String displayAlign, String textAlign) {
        public static final CueStyle EMPTY = new CueStyle(null, null);

        public CueStyle merge(CueStyle child) {
            if (child == null) return this;
            return new CueStyle(
                    child.displayAlign != null ? child.displayAlign : this.displayAlign,
                    child.textAlign != null ? child.textAlign : this.textAlign
            );
        }
    }

    /**
     * Inline text styling; extend as needed (font family/size, outlines, etc).
     * color/backgroundColor are nullable and override via last-specified-wins in StyleIndex.
     */
    public record TextStyle(boolean bold, boolean italic, boolean underline, Rgba color, Rgba backgroundColor) {
        public static final TextStyle EMPTY = new TextStyle(false, false, false, null, null);

        public TextStyle merge(TextStyle child) {
            if (child == null) return this;
            return new TextStyle(
                    this.bold || child.bold,
                    this.italic || child.italic,
                    this.underline || child.underline,
                    child.color != null ? child.color : this.color,
                    child.backgroundColor != null ? child.backgroundColor : this.backgroundColor
            );
        }
    }

    public sealed interface Length permits Px, Percent {
        double resolve(double reference);
    }

    public record Px(double value) implements Length {
        @Override public double resolve(double reference) { return value; }
    }

    public record Percent(double value) implements Length {
        @Override public double resolve(double reference) { return reference * (value / 100.0); }
    }

    public record Length2(Length x, Length y) {}
}
