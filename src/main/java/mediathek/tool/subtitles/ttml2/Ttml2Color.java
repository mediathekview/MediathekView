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

import java.util.Locale;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * TTML2 <color> lexical parser.
 * Supports:
 * - #RRGGBB
 * - #RRGGBBAA
 * - rgb(r,g,b) where each component is [0,255]
 * - rgba(r,g,b,a) where each component is [0,255]
 * - named colors (case-insensitive): transparent, black, silver, gray, white, maroon, red,
 * purple, fuchsia, magenta, green, lime, olive, yellow, navy, blue, teal, aqua, cyan.
 */
public final class Ttml2Color {

    private static final Pattern RGB = Pattern.compile("^rgb\\(\\s*(\\d{1,3})\\s*,\\s*(\\d{1,3})\\s*,\\s*(\\d{1,3})\\s*\\)$", Pattern.CASE_INSENSITIVE);
    private static final Pattern RGBA = Pattern.compile("^rgba\\(\\s*(\\d{1,3})\\s*,\\s*(\\d{1,3})\\s*,\\s*(\\d{1,3})\\s*,\\s*(\\d{1,3})\\s*\\)$", Pattern.CASE_INSENSITIVE);

    private static final Map<String, Rgba> NAMED = Map.ofEntries(
            Map.entry("transparent", new Rgba(0, 0, 0, 0)),
            Map.entry("black", new Rgba(0, 0, 0, 255)),
            Map.entry("silver", new Rgba(192, 192, 192, 255)),
            Map.entry("gray", new Rgba(128, 128, 128, 255)),
            Map.entry("white", new Rgba(255, 255, 255, 255)),
            Map.entry("maroon", new Rgba(128, 0, 0, 255)),
            Map.entry("red", new Rgba(255, 0, 0, 255)),
            Map.entry("purple", new Rgba(128, 0, 128, 255)),
            Map.entry("fuchsia", new Rgba(255, 0, 255, 255)),
            Map.entry("magenta", new Rgba(255, 0, 255, 255)),
            Map.entry("green", new Rgba(0, 128, 0, 255)),
            Map.entry("lime", new Rgba(0, 255, 0, 255)),
            Map.entry("olive", new Rgba(128, 128, 0, 255)),
            Map.entry("yellow", new Rgba(255, 255, 0, 255)),
            Map.entry("navy", new Rgba(0, 0, 128, 255)),
            Map.entry("blue", new Rgba(0, 0, 255, 255)),
            Map.entry("teal", new Rgba(0, 128, 128, 255)),
            Map.entry("aqua", new Rgba(0, 255, 255, 255)),
            Map.entry("cyan", new Rgba(0, 255, 255, 255))
    );

    private Ttml2Color() {
    }

    public static Rgba parse(String raw) {
        if (raw == null)
            return null;
        String s = raw.trim();
        if (s.isEmpty())
            return null;

        // Hex #RRGGBB or #RRGGBBAA
        if (s.startsWith("#")) {
            String hex = s.substring(1);
            if (hex.length() == 6) {
                return new Rgba(
                        Integer.parseInt(hex.substring(0, 2), 16),
                        Integer.parseInt(hex.substring(2, 4), 16),
                        Integer.parseInt(hex.substring(4, 6), 16),
                        255
                );
            }
            if (hex.length() == 8) {
                return new Rgba(
                        Integer.parseInt(hex.substring(0, 2), 16),
                        Integer.parseInt(hex.substring(2, 4), 16),
                        Integer.parseInt(hex.substring(4, 6), 16),
                        Integer.parseInt(hex.substring(6, 8), 16)
                );
            }
            throw new IllegalArgumentException("Invalid TTML2 color hex: " + raw);
        }

        Matcher m = RGB.matcher(s);
        if (m.matches()) {
            return new Rgba(comp(m.group(1), raw), comp(m.group(2), raw), comp(m.group(3), raw), 255);
        }
        m = RGBA.matcher(s);
        if (m.matches()) {
            return new Rgba(comp(m.group(1), raw), comp(m.group(2), raw), comp(m.group(3), raw), comp(m.group(4), raw));
        }

        Rgba named = NAMED.get(s.toLowerCase(Locale.ROOT));
        if (named != null)
            return named;

        throw new IllegalArgumentException("Invalid TTML2 <color>: " + raw);
    }

    private static int comp(String dec, String raw) {
        int v;
        try {
            v = Integer.parseInt(dec);
        }
        catch (NumberFormatException e) {
            throw new IllegalArgumentException("Invalid TTML2 color component in: " + raw, e);
        }
        if (v < 0 || v > 255)
            throw new IllegalArgumentException("TTML2 color component out of range [0,255]: " + raw);
        return v;
    }
}
