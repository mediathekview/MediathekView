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

import org.w3c.dom.Element;

import java.math.BigDecimal;
import java.math.RoundingMode;
import java.time.Duration;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * TTML2 time parsing (clock-time and offset-time).
 * Focused on subtitle conversion.
 */
public final class TtmlTime {

    private static final String NS_TTP = "http://www.w3.org/ns/ttml#parameter";
    private static final Pattern CLOCK_TIME = Pattern.compile(
            "^(?<h>\\d{2,}):(?<m>\\d{2}):(?<s>\\d{2})(?:(?<frac>\\.\\d+)|:(?<f>\\d{2,})(?:\\.(?<sf>\\d+))?)?$"
    );
    private static final Pattern OFFSET_TIME = Pattern.compile(
            "^(?<count>\\d+)(?<frac>\\.\\d+)?(?<metric>h|m|s|ms|f|t)$"
    );

    private TtmlTime() {
    }

    public static TimeContext readTimeContext(Element tt) {
        Integer frameRate = parseInt(XmlUtil.attr(tt, NS_TTP, "frameRate"));
        Integer subFrameRate = parseInt(XmlUtil.attr(tt, NS_TTP, "subFrameRate"));
        Integer tickRate = parseInt(XmlUtil.attr(tt, NS_TTP, "tickRate"));

        String frm = XmlUtil.attr(tt, NS_TTP, "frameRateMultiplier");
        BigDecimal num = BigDecimal.ONE, den = BigDecimal.ONE;
        if (frm != null) {
            String[] parts = frm.trim().split("\\s+");
            if (parts.length == 2) {
                num = new BigDecimal(parts[0]);
                den = new BigDecimal(parts[1]);
            }
        }
        String timeBase = XmlUtil.attr(tt, NS_TTP, "timeBase");

        int fr = frameRate == null ? 30 : frameRate;
        int sfr = subFrameRate == null ? 1 : subFrameRate;

        int tr;
        if (tickRate != null && tickRate > 0) {
            tr = tickRate;
        }
        else {
            BigDecimal eff = BigDecimal.valueOf(fr).multiply(num).divide(den, 12, RoundingMode.HALF_UP);
            tr = eff.multiply(BigDecimal.valueOf(sfr)).setScale(0, RoundingMode.HALF_UP).intValue();
            if (tr <= 0)
                tr = 1;
        }

        return new TimeContext(fr, sfr, num, den, tr, timeBase);
    }

    public static Duration parseTimeExpression(String raw, TimeContext ctx) {
        if (raw == null)
            return null;
        String s = raw.trim();
        if (s.isEmpty())
            return null;
        if ("indefinite".equals(s))
            return null;

        if (s.startsWith("wallclock(")) {
            throw new IllegalArgumentException("wallclock() time expressions are not supported for subtitle export: " + raw);
        }

        Matcher mc = CLOCK_TIME.matcher(s);
        if (mc.matches()) {
            long h = Long.parseLong(mc.group("h"));
            long m = Long.parseLong(mc.group("m"));
            long sec = Long.parseLong(mc.group("s"));

            BigDecimal totalSeconds = BigDecimal.valueOf(h * 3600L + m * 60L + sec);

            String frac = mc.group("frac");
            if (frac != null) {
                totalSeconds = totalSeconds.add(new BigDecimal(frac));
                return secondsToDuration(totalSeconds);
            }

            String frames = mc.group("f");
            if (frames != null) {
                BigDecimal eff = ctx.effectiveFrameRate();
                BigDecimal framePart = BigDecimal.valueOf(Long.parseLong(frames)).divide(eff, 12, RoundingMode.HALF_UP);
                String subFrames = mc.group("sf");
                if (subFrames != null) {
                    BigDecimal sf = BigDecimal.valueOf(Long.parseLong(subFrames));
                    BigDecimal subPart = sf
                            .divide(BigDecimal.valueOf(ctx.subFrameRate()), 12, RoundingMode.HALF_UP)
                            .divide(eff, 12, RoundingMode.HALF_UP);
                    framePart = framePart.add(subPart);
                }
                return secondsToDuration(totalSeconds.add(framePart));
            }

            return secondsToDuration(totalSeconds);
        }

        Matcher mo = OFFSET_TIME.matcher(s);
        if (mo.matches()) {
            BigDecimal count = new BigDecimal(mo.group("count"));
            String frac = mo.group("frac");
            if (frac != null)
                count = count.add(new BigDecimal(frac));
            String metric = mo.group("metric");

            return switch (metric) {
                case "h" -> secondsToDuration(count.multiply(BigDecimal.valueOf(3600)));
                case "m" -> secondsToDuration(count.multiply(BigDecimal.valueOf(60)));
                case "s" -> secondsToDuration(count);
                case "ms" -> Duration.ofMillis(count.setScale(0, RoundingMode.HALF_UP).longValue());
                case "f" -> {
                    BigDecimal eff = ctx.effectiveFrameRate();
                    yield secondsToDuration(count.divide(eff, 12, RoundingMode.HALF_UP));
                }
                case "t" ->
                        secondsToDuration(count.divide(BigDecimal.valueOf(ctx.tickRate()), 12, RoundingMode.HALF_UP));
                default -> throw new IllegalArgumentException("Unknown time metric: " + metric);
            };
        }

        throw new IllegalArgumentException("Unsupported TTML time expression: " + raw);
    }

    private static Duration secondsToDuration(BigDecimal seconds) {
        BigDecimal nanos = seconds.multiply(BigDecimal.valueOf(1_000_000_000L));
        long n = nanos.setScale(0, RoundingMode.HALF_UP).longValueExact();
        return Duration.ofNanos(n);
    }

    private static Integer parseInt(String s) {
        if (s == null)
            return null;
        return Integer.parseInt(s.trim());
    }

    public record TimeContext(
            int frameRate,
            int subFrameRate,
            BigDecimal frameRateMultNum,
            BigDecimal frameRateMultDen,
            int tickRate,
            String timeBase
    ) {
        public TimeContext {
            if (frameRate <= 0)
                frameRate = 30;
            if (subFrameRate <= 0)
                subFrameRate = 1;
            if (frameRateMultNum == null)
                frameRateMultNum = BigDecimal.ONE;
            if (frameRateMultDen == null || frameRateMultDen.signum() == 0)
                frameRateMultDen = BigDecimal.ONE;
            if (tickRate <= 0)
                tickRate = 1;
            if (timeBase == null || timeBase.isBlank())
                timeBase = "media";
        }

        public BigDecimal effectiveFrameRate() {
            return BigDecimal.valueOf(frameRate)
                    .multiply(frameRateMultNum)
                    .divide(frameRateMultDen, 12, RoundingMode.HALF_UP);
        }
    }
}
