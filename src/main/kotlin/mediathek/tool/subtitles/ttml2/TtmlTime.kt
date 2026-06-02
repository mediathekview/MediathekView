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

import org.w3c.dom.Element
import java.math.BigDecimal
import java.math.RoundingMode
import java.time.Duration
import java.util.regex.Pattern

/**
 * TTML2 time parsing (clock-time and offset-time).
 * Focused on subtitle conversion.
 */
object TtmlTime {
    private const val NS_TTP = "http://www.w3.org/ns/ttml#parameter"
    private val CLOCK_TIME = Pattern.compile(
        "^(?<h>\\d{2,}):(?<m>\\d{2}):(?<s>\\d{2})(?:(?<frac>\\.\\d+)|:(?<f>\\d{2,})(?:\\.(?<sf>\\d+))?)?$"
    )
    private val OFFSET_TIME = Pattern.compile("^(?<count>\\d+)(?<frac>\\.\\d+)?(?<metric>h|m|s|ms|f|t)$")

    fun readTimeContext(tt: Element): TimeContext {
        val frameRate = parseInt(XmlUtil.attr(tt, NS_TTP, "frameRate"))
        val subFrameRate = parseInt(XmlUtil.attr(tt, NS_TTP, "subFrameRate"))
        val tickRate = parseInt(XmlUtil.attr(tt, NS_TTP, "tickRate"))

        var multiplierNumerator = BigDecimal.ONE
        var multiplierDenominator = BigDecimal.ONE
        val frameRateMultiplier = XmlUtil.attr(tt, NS_TTP, "frameRateMultiplier")
        if (frameRateMultiplier != null) {
            val parts = frameRateMultiplier.trim().split(Regex("\\s+"))
            if (parts.size == 2) {
                multiplierNumerator = parts[0].toBigDecimal()
                multiplierDenominator = parts[1].toBigDecimal()
            }
        }
        val timeBase = XmlUtil.attr(tt, NS_TTP, "timeBase")

        val effectiveFrameRate = frameRate ?: 30
        val effectiveSubFrameRate = subFrameRate ?: 1

        val effectiveTickRate =
            if (tickRate != null && tickRate > 0) {
                tickRate
            } else {
                val effectiveRate = effectiveFrameRate.toBigDecimal()
                    .multiply(multiplierNumerator)
                    .divide(multiplierDenominator, 12, RoundingMode.HALF_UP)
                effectiveRate.multiply(effectiveSubFrameRate.toBigDecimal())
                    .setScale(0, RoundingMode.HALF_UP)
                    .toInt()
                    .takeIf { it > 0 } ?: 1
            }

        return TimeContext(
            effectiveFrameRate.takeIf { it > 0 } ?: 30,
            effectiveSubFrameRate.takeIf { it > 0 } ?: 1,
            multiplierNumerator,
            multiplierDenominator.takeIf { it.signum() != 0 } ?: BigDecimal.ONE,
            effectiveTickRate,
            timeBase?.takeIf { it.isNotBlank() } ?: "media",
        )
    }

    fun parseTimeExpression(raw: String?, ctx: TimeContext): Duration? {
        val expression = raw?.trim()?.takeIf { it.isNotEmpty() && it != "indefinite" } ?: return null

        if (expression.startsWith("wallclock(")) {
            throw IllegalArgumentException("wallclock() time expressions are not supported for subtitle export: $raw")
        }

        CLOCK_TIME.matcher(expression).takeIf { matcher -> matcher.matches() }?.let { matcher ->
            return parseClockTime(matcher, ctx)
        }

        OFFSET_TIME.matcher(expression).takeIf { matcher -> matcher.matches() }?.let { matcher ->
            return parseOffsetTime(matcher, ctx)
        }

        throw IllegalArgumentException("Unsupported TTML time expression: $raw")
    }

    private fun parseClockTime(matcher: java.util.regex.Matcher, ctx: TimeContext): Duration {
        val hours = matcher.group("h").toLong()
        val minutes = matcher.group("m").toLong()
        val seconds = matcher.group("s").toLong()

        var totalSeconds = (hours * 3600L + minutes * 60L + seconds).toBigDecimal()

        val fraction = matcher.group("frac")
        if (fraction != null) {
            totalSeconds = totalSeconds.add(fraction.toBigDecimal())
            return secondsToDuration(totalSeconds)
        }

        val frames = matcher.group("f")
        if (frames != null) {
            val effectiveFrameRate = ctx.effectiveFrameRate()
            var framePart = frames.toLong().toBigDecimal().divide(effectiveFrameRate, 12, RoundingMode.HALF_UP)
            val subFrames = matcher.group("sf")
            if (subFrames != null) {
                val subFramePart = subFrames.toLong().toBigDecimal()
                    .divide(ctx.subFrameRate.toBigDecimal(), 12, RoundingMode.HALF_UP)
                    .divide(effectiveFrameRate, 12, RoundingMode.HALF_UP)
                framePart = framePart.add(subFramePart)
            }
            return secondsToDuration(totalSeconds.add(framePart))
        }

        return secondsToDuration(totalSeconds)
    }

    private fun parseOffsetTime(matcher: java.util.regex.Matcher, ctx: TimeContext): Duration {
        var count = matcher.group("count").toBigDecimal()
        val fraction = matcher.group("frac")
        if (fraction != null) {
            count = count.add(fraction.toBigDecimal())
        }

        return when (val metric = matcher.group("metric")) {
            "h" -> secondsToDuration(count.multiply(3600.toBigDecimal()))
            "m" -> secondsToDuration(count.multiply(60.toBigDecimal()))
            "s" -> secondsToDuration(count)
            "ms" -> Duration.ofMillis(count.setScale(0, RoundingMode.HALF_UP).toLong())
            "f" -> secondsToDuration(count.divide(ctx.effectiveFrameRate(), 12, RoundingMode.HALF_UP))
            "t" -> secondsToDuration(count.divide(ctx.tickRate.toBigDecimal(), 12, RoundingMode.HALF_UP))
            else -> throw IllegalArgumentException("Unknown time metric: $metric")
        }
    }

    private fun secondsToDuration(seconds: BigDecimal): Duration {
        val nanos = seconds.multiply(1_000_000_000L.toBigDecimal())
        return Duration.ofNanos(nanos.setScale(0, RoundingMode.HALF_UP).longValueExact())
    }

    private fun parseInt(value: String?): Int? = value?.trim()?.toInt()

    data class TimeContext(
        val frameRate: Int,
        val subFrameRate: Int,
        val frameRateMultNum: BigDecimal,
        val frameRateMultDen: BigDecimal,
        val tickRate: Int,
        val timeBase: String,
    ) {
        fun effectiveFrameRate(): BigDecimal =
            frameRate.toBigDecimal()
                .multiply(frameRateMultNum)
                .divide(frameRateMultDen, 12, RoundingMode.HALF_UP)
    }
}
