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

package mediathek.tool

import kotlin.time.Duration

data class DurationFormatter(val totalMinutes: Long) {
    val days: Long = totalMinutes / MINUTES_PER_DAY
    val hours: Long = (totalMinutes % MINUTES_PER_DAY) / MINUTES_PER_HOUR
    val minutes: Long = totalMinutes % MINUTES_PER_HOUR

    fun toDisplayText(prefix: String = ""): String {
        return when {
            days > 0 -> "${prefix}${days}d ${hours}h ${minutes}m"
            hours > 0 -> "${prefix}${hours}h ${minutes}m"
            else -> "${prefix}${minutes}m"
        }
    }

    companion object {
        private const val MINUTES_PER_HOUR = 60L
        private const val MINUTES_PER_DAY = 24L * MINUTES_PER_HOUR

        fun from(duration: Duration): DurationFormatter {
            return DurationFormatter(duration.inWholeMinutes)
        }

        fun from(duration: String = ""): DurationFormatter {
            return fromOrNull(duration) ?: throw IllegalArgumentException("Invalid duration format: $duration")
        }

        fun fromOrNull(duration: String = ""): DurationFormatter? {
            val parts = duration.split(":")
            if (parts.size != 3) {
                return null
            }

            val hours = parts[0].toLongOrNull() ?: return null
            val minutes = parts[1].toLongOrNull() ?: return null
            return DurationFormatter(hours * MINUTES_PER_HOUR + minutes)
        }
    }
}
