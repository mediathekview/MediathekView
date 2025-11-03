/*
 * Copyright (c) 2025 derreisende77.
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

import java.util.*

object BandwidthFormatter {
    private const val KB = 1_000L
    private const val MB = 1_000_000L
    private const val GB = 1_000_000_000L

    @JvmStatic
    fun format(bytesPerSecond: Long): String = when {
        bytesPerSecond <= 1 -> ""
        bytesPerSecond >= GB -> String.format(Locale.GERMANY, "%.1f GB/s", bytesPerSecond.toDouble() / GB)
        bytesPerSecond >= MB -> String.format(Locale.GERMANY, "%.1f MB/s", bytesPerSecond.toDouble() / MB)
        bytesPerSecond >= KB -> String.format(Locale.GERMANY, "%,d KB/s", bytesPerSecond / KB)
        else -> String.format(Locale.GERMANY, "%,d B/s", bytesPerSecond)
    }
}
