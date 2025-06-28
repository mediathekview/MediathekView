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

package mediathek.gui.bookmark.expiration

import java.time.LocalDate

object SenderExpirationService {
    private val ARD_SENDERS = setOf(
        "ard", "ardalpha", "br", "funknet", "hr", "mdr", "ndr",
        "one", "phoenix", "radiobremntv", "rbb", "sr", "swr",
        "tagesschau24", "wdr"
    )

    @JvmStatic
    fun fetchExpiryDate(sender: String, websiteUrl: String): LocalDate? {
        val normalizedSender = normalizeSender(sender)
        return when {
            "arte" in normalizedSender -> ArteExpiryHelper.getExpiryInfo(websiteUrl).map { it.expiryDate }
                .orElse(null)

            normalizedSender == "3sat" -> ThreeSatExpiryHelper.getExpiryInfo(websiteUrl).map { it.expiryDate }
                .orElse(null)

            normalizedSender in ARD_SENDERS -> ArdMediathekExpiryHelper.getExpiryInfo(websiteUrl)
                .map { it.expiryDate }.orElse(null)

            normalizedSender == "orf" -> OrfExpiryHelper.getExpiryInfo(websiteUrl).map { it.expiryDate }.orElse(null)
            else -> null
        }
    }

    private fun normalizeSender(sender: String) =
        sender.trim().lowercase().replace(Regex("[^a-z0-9]"), "")
}