/*
 * Copyright (c) 2025-2026 derreisende77.
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

package mediathek.gui.expiration

import mediathek.tool.timer.TimerPool
import org.apache.logging.log4j.LogManager
import java.time.Duration
import java.time.Instant
import java.time.LocalDate
import java.util.concurrent.ConcurrentHashMap
import java.util.concurrent.ScheduledFuture
import java.util.concurrent.TimeUnit

object SenderExpirationService {
    private val LOG = LogManager.getLogger()

    private data class CacheEntry(
        val expiryDate: LocalDate?,
        val fetchedAt: Instant
    )

    private val ARD_SENDERS = setOf(
        "ard", "ardalpha", "br", "funknet", "hr", "mdr", "ndr",
        "one", "phoenix", "radiobrementv", "rbb", "sr", "swr",
        "tagesschau24", "wdr"
    )
    private val expiryCache = ConcurrentHashMap<String, CacheEntry>()
    private val positiveTtl: Duration = Duration.ofMinutes(15)
    private val negativeTtl: Duration = Duration.ofMinutes(5)
    private const val CLEANUP_INTERVAL_SECONDS: Long = 60
    @Suppress("unused")
    private val cleanupFuture: ScheduledFuture<*> = TimerPool.timerPool.scheduleWithFixedDelay(
        { evictExpiredEntries(Instant.now()) },
        CLEANUP_INTERVAL_SECONDS,
        CLEANUP_INTERVAL_SECONDS,
        TimeUnit.SECONDS
    )

    @JvmStatic
    fun fetchExpiryDate(sender: String, websiteUrl: String): LocalDate? {
        val normalizedSender = normalizeSender(sender)
        val cacheKey = websiteUrl.trim()
        if (cacheKey.isEmpty()) {
            return null
        }

        val now = Instant.now()
        expiryCache[cacheKey]?.let { entry ->
            val ttl = if (entry.expiryDate != null) positiveTtl else negativeTtl
            if (entry.fetchedAt.plus(ttl).isAfter(now)) {
                return entry.expiryDate
            }
        }

        val result = fetchExpiryDateUncached(normalizedSender, websiteUrl)
        expiryCache[cacheKey] = CacheEntry(result, now)
        return result
    }

    private fun evictExpiredEntries(now: Instant) {
        try {
            expiryCache.entries.removeIf { (_, entry) ->
                val ttl = if (entry.expiryDate != null) positiveTtl else negativeTtl
                entry.fetchedAt.plus(ttl).isBefore(now)
            }
        } catch (ex: Exception) {
            LOG.debug("Failed to evict expiration-cache entries", ex)
        }
    }

    private fun fetchExpiryDateUncached(normalizedSender: String, websiteUrl: String): LocalDate? =
        when {
            "arte" in normalizedSender -> ArteExpiryHelper.getExpiryInfo(websiteUrl).map { it.expiryDate }
                .orElse(null)

            normalizedSender == "3sat" -> ThreeSatExpiryHelper.getExpiryInfo(websiteUrl).map { it.expiryDate }
                .orElse(null)

            normalizedSender in ARD_SENDERS -> ArdMediathekExpiryHelper.getExpiryInfo(websiteUrl)
                .map { it.expiryDate }.orElse(null)

            normalizedSender == "orf" -> OrfExpiryHelper.getExpiryInfo(websiteUrl).map { it.expiryDate }.orElse(null)
            else -> null
        }

    private fun normalizeSender(sender: String) =
        sender.trim().lowercase().replace(Regex("[^a-z0-9]"), "")
}
