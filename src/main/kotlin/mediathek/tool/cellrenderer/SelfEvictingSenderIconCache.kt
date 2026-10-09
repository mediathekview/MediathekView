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

package mediathek.tool.cellrenderer

import mediathek.tool.timer.TimerPool
import java.util.*
import java.util.concurrent.ConcurrentHashMap
import javax.swing.Icon
import kotlin.time.Duration.Companion.minutes

/**
 * A cache for sender icons at a specific cell dimension.
 * Live cache instances are cleared periodically without keeping renderers alive.
 */
class SelfEvictingSenderIconCache : ConcurrentHashMap<SenderCacheKey, Icon>(), AutoCloseable {
    init {
        register(this)
    }

    override fun close() {
        synchronized(caches) {
            caches.remove(this)
        }
        clear()
    }

    private companion object {
        private val caches = Collections.newSetFromMap(WeakHashMap<SelfEvictingSenderIconCache, Boolean>())

        init {
            TimerPool.scheduleAtFixedRate(::clearRegisteredCaches, 5.minutes, 5.minutes)
        }

        private fun register(cache: SelfEvictingSenderIconCache) {
            synchronized(caches) {
                caches.add(cache)
            }
        }

        private fun clearRegisteredCaches() {
            synchronized(caches) {
                caches.forEach(SelfEvictingSenderIconCache::clear)
            }
        }
    }
}
