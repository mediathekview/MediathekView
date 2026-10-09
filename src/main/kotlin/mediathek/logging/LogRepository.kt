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

package mediathek.logging

import kotlinx.coroutines.channels.BufferOverflow
import kotlinx.coroutines.channels.Channel
import java.util.*

object LogRepository {

    const val HISTORY_LIMIT: Int = 20_000
    const val LIVE_BUFFER_CAPACITY: Int = 5_000

    private val lock = Any()
    private val history = ArrayDeque<LogEntry>(HISTORY_LIMIT)

    private val liveChannel = Channel<LogEntry>(
        capacity = LIVE_BUFFER_CAPACITY,
        onBufferOverflow = BufferOverflow.DROP_OLDEST
    )

    /**
     * Called from Log4j threads. Must not block.
     */
    fun emit(entry: LogEntry) {
        synchronized(lock) {
            if (history.size >= HISTORY_LIMIT) history.removeFirst()
            history.addLast(entry)
        }

        liveChannel.trySend(entry)
    }

    /**
     * Snapshot current history. Use this when a panel opens.
     */
    fun snapshot(): List<LogEntry> =
        synchronized(lock) { history.toList() }

    /**
     * Live stream for consumers (LogPanel).
     */
    fun channel(): Channel<LogEntry> = liveChannel
}