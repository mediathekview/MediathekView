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

package mediathek.controller.history

import mediathek.audiothek.model.AudioEntry
import mediathek.gui.messages.history.AudioSeenStateChangedEvent
import mediathek.tool.MessageBus
import org.apache.logging.log4j.LogManager

class AudioSeenHistoryController : AutoCloseable {
    private val controller = SeenHistoryController()

    fun markSeen(entry: AudioEntry?) {
        if (entry == null) {
            logger.warn("markSeen: no audio entry found")
            return
        }

        val historyEntry = entry.toSeenHistoryEntry() ?: return
        if (controller.markSeen(historyEntry)) {
            sendAudioSeenStateChanged(true, listOf(entry))
        }
    }

    fun markUnseen(entry: AudioEntry) {
        val url = entry.audioUrl?.toString().orEmpty()
        if (url.isBlank()) {
            return
        }

        if (controller.markUnseen(SeenHistorySource.AUDIOTHEK, url)) {
            sendAudioSeenStateChanged(false, listOf(entry))
        }
    }

    fun hasBeenSeen(entry: AudioEntry): Boolean {
        val url = entry.audioUrl?.toString().orEmpty()
        if (url.isBlank()) {
            return false
        }
        return controller.hasBeenSeen(SeenHistorySource.AUDIOTHEK, url)
    }

    fun prepareMemoryCache() {
        controller.prepareMemoryCache(SeenHistorySource.AUDIOTHEK)
    }

    fun isMemoryCachePrepared(): Boolean =
        controller.isMemoryCachePrepared(SeenHistorySource.AUDIOTHEK)

    override fun close() {
        controller.close()
    }

    private fun sendAudioSeenStateChanged(seen: Boolean, entries: List<AudioEntry>) {
        if (entries.isNotEmpty()) {
            MessageBus.messageBus.publishAsync(AudioSeenStateChangedEvent(seen, entries))
        }
    }

    companion object {
        private val logger = LogManager.getLogger()
    }
}

private fun AudioEntry.toSeenHistoryEntry(): SeenHistoryEntry? {
    val url = audioUrl?.toString()?.takeIf(String::isNotBlank) ?: return null
    return SeenHistoryEntry(
        source = SeenHistorySource.AUDIOTHEK,
        theme = theme,
        title = title,
        url = url
    )
}
