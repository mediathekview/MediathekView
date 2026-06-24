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

package mediathek.filmlisten

import kotlinx.coroutines.CoroutineScope
import kotlinx.coroutines.Dispatchers
import kotlinx.coroutines.launch
import kotlinx.coroutines.swing.Swing
import mediathek.config.CommandLineOptions
import mediathek.filmeSuchen.ListenerFilmeLaden
import mediathek.filmeSuchen.ListenerFilmeLadenEvent
import org.apache.logging.log4j.LogManager
import java.awt.GraphicsEnvironment
import java.util.concurrent.atomic.AtomicBoolean
import javax.swing.event.EventListenerList

internal class FilmListLoadEventDispatcher(
    private val scope: CoroutineScope,
) {
    private val listeners = EventListenerList()
    private val appLifetimeCompletionAlreadyNotified = AtomicBoolean(false)

    fun addListener(listener: ListenerFilmeLaden) {
        synchronized(listeners) {
            listeners.add(ListenerFilmeLaden::class.java, listener)
        }
    }

    fun removeListener(listener: ListenerFilmeLaden) {
        synchronized(listeners) {
            listeners.remove(ListenerFilmeLaden::class.java, listener)
        }
    }

    fun notifyStart(event: ListenerFilmeLadenEvent) {
        try {
            notifyListenersAsync { listener -> listener.start(event) }
        } catch (ex: Exception) {
            logger.error(ex)
        }
    }

    fun notifyProgress(event: ListenerFilmeLadenEvent) {
        try {
            notifyListenersAsync { listener -> listener.progress(event) }
        } catch (ex: Exception) {
            logger.error(ex)
        }
    }

    fun notifyFinished(event: ListenerFilmeLadenEvent) {
        try {
            notifyListenersAsync { listener -> listener.fertig(event) }

            if (appLifetimeCompletionAlreadyNotified.compareAndSet(false, true)) {
                notifyListenersAsync { listener -> listener.fertigOnlyOne(event) }
            }
        } catch (ex: Exception) {
            logger.error(ex)
        }
    }

    private fun notifyListenersAsync(action: (ListenerFilmeLaden) -> Unit) {
        val currentListeners = synchronized(listeners) {
            listeners.getListeners(ListenerFilmeLaden::class.java)
        }
        val notifyListeners = {
            currentListeners.forEach { listener -> action(listener) }
        }
        if (CommandLineOptions.isDownloadAndQuit() || GraphicsEnvironment.isHeadless()) {
            notifyListeners()
        } else {
            runOnSwing(notifyListeners)
        }
    }

    private fun runOnSwing(action: () -> Unit) {
        scope.launch(Dispatchers.Swing) {
            action()
        }
    }

    private companion object {
        private val logger = LogManager.getLogger(FilmListLoadEventDispatcher::class.java)
    }
}
