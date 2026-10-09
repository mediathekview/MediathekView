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
import org.apache.logging.log4j.LogManager
import java.awt.GraphicsEnvironment
import java.util.concurrent.atomic.AtomicBoolean
import javax.swing.event.EventListenerList

internal class FilmListLoadEventDispatcher(
    private val scope: CoroutineScope,
) {
    private val listeners = EventListenerList()
    private val appLifetimeCompletionAlreadyNotified = AtomicBoolean(false)

    fun addListener(listener: FilmListLoadListener) {
        synchronized(listeners) {
            listeners.add(FilmListLoadListener::class.java, listener)
        }
    }

    fun removeListener(listener: FilmListLoadListener) {
        synchronized(listeners) {
            listeners.remove(FilmListLoadListener::class.java, listener)
        }
    }

    fun notifyStart(progress: FilmListLoadProgress) {
        try {
            notifyListenersAsync { listener -> listener.loadStarted(progress) }
        } catch (ex: Exception) {
            logger.error(ex)
        }
    }

    fun notifyProgress(progress: FilmListLoadProgress) {
        try {
            notifyListenersAsync { listener -> listener.loadProgress(progress) }
        } catch (ex: Exception) {
            logger.error(ex)
        }
    }

    fun notifyFinished(progress: FilmListLoadProgress) {
        try {
            notifyListenersAsync { listener -> listener.loadFinished(progress) }

            if (appLifetimeCompletionAlreadyNotified.compareAndSet(false, true)) {
                notifyListenersAsync { listener -> listener.firstLoadFinished(progress) }
            }
        } catch (ex: Exception) {
            logger.error(ex)
        }
    }

    private fun notifyListenersAsync(action: (FilmListLoadListener) -> Unit) {
        val currentListeners = synchronized(listeners) {
            listeners.getListeners(FilmListLoadListener::class.java)
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
