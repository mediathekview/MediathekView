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

package mediathek.mainwindow

import mediathek.config.Daten
import mediathek.filmeSuchen.ListenerFilmeLaden
import mediathek.filmeSuchen.ListenerFilmeLadenEvent
import mediathek.gui.messages.TableModelChangeEvent
import mediathek.tool.MessageBus
import java.beans.PropertyChangeListener
import javax.swing.SwingUtilities
import javax.swing.UIManager

class MainWindowLifecycle(
    private val messageBusSubscriber: Any,
    private val daten: Daten,
    private val dialogOwner: MainWindowHandle,
    private val lookAndFeelListener: PropertyChangeListener,
    private val filmlistProgressListener: ListenerFilmeLaden,
    private val filmListListener: ListenerFilmeLaden,
    private val filmListLoadHost: FilmListLoadHost,
    private val zeitraumFilterValueProvider: () -> String?,
) : AutoCloseable {
    private var messageBusSubscribed = false
    private var downloadDialogOwnerRegistered = false
    private var lookAndFeelListenerRegistered = false
    private var filmlistProgressListenerRegistered = false
    private var filmListListenersRegistered = false
    private val bookmarkRefreshListener = object : ListenerFilmeLaden() {
        override fun fertig(@Suppress("UNUSED_PARAMETER") event: ListenerFilmeLadenEvent) {
            daten.listeBookmarkList.refreshFromCurrentFilmListAsync()
        }
    }

    fun registerLookAndFeelListener() {
        requireEventDispatchThread()
        if (lookAndFeelListenerRegistered) {
            return
        }

        UIManager.addPropertyChangeListener(lookAndFeelListener)
        lookAndFeelListenerRegistered = true
    }

    fun registerFilmlistProgressListener() {
        requireEventDispatchThread()
        if (filmlistProgressListenerRegistered) {
            return
        }

        daten.filmeLaden.addFilmLoadListener(filmlistProgressListener)
        filmlistProgressListenerRegistered = true
    }

    fun registerFilmListListeners() {
        requireEventDispatchThread()
        if (filmListListenersRegistered) {
            return
        }

        daten.listeBlacklist.setZeitraumFilterValueProvider(zeitraumFilterValueProvider)
        daten.filmeLaden.setUiHost(filmListLoadHost)
        daten.filmeLaden.addFilmLoadListener(filmListListener)
        daten.filmeLaden.addFilmLoadListener(bookmarkRefreshListener)
        filmListListenersRegistered = true
    }

    fun start() {
        requireEventDispatchThread()
        registerDownloadDialogOwner()
        subscribeToMessageBus()
    }

    private fun requireEventDispatchThread() {
        check(SwingUtilities.isEventDispatchThread()) { "MainWindowLifecycle must be accessed on the EDT." }
    }

    private fun registerDownloadDialogOwner() {
        if (downloadDialogOwnerRegistered) {
            return
        }

        daten.downloadStartCoordinator.setDialogOwner(dialogOwner)
        downloadDialogOwnerRegistered = true
    }

    private fun subscribeToMessageBus() {
        if (messageBusSubscribed) {
            return
        }

        val messageBus = MessageBus.messageBus
        // Preserve startup behavior: publish current table-model state before subscribing the main window.
        messageBus.publishAsync(TableModelChangeEvent(true, false))
        messageBus.subscribe(messageBusSubscriber)
        messageBusSubscribed = true
    }

    override fun close() {
        requireEventDispatchThread()
        unsubscribeFromMessageBus()
        unregisterDownloadDialogOwner()
        unregisterFilmListListeners()
        unregisterFilmlistProgressListener()
        unregisterLookAndFeelListener()
    }

    private fun unsubscribeFromMessageBus() {
        if (!messageBusSubscribed) {
            return
        }

        MessageBus.messageBus.unsubscribe(messageBusSubscriber)
        messageBusSubscribed = false
    }

    private fun unregisterDownloadDialogOwner() {
        if (!downloadDialogOwnerRegistered) {
            return
        }

        daten.downloadStartCoordinator.setDialogOwner(null)
        downloadDialogOwnerRegistered = false
    }

    private fun unregisterFilmListListeners() {
        if (!filmListListenersRegistered) {
            return
        }

        daten.listeBlacklist.setZeitraumFilterValueProvider(null)
        daten.filmeLaden.setUiHost(null)
        daten.filmeLaden.removeFilmLoadListener(bookmarkRefreshListener)
        daten.filmeLaden.removeFilmLoadListener(filmListListener)
        filmListListenersRegistered = false
    }

    private fun unregisterFilmlistProgressListener() {
        if (!filmlistProgressListenerRegistered) {
            return
        }

        daten.filmeLaden.removeFilmLoadListener(filmlistProgressListener)
        filmlistProgressListenerRegistered = false
    }

    private fun unregisterLookAndFeelListener() {
        if (!lookAndFeelListenerRegistered) {
            return
        }

        UIManager.removePropertyChangeListener(lookAndFeelListener)
        lookAndFeelListenerRegistered = false
    }
}
