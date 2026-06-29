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

import mediathek.controller.starter.DownloadServices
import mediathek.daten.blacklist.BlacklistServices
import mediathek.filmeSuchen.ListenerFilmeLaden
import mediathek.filmeSuchen.ListenerFilmeLadenEvent
import mediathek.filmlisten.FilmeLaden
import mediathek.gui.bookmark.BookmarkServices
import mediathek.gui.messages.TableModelChangeEvent
import mediathek.tool.MessageBus
import java.beans.PropertyChangeListener
import javax.swing.SwingUtilities
import javax.swing.UIManager

class MainWindowLifecycle(
    private val messageBusSubscriber: Any,
    private val downloads: DownloadServices,
    private val filmListLoader: FilmeLaden,
    private val blacklist: BlacklistServices,
    private val bookmarks: BookmarkServices,
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
            bookmarks.list.refreshFromCurrentFilmListAsync()
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

        filmListLoader.addFilmLoadListener(filmlistProgressListener)
        filmlistProgressListenerRegistered = true
    }

    fun registerFilmListListeners() {
        requireEventDispatchThread()
        if (filmListListenersRegistered) {
            return
        }

        blacklist.setZeitraumFilterValueProvider(zeitraumFilterValueProvider)
        filmListLoader.setUiHost(filmListLoadHost)
        filmListLoader.addFilmLoadListener(filmListListener)
        filmListLoader.addFilmLoadListener(bookmarkRefreshListener)
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

        downloads.setDialogOwner(dialogOwner)
        downloads.startStarter()
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

        downloads.setDialogOwner(null)
        downloadDialogOwnerRegistered = false
    }

    private fun unregisterFilmListListeners() {
        if (!filmListListenersRegistered) {
            return
        }

        blacklist.setZeitraumFilterValueProvider(null)
        filmListLoader.setUiHost(null)
        filmListLoader.removeFilmLoadListener(bookmarkRefreshListener)
        filmListLoader.removeFilmLoadListener(filmListListener)
        filmListListenersRegistered = false
    }

    private fun unregisterFilmlistProgressListener() {
        if (!filmlistProgressListenerRegistered) {
            return
        }

        filmListLoader.removeFilmLoadListener(filmlistProgressListener)
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
