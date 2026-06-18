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
import mediathek.gui.messages.TableModelChangeEvent
import mediathek.tool.MessageBus
import java.beans.PropertyChangeListener
import javax.swing.UIManager

class MainWindowLifecycle(
    private val messageBusSubscriber: Any,
    private val daten: Daten,
    private val dialogOwner: MainWindowHandle,
    private val lookAndFeelListener: PropertyChangeListener,
) : AutoCloseable {
    private var messageBusSubscribed = false
    private var downloadDialogOwnerRegistered = false
    private var lookAndFeelListenerRegistered = false

    fun registerLookAndFeelListener() {
        if (lookAndFeelListenerRegistered) {
            return
        }

        UIManager.addPropertyChangeListener(lookAndFeelListener)
        lookAndFeelListenerRegistered = true
    }

    fun start() {
        registerDownloadDialogOwner()
        subscribeToMessageBus()
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
        unsubscribeFromMessageBus()
        unregisterDownloadDialogOwner()
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

    private fun unregisterLookAndFeelListener() {
        if (!lookAndFeelListenerRegistered) {
            return
        }

        UIManager.removePropertyChangeListener(lookAndFeelListener)
        lookAndFeelListenerRegistered = false
    }
}
