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

import kotlinx.coroutines.*
import kotlinx.coroutines.swing.Swing
import mediathek.config.Daten
import mediathek.gui.messages.UpdateStatusBarLeftDisplayEvent
import mediathek.tool.MessageBus
import net.engio.mbassy.listener.Handler
import javax.swing.JLabel
import kotlin.time.Duration.Companion.seconds

class FilmSizeInfoLabel(private val mediathekGui: MediathekGui) : JLabel() {
    private var oldGesamt = 0
    private var oldRowCount = 0
    private var uiScope: CoroutineScope? = null
    private var updateJob: Job? = null
    private var subscribedToMessageBus = false

    override fun addNotify() {
        super.addNotify()
        subscribeToMessageBus()
        startUpdating()
    }

    override fun removeNotify() {
        stopUpdating()
        unsubscribeFromMessageBus()
        super.removeNotify()
    }

    @Suppress("UNUSED_PARAMETER")
    @Handler
    private fun handleLeftDisplayUpdate(event: UpdateStatusBarLeftDisplayEvent) {
        uiScope?.launch {
            updateValues()
        }
    }

    private fun startUpdating() {
        if (updateJob?.isActive == true) {
            return
        }

        val scope = CoroutineScope(SupervisorJob() + Dispatchers.Swing)
        uiScope = scope
        updateJob = scope.launch {
            while (isActive) {
                updateValues()
                delay(1.seconds)
            }
        }
    }

    private fun stopUpdating() {
        updateJob?.cancel()
        updateJob = null
        uiScope?.cancel()
        uiScope = null
    }

    private fun subscribeToMessageBus() {
        if (!subscribedToMessageBus) {
            MessageBus.messageBus.subscribe(this)
            subscribedToMessageBus = true
        }
    }

    private fun unsubscribeFromMessageBus() {
        if (subscribedToMessageBus) {
            MessageBus.messageBus.unsubscribe(this)
            subscribedToMessageBus = false
        }
    }

    private fun updateValues() {
        val gesamt = Daten.getInstance().listeFilme.size
        val rowCount = mediathekGui.filmTableRowCount

        if (gesamt == oldGesamt && rowCount == oldRowCount) {
            return
        }

        val textLinks = if (gesamt == rowCount) {
            createFilmLabel(rowCount)
        } else {
            "${createFilmLabel(rowCount)} (Insgesamt: $gesamt)"
        }

        text = textLinks

        oldGesamt = gesamt
        oldRowCount = rowCount
    }

    private fun createFilmLabel(rowCount: Int): String = if (rowCount == 1) {
        "1 Film"
    } else {
        "$rowCount Filme"
    }
}
