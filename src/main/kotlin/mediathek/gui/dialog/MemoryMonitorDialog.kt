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

package mediathek.gui.dialog

import mediathek.config.application.ApplicationConfiguration
import mediathek.mainwindow.MemoryUsagePanel
import java.awt.BorderLayout
import java.awt.Dimension
import java.awt.event.ComponentAdapter
import java.awt.event.ComponentEvent
import java.awt.event.WindowAdapter
import java.awt.event.WindowEvent
import java.time.Duration
import javax.swing.JDialog
import javax.swing.JFrame

class MemoryMonitorDialog(
    parent: JFrame,
    private val onClose: Runnable,
) : JDialog(parent, "Speicherverbrauch", false) {

    private val memoryUsagePanel = MemoryUsagePanel(HISTORY_WINDOW, SAMPLE_INTERVAL)
    private var preserveVisibilityOnClose = false

    init {
        type = Type.UTILITY
        defaultCloseOperation = DISPOSE_ON_CLOSE

        memoryUsagePanel.preferredSize = DEFAULT_SIZE
        add(memoryUsagePanel, BorderLayout.CENTER)
        pack()
        restoreBounds()

        addComponentListener(object : ComponentAdapter() {
            override fun componentResized(event: ComponentEvent) {
                storeBounds()
            }

            override fun componentMoved(event: ComponentEvent) {
                storeBounds()
            }
        })
        addWindowListener(object : WindowAdapter() {
            override fun windowOpened(event: WindowEvent) {
                storeVisibility(true)
            }

            override fun windowClosed(event: WindowEvent) {
                storeVisibility(preserveVisibilityOnClose)
                notifyClosed()
            }
        })
    }

    override fun dispose() {
        memoryUsagePanel.close()
        super.dispose()
    }

    fun disposeForShutdown() {
        preserveVisibilityOnClose = true
        dispose()
    }

    private fun restoreBounds() {
        readStoredBounds()?.let(::applyBounds) ?: applyDefaultBounds()
    }

    private fun readStoredBounds(): DialogBounds? {
        val state = ApplicationConfiguration.getInstance().memoryMonitorDialogState
        return if (state.hasStoredBounds()) {
            DialogBounds(state.x, state.y, state.width, state.height)
        } else {
            null
        }
    }

    private fun applyBounds(bounds: DialogBounds) {
        setBounds(bounds.x, bounds.y, bounds.width, bounds.height)
    }

    private fun applyDefaultBounds() {
        size = DEFAULT_SIZE
        setLocationRelativeTo(owner)
    }

    private fun storeBounds() {
        if (!isShowing) {
            return
        }

        val bounds = bounds
        ApplicationConfiguration.getInstance().setMemoryMonitorDialogBounds(bounds.x, bounds.y, bounds.width, bounds.height)
    }

    private fun storeVisibility(visible: Boolean) {
        ApplicationConfiguration.getInstance().memoryMonitorDialogVisible = visible
    }

    private fun notifyClosed() {
        onClose.run()
    }

    private data class DialogBounds(
        val x: Int,
        val y: Int,
        val width: Int,
        val height: Int,
    )

    companion object {
        private val HISTORY_WINDOW: Duration = Duration.ofMinutes(2)
        private val SAMPLE_INTERVAL: Duration = Duration.ofSeconds(1)
        private val DEFAULT_SIZE = Dimension(480, 240)
    }
}
