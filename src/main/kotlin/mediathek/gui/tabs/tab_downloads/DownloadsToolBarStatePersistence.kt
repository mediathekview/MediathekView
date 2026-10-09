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

package mediathek.gui.tabs.tab_downloads

import mediathek.config.application.ApplicationConfiguration
import java.awt.Point
import java.awt.event.HierarchyEvent
import javax.swing.*
import javax.swing.plaf.basic.BasicToolBarUI

internal const val DOWNLOADS_TOOLBAR_ID_PROPERTY = "mediathek.downloads.toolbar.id"

internal fun JComponent.setDownloadsToolBarId(id: String) {
    putClientProperty(DOWNLOADS_TOOLBAR_ID_PROPERTY, id)
}

internal class DownloadsToolBarStatePersistence(
    private val toolBarRow: JPanel,
    toolBars: List<JToolBar>
) {
    private val applicationConfiguration = ApplicationConfiguration.getInstance()
    private val persistedToolBars = toolBars.mapIndexed { index, toolBar ->
        PersistedToolBar(toolBar, (toolBar.getClientProperty(DOWNLOADS_TOOLBAR_ID_PROPERTY) as? String) ?: "toolbar-$index")
    }
    private val snapshots = mutableMapOf<String, Snapshot>()
    private val saveTimer = Timer(750) { saveChangedSnapshots() }.apply {
        isRepeats = true
    }

    fun restoreLater() {
        toolBarRow.addHierarchyListener { event ->
            if ((event.changeFlags and HierarchyEvent.SHOWING_CHANGED.toLong()) != 0L) {
                scheduleFloatingWindowSync()
            }
        }
        SwingUtilities.invokeLater {
            restore()
            scheduleFloatingWindowSync()
            saveChangedSnapshots()
            saveTimer.start()
        }
    }

    private fun restore() {
        persistedToolBars.forEach { persistedToolBar ->
            val snapshot = readSnapshot(persistedToolBar.id)
            snapshots[persistedToolBar.id] = snapshot
            val ui = persistedToolBar.toolBar.ui as? BasicToolBarUI ?: return@forEach
            ui.setOrientation(snapshot.orientation)
            if (snapshot.floating) {
                val location = Point(snapshot.x, snapshot.y)
                ui.setFloatingLocation(location.x, location.y)
                ui.setFloating(true, location)
                persistedToolBar.toolBar.revalidate()
                persistedToolBar.toolBar.repaint()
            }
        }
    }

    private fun scheduleFloatingWindowSync() {
        SwingUtilities.invokeLater {
            updateFloatingWindowVisibility()
            listOf(75, 250).forEach { delay ->
                Timer(delay) {
                    updateFloatingWindowVisibility()
                }.apply {
                    isRepeats = false
                    start()
                }
            }
        }
    }

    private fun updateFloatingWindowVisibility() {
        val visible = toolBarRow.isShowing
        persistedToolBars.forEach { persistedToolBar ->
            val ui = persistedToolBar.toolBar.ui as? BasicToolBarUI ?: return@forEach
            if (ui.isFloating) {
                val window = SwingUtilities.getWindowAncestor(persistedToolBar.toolBar) ?: return@forEach
                if (window.isVisible != visible) {
                    window.isVisible = visible
                }
                persistedToolBar.toolBar.revalidate()
                persistedToolBar.toolBar.repaint()
                window.validate()
                window.repaint()
            }
        }
    }

    private fun saveChangedSnapshots() {
        if (!toolBarRow.isShowing) {
            return
        }

        persistedToolBars.forEach { persistedToolBar ->
            val snapshot = currentSnapshot(persistedToolBar.toolBar)
            if (snapshots[persistedToolBar.id] != snapshot) {
                snapshots[persistedToolBar.id] = snapshot
                writeSnapshot(persistedToolBar.id, snapshot)
            }
        }
    }

    private fun currentSnapshot(toolBar: JToolBar): Snapshot {
        val ui = toolBar.ui as? BasicToolBarUI
        val floating = ui?.isFloating ?: false
        val location = if (floating) {
            SwingUtilities.getWindowAncestor(toolBar)?.locationOnScreen ?: toolBar.locationOnScreen
        } else {
            Point(0, 0)
        }
        return Snapshot(
            floating = floating,
            x = location.x,
            y = location.y,
            orientation = toolBar.orientation
        )
    }

    private fun readSnapshot(id: String): Snapshot {
        val state = applicationConfiguration.getDownloadToolbarState(id, JToolBar.HORIZONTAL)
        return Snapshot(
            floating = state.floating,
            x = state.x,
            y = state.y,
            orientation = state.orientation
                .takeIf { it == JToolBar.HORIZONTAL || it == JToolBar.VERTICAL }
                ?: JToolBar.HORIZONTAL
        )
    }

    private fun writeSnapshot(id: String, snapshot: Snapshot) {
        applicationConfiguration.setDownloadToolbarState(
            id,
            ApplicationConfiguration.DownloadToolbarState(
                snapshot.floating,
                snapshot.x,
                snapshot.y,
                snapshot.orientation
            )
        )
    }

    private data class PersistedToolBar(val toolBar: JToolBar, val id: String)

    private data class Snapshot(
        val floating: Boolean,
        val x: Int,
        val y: Int,
        val orientation: Int
    )
}
