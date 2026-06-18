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

import java.awt.BorderLayout
import java.awt.Container
import java.util.concurrent.atomic.AtomicBoolean
import java.util.function.BiConsumer
import java.util.function.IntSupplier
import javax.swing.JLabel
import javax.swing.JProgressBar
import javax.swing.SwingUtilities

class MainWindowStatusBarController(
    private val contentPane: Container,
    private val selectedListItemsProperty: ListSelectedItemsProperty,
    private val filmTableRowCount: IntSupplier,
    private val runOnEventDispatchThreadAndWait: BiConsumer<String, Runnable>,
) {
    val startupProgressLabel: JLabel = JLabel()
    val startupProgressBar: JProgressBar = JProgressBar()
    private lateinit var statusBar: FixedRedrawStatusBar

    fun createStatusBar() {
        statusBar = FixedRedrawStatusBar(filmTableRowCount, selectedListItemsProperty)
        contentPane.add(statusBar, BorderLayout.SOUTH)
    }

    fun updateComponentTreeUi() {
        SwingUtilities.updateComponentTreeUI(startupProgressLabel)
        SwingUtilities.updateComponentTreeUI(startupProgressBar)
    }

    fun installStartupProgress() {
        installProgressOnEdt(startupProgressLabel, startupProgressBar)
    }

    fun uninstallStartupProgress() {
        uninstallProgressOnEdt(startupProgressLabel, startupProgressBar)
    }

    fun showProgress(): StatusBarProgressHandle {
        val label = JLabel()
        val progressBar = JProgressBar()
        installProgressOnEdt(label, progressBar)
        return StatusBarProgressRegistration(label, progressBar)
    }

    private fun installProgressOnEdt(label: JLabel, progressBar: JProgressBar) {
        runOnEventDispatchThreadAndWait.accept("Install status bar progress", Runnable {
            installProgress(label, progressBar)
        })
    }

    private fun installProgress(label: JLabel, progressBar: JProgressBar) {
        if (label.parent !== statusBar) {
            statusBar.add(label)
        }
        if (progressBar.parent !== statusBar) {
            statusBar.add(progressBar)
        }
        refreshStatusBar()
    }

    private fun uninstallProgressOnEdt(label: JLabel, progressBar: JProgressBar) {
        runOnEventDispatchThreadAndWait.accept("Uninstall status bar progress", Runnable {
            uninstallProgress(label, progressBar)
        })
    }

    private fun uninstallProgress(label: JLabel, progressBar: JProgressBar) {
        if (progressBar.parent === statusBar) {
            statusBar.remove(progressBar)
        }
        if (label.parent === statusBar) {
            statusBar.remove(label)
        }
        refreshStatusBar()
    }

    private fun refreshStatusBar() {
        statusBar.revalidate()
        statusBar.repaint()
    }

    private inner class StatusBarProgressRegistration(
        private val label: JLabel,
        private val progressBar: JProgressBar,
    ) : StatusBarProgressHandle {
        private val closed = AtomicBoolean()

        override fun label(): JLabel = label

        override fun progressBar(): JProgressBar = progressBar

        override fun close() {
            if (!closed.compareAndSet(false, true)) {
                return
            }
            uninstallProgressOnEdt(label, progressBar)
        }
    }
}
