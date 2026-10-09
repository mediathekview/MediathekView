/*
 * Copyright (c) 2025-2026 derreisende77.
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

package mediathek.swing

import net.miginfocom.layout.AC
import net.miginfocom.layout.CC
import net.miginfocom.layout.LC
import net.miginfocom.swing.MigLayout
import org.jdesktop.swingx.JXBusyLabel
import java.awt.event.MouseAdapter
import javax.swing.JLabel
import javax.swing.JPanel
import javax.swing.SwingUtilities

/**
 * Displays an indefinite progress indicator and status messages as a glass pane overlay during app termination.
 */
class AppTerminationIndefiniteProgress(willBeShutDown: Boolean) : JPanel() {
    private val lblMessage = JLabel("Warte auf Abschluss der Downloads...")
    // A visible glass pane only blocks pointer input once it starts receiving mouse events itself.
    private val mouseBlocker = object : MouseAdapter() {}

    init {
        layout = MigLayout(
            LC().hideMode(3),
            AC().fill().fill(),
            AC()
        )
        addMouseListener(mouseBlocker)
        addMouseMotionListener(mouseBlocker)
        addMouseWheelListener(mouseBlocker)

        val busyLabel = JXBusyLabel().apply {
            delay *= 4
            isBusy = true
        }
        add(busyLabel, CC().cell(0, 0).span(1, 3))

        add(lblMessage, CC().cell(1, 0))
        if (willBeShutDown) {
            add(JLabel("Der Rechner wird danach heruntergefahren."), CC().cell(1, 1))
        }
        add(JLabel("Sie können den Vorgang mit Escape abbrechen."), CC().cell(1, 2))
    }

    fun setMessage(text: String) {
        SwingUtilities.invokeLater { lblMessage.text = text }
    }
}
