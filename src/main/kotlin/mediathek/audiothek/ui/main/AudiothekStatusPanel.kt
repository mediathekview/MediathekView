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

package mediathek.audiothek.ui.main

import mediathek.swing.FilmAgeLabel
import org.jdesktop.swingx.JXBusyLabel
import org.jdesktop.swingx.JXStatusBar
import java.awt.FlowLayout
import javax.swing.JLabel
import javax.swing.JPanel
import javax.swing.SwingConstants
import kotlin.time.Duration

class AudiothekStatusPanel(
    private val ageProvider: () -> Duration?
) : JXStatusBar() {
    private val sourceLabel = JLabel("Audiothek erstellt: -")
    private val loadingBusyLabel = JXBusyLabel().apply {
        isBusy = false
        isVisible = false
        toolTipText = "Audiothek-Daten werden aktualisiert"
    }
    private val loadingLabel = JLabel("Audiothek wird aktualisiert").apply {
        isVisible = false
    }
    private val ageLabel = FilmAgeLabel(
        ageProvider = ageProvider,
        tooltip = "Alter der Audiothek"
    )
    private val activeDownloadsLabel = JLabel("")
    private val countLabel = JLabel("0 Einträge", SwingConstants.RIGHT)
    private val leftPanel = JPanel(FlowLayout(FlowLayout.LEFT, 8, 0)).apply {
        isOpaque = false
        add(sourceLabel)
        add(loadingBusyLabel)
        add(loadingLabel)
        add(activeDownloadsLabel)
    }

    init {
        add(leftPanel, Constraint(Constraint.ResizeBehavior.FILL))
        add(ageLabel)
        add(countLabel)
        activeDownloadsLabel.isVisible = false
    }

    fun setStand(text: String) {
        sourceLabel.text = text
    }

    fun setStandVisible(visible: Boolean) {
        sourceLabel.isVisible = visible
    }

    fun setCount(text: String) {
        countLabel.text = text
    }

    fun setActiveDownloads(count: Int) {
        if (count <= 0) {
            activeDownloadsLabel.text = ""
            activeDownloadsLabel.isVisible = false
            return
        }

        activeDownloadsLabel.text = if (count == 1) {
            "1 aktiver Download"
        } else {
            "$count aktive Downloads"
        }
        activeDownloadsLabel.isVisible = true
    }

    fun setLoading(loading: Boolean) {
        loadingBusyLabel.isBusy = loading
        loadingBusyLabel.isVisible = loading
        loadingLabel.isVisible = loading
    }
}
