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

package mediathek.gui.watchlist

import mediathek.swing.IconUtils
import org.kordamp.ikonli.materialdesign2.MaterialDesignB
import java.awt.Color
import java.awt.Graphics
import java.awt.Graphics2D
import java.awt.RenderingHints
import javax.swing.JButton

/**
 * Toolbar bell button for the watchlist. Shows a red badge dot as long as new
 * watchlist notifications have not been acknowledged by opening the notification window.
 */
class WatchlistBellButton(showNotificationsAction: () -> Unit) : JButton() {
    private var hasUnseen = false

    init {
        icon = IconUtils.toolbarIcon(MaterialDesignB.BELL_OUTLINE)
        isFocusable = false
        addActionListener { showNotificationsAction() }
        setNotificationState(hasUnseen = false, pendingCount = 0)
    }

    fun setNotificationState(hasUnseen: Boolean, pendingCount: Int) {
        this.hasUnseen = hasUnseen
        toolTipText = when {
            hasUnseen -> "Neue Folgen auf der Watchlist eingetroffen"
            pendingCount > 0 -> "Watchlist: $pendingCount ungelesene Benachrichtigungen"
            else -> "Watchlist: keine neuen Folgen"
        }
        repaint()
    }

    override fun paintComponent(graphics: Graphics) {
        super.paintComponent(graphics)
        if (!hasUnseen) {
            return
        }

        val g2 = graphics.create() as Graphics2D
        try {
            g2.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)
            g2.color = BADGE_COLOR
            val dotSize = BADGE_DOT_SIZE
            g2.fillOval(width - dotSize - BADGE_DOT_MARGIN, BADGE_DOT_MARGIN, dotSize, dotSize)
        } finally {
            g2.dispose()
        }
    }

    private companion object {
        private const val BADGE_DOT_SIZE = 8
        private const val BADGE_DOT_MARGIN = 3
        private val BADGE_COLOR = Color(210, 40, 40)
    }
}
