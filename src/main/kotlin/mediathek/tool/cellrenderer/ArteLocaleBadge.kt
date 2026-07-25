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

package mediathek.tool.cellrenderer

import java.awt.*
import java.util.*
import javax.swing.UIManager
import kotlin.math.min

internal object ArteLocaleBadge {
    private const val MIN_FONT_SIZE = 7f
    private const val MAX_FONT_SIZE = 8f
    private const val RELATIVE_FONT_SIZE = 0.4f
    private const val HORIZONTAL_PADDING = 2
    private const val VERTICAL_PADDING = 1
    private const val ARC_SIZE = 4
    private val BADGE_BACKGROUND = Color(255, 255, 255, 245)
    private val BADGE_BORDER = Color(0, 0, 0, 210)
    private val LOCALIZED_ARTE_SENDER = Regex("^arte[.]([a-z]{2})$")

    fun localeCode(sender: String): String? = LOCALIZED_ARTE_SENDER
        .matchEntire(sender.lowercase(Locale.ROOT))
        ?.groupValues
        ?.get(1)
        ?.uppercase(Locale.ROOT)

    fun visibleIconBounds(iconBounds: Rectangle, cellBounds: Rectangle): Rectangle =
        iconBounds.intersection(cellBounds)

    fun paint(graphics: Graphics2D, localeCode: String, iconBounds: Rectangle): Rectangle {
        graphics.clip(iconBounds)
        graphics.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)
        graphics.setRenderingHint(RenderingHints.KEY_TEXT_ANTIALIASING, RenderingHints.VALUE_TEXT_ANTIALIAS_ON)

        val baseFont = UIManager.getFont("Label.font") ?: Font(Font.SANS_SERIF, Font.PLAIN, 12)
        val fontSize = (iconBounds.height * RELATIVE_FONT_SIZE).coerceIn(MIN_FONT_SIZE, MAX_FONT_SIZE)
        graphics.font = baseFont.deriveFont(Font.BOLD, fontSize)
        val metrics = graphics.fontMetrics
        val badgeWidth = min(iconBounds.width, metrics.stringWidth(localeCode) + HORIZONTAL_PADDING * 2)
        val badgeHeight = min(iconBounds.height, metrics.height + VERTICAL_PADDING * 2)
        val badgeBounds = Rectangle(
            iconBounds.x + iconBounds.width - badgeWidth,
            iconBounds.y,
            badgeWidth,
            badgeHeight,
        )

        graphics.color = BADGE_BACKGROUND
        graphics.fillRoundRect(
            badgeBounds.x,
            badgeBounds.y,
            badgeBounds.width,
            badgeBounds.height,
            ARC_SIZE,
            ARC_SIZE,
        )
        graphics.color = BADGE_BORDER
        graphics.drawRoundRect(
            badgeBounds.x,
            badgeBounds.y,
            badgeBounds.width - 1,
            badgeBounds.height - 1,
            ARC_SIZE,
            ARC_SIZE,
        )
        graphics.color = Color.BLACK
        val textX = badgeBounds.x + (badgeBounds.width - metrics.stringWidth(localeCode)) / 2
        val textY = badgeBounds.y + (badgeBounds.height - metrics.height) / 2 + metrics.ascent
        graphics.drawString(localeCode, textX, textY)
        return badgeBounds
    }
}
