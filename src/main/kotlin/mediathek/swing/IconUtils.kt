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

import com.formdev.flatlaf.FlatLaf
import org.apache.commons.lang3.SystemUtils
import org.kordamp.ikonli.Ikon
import org.kordamp.ikonli.swing.FontIcon
import java.awt.Color
import java.awt.image.BufferedImage
import java.lang.ref.WeakReference
import javax.swing.*

object IconUtils {
    const val DEFAULT_SIZE: Int = 16
    const val DEFAULT_TOOLBAR_SIZE: Int = 18

    val DEFAULT_LIGHT_COLOR: Color = Color(110, 110, 110)

    val DEFAULT_DARK_COLOR: Color = Color(176, 177, 179)

    private const val DEFAULT_MAC_TOOLBAR_WINDOWBAR_SIZE: Int = 16
    private const val DEFAULT_WINDOWS_TOOLBAR_WINDOWBAR_SIZE: Int = 18
    private val themedIcons = mutableListOf<WeakReference<FontIcon>>()

    init {
        UIManager.addPropertyChangeListener { event ->
            if (event.propertyName == "lookAndFeel") {
                updateIconColors()
            }
        }
    }

    fun of(ikon: Ikon): FontIcon = of(ikon, DEFAULT_SIZE)

    fun generateDisabledIcon(action: Action): ImageIcon? {
        val normalIcon = action.getValue(Action.SMALL_ICON) as? FontIcon
        return normalIcon?.let(::generateDisabledIcon)
    }

    fun generateDisabledIcon(normalIcon: Icon): ImageIcon {
        val image = BufferedImage(normalIcon.iconWidth, normalIcon.iconHeight, BufferedImage.TYPE_INT_ARGB)
        val graphics = image.createGraphics()
        try {
            normalIcon.paintIcon(null, graphics, 0, 0)
        } finally {
            graphics.dispose()
        }

        return ImageIcon(GrayFilter.createDisabledImage(image))
    }

    fun windowBarSpecificToolbarIcon(ikon: Ikon): FontIcon = of(ikon, windowBarSpecificSize())

    fun toolbarIcon(ikon: Ikon): FontIcon = of(ikon, DEFAULT_TOOLBAR_SIZE)

    fun windowBarSpecificToolbarIcon(ikon: Ikon, color: Color): FontIcon =
        FontIcon.of(ikon, windowBarSpecificSize(), color)

    fun of(ikon: Ikon, size: Int): FontIcon = of(ikon, size, defaultColor())

    fun of(ikon: Ikon, size: Int, color: Color): FontIcon {
        val icon = FontIcon.of(ikon, size, color)
        themedIcons += WeakReference(icon)
        return icon
    }

    private fun windowBarSpecificSize(): Int =
        when {
            SystemUtils.IS_OS_MAC_OSX -> DEFAULT_MAC_TOOLBAR_WINDOWBAR_SIZE
            SystemUtils.IS_OS_WINDOWS -> DEFAULT_WINDOWS_TOOLBAR_WINDOWBAR_SIZE
            else -> DEFAULT_TOOLBAR_SIZE
        }

    private fun defaultColor(): Color = if (FlatLaf.isLafDark()) DEFAULT_DARK_COLOR else DEFAULT_LIGHT_COLOR

    private fun updateIconColors() {
        if (themedIcons.isEmpty()) {
            return
        }

        val iterator = themedIcons.iterator()
        while (iterator.hasNext()) {
            val icon = iterator.next().get()
            if (icon == null) {
                iterator.remove()
            } else {
                icon.iconColor = defaultColor()
            }
        }
    }
}
