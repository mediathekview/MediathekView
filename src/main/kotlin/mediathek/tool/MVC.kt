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

package mediathek.tool

import com.formdev.flatlaf.FlatLaf
import java.awt.Color

class MVC(
    val key: String,
    private val lightDefaultColor: Color,
    private val darkDefaultColor: Color,
    val text: String,
) {
    private var lightOverrideColor: Color? = null
    private var darkOverrideColor: Color? = null

    val color: Color
        get() = getColor(FlatLaf.isLafDark())

    fun getColor(darkMode: Boolean): Color =
        if (darkMode) {
            darkOverrideColor ?: darkDefaultColor
        } else {
            lightOverrideColor ?: lightDefaultColor
        }

    fun set(color: Color) {
        lightOverrideColor = color
        darkOverrideColor = color
    }

    fun setColor(darkMode: Boolean, color: Color) {
        if (darkMode) {
            darkOverrideColor = color
        } else {
            lightOverrideColor = color
        }
    }

    fun hasOverride(): Boolean =
        lightOverrideColor != null || darkOverrideColor != null

    fun hasOverride(darkMode: Boolean): Boolean =
        if (darkMode) darkOverrideColor != null else lightOverrideColor != null

    fun getOverrideColor(darkMode: Boolean): Color? =
        if (darkMode) darkOverrideColor else lightOverrideColor

    fun reset() {
        lightOverrideColor = null
        darkOverrideColor = null
    }

    fun reset(darkMode: Boolean) {
        if (darkMode) {
            darkOverrideColor = null
        } else {
            lightOverrideColor = null
        }
    }
}
