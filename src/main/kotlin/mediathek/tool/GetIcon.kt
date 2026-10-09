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

import java.awt.Image
import javax.swing.ImageIcon

object GetIcon {
    private const val PFAD_PROGRAMM = "/mediathek/res/programm/"

    fun getProgramIcon(strIcon: String, w: Int, h: Int): ImageIcon =
        getIcon(strIcon, PFAD_PROGRAMM, w, h)

    fun getIcon(strIcon: String, path: String, w: Int, h: Int): ImageIcon {
        val icon = getStandard(strIcon, path)

        if (w > 0 && h > 0 && (icon.iconWidth != w || icon.iconHeight != h)) {
            icon.image = icon.image.getScaledInstance(w, h, Image.SCALE_AREA_AVERAGING)
        }
        return icon
    }

    private fun getStandard(strIcon: String, path: String): ImageIcon =
        ImageIcon(GetIcon::class.java.getResource(path + strIcon))
}
