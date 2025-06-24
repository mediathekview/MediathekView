/*
 * Copyright (c) 2025 derreisende77.
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

package mediathek.gui.tabs.tab_livestreams.icons

import com.formdev.flatlaf.extras.FlatSVGIcon
import java.awt.Dimension
import java.net.URL
import javax.swing.JLabel

class SvgSenderIconLabel : JLabel() {

    private var iconUrl: URL? = null
    private var targetSize = 24  // Both width and height for bounding box

    fun setSenderIcon(url: URL, boundingSize: Int = 24) {
        iconUrl = url
        targetSize = boundingSize
        icon = iconUrl?.let {
            FlatSVGIcon(it).derive(targetSize, targetSize)
        }
        horizontalAlignment = CENTER
        verticalAlignment = CENTER
        revalidate()
        repaint()
    }

    override fun getPreferredSize(): Dimension {
        return Dimension(targetSize, targetSize)
    }
}