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

package mediathek.gui.tabs.tab_film

import com.formdev.flatlaf.extras.FlatSVGIcon
import com.formdev.flatlaf.util.ScaledImageIcon
import mediathek.tool.sender_icon_cache.MVSenderIconCache
import mediathek.tool.sender_icon_cache.SenderIconRenderUtil
import java.awt.Dimension
import javax.swing.Icon
import javax.swing.JLabel

class SenderIconLabel : JLabel() {
    private var iconDimension = DEFAULT_ICON_DIMENSION

    init {
        text = ""
        icon = null
    }

    private fun sizeToIcon(icon: Icon) {
        preferredSize = Dimension(icon.iconWidth, icon.iconHeight)
    }

    fun setMaxIconSize(iconDimension: Dimension) {
        this.iconDimension = Dimension(iconDimension)
        if (icon == null) {
            preferredSize = Dimension(iconDimension)
            maximumSize = Dimension(iconDimension)
        }
    }

    fun setSender(sender: String?) {
        if (sender == null) {
            icon = null
            preferredSize = Dimension(iconDimension)
            return
        }

        MVSenderIconCache[sender].ifPresentOrElse({ cachedIcon ->
            val renderedIcon = if (cachedIcon is FlatSVGIcon) {
                val destinationDimension = SenderIconRenderUtil.calculateFittedDimensionAllowUpscale(
                    Dimension(cachedIcon.iconWidth, cachedIcon.iconHeight),
                    iconDimension
                )
                cachedIcon.derive(destinationDimension.width, destinationDimension.height)
            } else {
                val imageDimension = Dimension(cachedIcon.iconWidth, cachedIcon.iconHeight)
                val destinationDimension = SenderIconRenderUtil.calculateFittedDimensionAllowUpscale(
                    imageDimension,
                    iconDimension
                )
                ScaledImageIcon(cachedIcon, destinationDimension.width, destinationDimension.height)
            }

            text = ""
            icon = renderedIcon
            sizeToIcon(renderedIcon)
        }, {
            icon = null
            text = sender
            preferredSize = null
        })
    }

    companion object {
        private val DEFAULT_ICON_DIMENSION = Dimension(96, 96)
    }
}
