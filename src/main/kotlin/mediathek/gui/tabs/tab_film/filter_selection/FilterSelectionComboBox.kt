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

package mediathek.gui.tabs.tab_film.filter_selection

import mediathek.swing.IconUtils
import mediathek.tool.FilterDTO
import org.kordamp.ikonli.fontawesome6.FontAwesomeSolid
import org.kordamp.ikonli.swing.FontIcon
import java.awt.BorderLayout
import java.awt.Color
import java.awt.Component
import java.awt.Dimension
import java.awt.image.BufferedImage
import javax.swing.*
import kotlin.math.max

class FilterSelectionComboBox() : JComboBox<FilterDTO>() {
    init {
        setup()
    }

    constructor(model: DefaultComboBoxModel<FilterDTO>) : this() {
        setModel(model)
    }

    private fun setup() {
        maximumSize = Dimension(200, 100)
        isEditable = false
        toolTipText = "Aktiver Filter"
        renderer = FilterSelectionRenderer()
    }

    private class FilterSelectionRenderer : JPanel(BorderLayout(8, 0)), ListCellRenderer<FilterDTO> {
        private val textLabel = JLabel()
        private val iconLabel = JLabel()

        init {
            isOpaque = true
            border = BorderFactory.createEmptyBorder(0, 4, 0, 4)
            textLabel.border = BorderFactory.createEmptyBorder(0, 0, 0, 0)
            iconLabel.border = BorderFactory.createEmptyBorder(0, 0, 0, 2)
            add(textLabel, BorderLayout.CENTER)
            add(iconLabel, BorderLayout.EAST)
        }

        override fun getListCellRendererComponent(
            list: JList<out FilterDTO>?,
            value: FilterDTO?,
            index: Int,
            isSelected: Boolean,
            cellHasFocus: Boolean,
        ): Component {
            val bg = if (isSelected && list != null) list.selectionBackground else list?.background ?: background
            val fg = if (isSelected && list != null) list.selectionForeground else list?.foreground ?: foreground

            background = bg
            foreground = fg
            textLabel.background = bg
            textLabel.foreground = fg
            iconLabel.background = bg
            iconLabel.foreground = fg
            textLabel.font = list?.font ?: font
            textLabel.text = value?.name ?: ""

            val model = (list?.model as? FilterSelectionComboBoxModel)
            val isLocked = model?.isFilterLocked(value) == true
            iconLabel.icon = if (isLocked) {
                createLockedIcon(isSelected)
            } else {
                null
            }
            iconLabel.isVisible = isLocked

            return this
        }

        private fun createLockedIcon(isSelected: Boolean): Icon {
            val iconSize = max(textLabel.font.size, 1)
            val sourceIcon = if (isSelected) {
                FontIcon.of(FontAwesomeSolid.LOCK, iconSize, Color.WHITE)
            } else {
                IconUtils.of(FontAwesomeSolid.LOCK, iconSize)
            }

            val image = BufferedImage(iconSize, iconSize, BufferedImage.TYPE_INT_ARGB)
            val graphics = image.createGraphics()
            try {
                val x = (iconSize - sourceIcon.iconWidth) / 2
                val y = (iconSize - sourceIcon.iconHeight) / 2
                sourceIcon.paintIcon(this, graphics, x, y)
            } finally {
                graphics.dispose()
            }

            return ImageIcon(image)
        }
    }
}
