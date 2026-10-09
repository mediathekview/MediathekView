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

import org.kordamp.ikonli.swing.FontIcon
import java.awt.Graphics
import java.awt.Rectangle
import javax.swing.JCheckBoxMenuItem
import javax.swing.JMenuItem
import javax.swing.plaf.basic.BasicCheckBoxMenuItemUI

class IconizedCheckBoxMenuItem(itemIcon: FontIcon) : JCheckBoxMenuItem() {
    private val iconizedUi = IconizedCheckBoxMenuItemUI(itemIcon)

    constructor(icon: FontIcon, state: Boolean) : this(icon) {
        this.state = state
    }

    init {
        text = "dummy"
        this.ui = iconizedUi
    }

    override fun updateUI() {
        this.ui = iconizedUi
    }

    private class IconizedCheckBoxMenuItemUI(
        private val icon: FontIcon,
    ) : BasicCheckBoxMenuItemUI() {
        override fun paintText(graphics: Graphics, menuItem: JMenuItem, textRect: Rectangle, text: String) {
            icon.paintIcon(null, graphics, textRect.x, textRect.y)
        }
    }
}
