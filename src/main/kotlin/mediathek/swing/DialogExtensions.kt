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

package mediathek.swing

import java.awt.Toolkit
import javax.swing.JDialog

/**
 * Places the dialog on the primary screen.
 *
 * The default keeps the legacy MediathekView behavior: dialogs are placed at half of the calculated
 * centered coordinates, resulting in a quarter-screen offset. Pass `absolute = true` for true screen center.
 */
fun JDialog.centerOnScreen(absolute: Boolean = false) {
    val screenSize = Toolkit.getDefaultToolkit().screenSize
    var x = (screenSize.width / 2) - (width / 2)
    var y = (screenSize.height / 2) - (height / 2)
    if (!absolute) {
        x /= 2
        y /= 2
    }
    setLocation(x, y)
}
