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

package mediathek.logging

import java.awt.BorderLayout
import javax.swing.JDialog
import javax.swing.JFrame

class LogDialog(parent: JFrame) : JDialog(parent) {
    init {
        defaultCloseOperation = HIDE_ON_CLOSE
        title = "Programm-Log"
        layout = BorderLayout()
        add(LogPanel(), BorderLayout.CENTER)
        setSize(800, 400)
    }
}
