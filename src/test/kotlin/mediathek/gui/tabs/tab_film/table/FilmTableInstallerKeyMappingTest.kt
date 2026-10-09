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

package mediathek.gui.tabs.tab_film.table

import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Assertions.assertNull
import org.junit.jupiter.api.Test
import java.awt.event.KeyEvent
import javax.swing.JComponent
import javax.swing.JTable
import javax.swing.KeyStroke

internal class FilmTableInstallerKeyMappingTest {
    @Test
    fun `modified film URL shortcuts are limited to the focused film table`() {
        val table = JTable()
        val focusedInputMap = table.getInputMap(JComponent.WHEN_FOCUSED)
        val modifiers = KeyEvent.CTRL_DOWN_MASK or KeyEvent.SHIFT_DOWN_MASK or KeyEvent.ALT_DOWN_MASK

        installFilmUrlCopyAccelerators(focusedInputMap, KeyEvent.CTRL_DOWN_MASK)

        assertEquals("copy_url_hd", focusedInputMap[KeyStroke.getKeyStroke(KeyEvent.VK_H, modifiers)])
        assertEquals("copy_url", focusedInputMap[KeyStroke.getKeyStroke(KeyEvent.VK_N, modifiers)])
        val windowInputMap = table.getInputMap(JComponent.WHEN_IN_FOCUSED_WINDOW)
        assertNull(windowInputMap[KeyStroke.getKeyStroke(KeyEvent.VK_H, modifiers)])
        assertNull(windowInputMap[KeyStroke.getKeyStroke(KeyEvent.VK_N, modifiers)])
    }
}
