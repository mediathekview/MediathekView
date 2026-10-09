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

import org.apache.commons.lang3.SystemUtils
import java.awt.Toolkit
import java.awt.event.ActionEvent
import java.awt.event.KeyEvent
import javax.swing.*

object EscapeKeyHandler {
    private const val CANCEL_KEY_HANDLER = "key_cancel"

    fun installHandler(dialog: JDialog, action: Runnable) {
        installHandler(dialog.rootPane, action)
    }

    fun installHandler(frame: JFrame, action: Runnable) {
        installHandler(frame.rootPane, action)
    }

    private fun installHandler(rootPane: JRootPane, action: Runnable) {
        val inputMap = rootPane.getInputMap(JComponent.WHEN_IN_FOCUSED_WINDOW)
        inputMap.put(KeyStroke.getKeyStroke(KeyEvent.VK_ESCAPE, 0), CANCEL_KEY_HANDLER)
        if (SystemUtils.IS_OS_MAC_OSX) {
            inputMap.put(
                KeyStroke.getKeyStroke(KeyEvent.VK_W, Toolkit.getDefaultToolkit().menuShortcutKeyMaskEx),
                CANCEL_KEY_HANDLER,
            )
        }
        rootPane.actionMap.put(
            CANCEL_KEY_HANDLER,
            object : AbstractAction() {
                override fun actionPerformed(e: ActionEvent) {
                    action.run()
                }
            },
        )
    }
}
