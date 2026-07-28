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
import org.pushingpixels.radiance.swing.ktx.swing.KeyboardActionScopeType
import org.pushingpixels.radiance.swing.ktx.swing.wireActionToKeyStroke
import java.awt.Toolkit
import java.awt.event.KeyEvent
import javax.swing.JDialog
import javax.swing.JFrame
import javax.swing.JRootPane
import javax.swing.KeyStroke

object EscapeKeyHandler {
    private const val CANCEL_KEY_HANDLER = "key_cancel"

    fun installHandler(dialog: JDialog, action: Runnable) {
        installHandler(dialog.rootPane, action)
    }

    fun installHandler(frame: JFrame, action: Runnable) {
        installHandler(frame.rootPane, action)
    }

    private fun installHandler(rootPane: JRootPane, action: Runnable) {
        rootPane.wireActionToKeyStroke(
            CANCEL_KEY_HANDLER,
            KeyStroke.getKeyStroke(KeyEvent.VK_ESCAPE, 0),
            KeyboardActionScopeType.WHEN_IN_FOCUSED_WINDOW_TYPE
        )
        { action.run() }
        if (SystemUtils.IS_OS_MAC_OSX) {
            rootPane.wireActionToKeyStroke(
                CANCEL_KEY_HANDLER,
                KeyStroke.getKeyStroke(KeyEvent.VK_W, Toolkit.getDefaultToolkit().menuShortcutKeyMaskEx),
                KeyboardActionScopeType.WHEN_IN_FOCUSED_WINDOW_TYPE
            )
            { action.run() }
        }
    }
}
