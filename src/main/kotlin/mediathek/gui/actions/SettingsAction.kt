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

package mediathek.gui.actions

import mediathek.gui.messages.ShowSettingsDialogEvent
import mediathek.swing.IconUtils
import mediathek.tool.MessageBus
import org.apache.commons.lang3.SystemUtils
import org.kordamp.ikonli.fontawesome6.FontAwesomeSolid
import java.awt.event.ActionEvent
import java.awt.event.KeyEvent
import javax.swing.AbstractAction
import javax.swing.KeyStroke

class SettingsAction : AbstractAction() {
    init {
        putValue(NAME, "Einstellungen...")
        if (!SystemUtils.IS_OS_MAC_OSX) {
            putValue(ACCELERATOR_KEY, KeyStroke.getKeyStroke(KeyEvent.VK_F4, 0))
        }
        putValue(SMALL_ICON, IconUtils.windowBarSpecificToolbarIcon(FontAwesomeSolid.COGS))
        putValue(SHORT_DESCRIPTION, "Einstellungen öffnen")
    }

    override fun actionPerformed(event: ActionEvent?) {
        MessageBus.messageBus.publishAsync(ShowSettingsDialogEvent())
    }
}
