/*
 * Copyright (c) 2024 derreisende77.
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

import mediathek.gui.dialog.subripmerge.MergeSubripVideoDialog
import mediathek.mainwindow.MediathekGui
import java.awt.event.ActionEvent
import javax.swing.AbstractAction

class MergeSubtitleWithVideoAction(
    private val ui: MediathekGui,
) : AbstractAction() {
    init {
        putValue(NAME, "Untertiteldatei zu Video hinzufügen...")
    }

    override fun actionPerformed(event: ActionEvent?) {
        MergeSubripVideoDialog(ui).isVisible = true
    }
}
