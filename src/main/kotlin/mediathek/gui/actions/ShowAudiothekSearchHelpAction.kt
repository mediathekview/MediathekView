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

import mediathek.config.Konstanten
import mediathek.gui.dialog.lucene_tutorial.LuceneTutorialDialog
import mediathek.tool.SVGIconUtilities
import org.pushingpixels.radiance.swing.ktx.addDelayedWindowListener
import java.awt.KeyboardFocusManager
import java.awt.event.ActionEvent
import javax.swing.AbstractAction

class ShowAudiothekSearchHelpAction : AbstractAction() {
    init {
        putValue(NAME, "Audiothek-Suche anzeigen...")
        putValue(SHORT_DESCRIPTION, "Hilfe zur Audiothek-Suche")
        putValue(SMALL_ICON, SVGIconUtilities.createSVGIcon("icons/fontawesome/circle-question.svg"))
    }

    override fun actionPerformed(event: ActionEvent?) {
        val owner = KeyboardFocusManager.getCurrentKeyboardFocusManager().activeWindow
        val dialog = LuceneTutorialDialog(owner, "Audiothek-Suche", Konstanten.PFAD_AUDIOTHEK_SUCHE_HILFE_MARKDOWN)
        dialog.addDelayedWindowListener(onWindowClosed = {
            isEnabled = true
        })
        isEnabled = false
        dialog.isVisible = true
    }
}
