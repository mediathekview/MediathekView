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
import mediathek.mainwindow.MainWindowHandle
import mediathek.tool.SVGIconUtilities
import mediathek.tool.SwingErrorDialog
import java.awt.Desktop
import java.awt.event.ActionEvent
import java.io.IOException
import java.net.URI
import java.net.URISyntaxException
import javax.swing.AbstractAction

class ShowOnlineHelpAction(
    private val owner: MainWindowHandle,
) : AbstractAction() {
    init {
        putValue(NAME, "Online-Hilfe anzeigen...")
        putValue(SMALL_ICON, SVGIconUtilities.createSVGIcon("icons/fontawesome/circle-question.svg"))
    }

    private fun openUrl() {
        UrlHyperlinkAction.openURL(Konstanten.ADRESSE_ONLINE_HELP)
    }

    override fun actionPerformed(event: ActionEvent?) {
        if (!Desktop.isDesktopSupported()) {
            openUrl()
            return
        }

        val desktop = Desktop.getDesktop()
        if (!desktop.isSupported(Desktop.Action.BROWSE)) {
            openUrl()
            return
        }

        try {
            desktop.browse(URI(Konstanten.ADRESSE_ONLINE_HELP))
        } catch (ex: IOException) {
            showError(ex)
        } catch (ex: URISyntaxException) {
            showError(ex)
        }
    }

    private fun showError(ex: Exception) {
        SwingErrorDialog.showExceptionMessage(
            owner.ownerFrame(),
            "Es trat ein Fehler beim Öffnen der Online-Hilfe auf.\nSollte dies häufiger auftreten kontaktieren Sie bitte das Entwicklerteam.",
            ex,
        )
    }
}
