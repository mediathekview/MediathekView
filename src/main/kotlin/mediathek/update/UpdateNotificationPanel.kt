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

package mediathek.update

import mediathek.config.Konstanten
import org.apache.logging.log4j.LogManager
import java.io.IOException
import javax.swing.JEditorPane
import javax.swing.JLabel

class UpdateNotificationPanel : UpdateNotificationPanelBase() {
    val releaseInfoLabel: JLabel
        get() = lblReleaseInfo

    init {
        initComponents()
    }

    override fun createUIComponents() {
        try {
            webView = JEditorPane(requireNotNull(Konstanten.WEBSITE_BASE_URL.resolve("changelogs")).toString())
        } catch (e: IOException) {
            logger.error("Failed to load changelog from web")
            webView = JEditorPane().apply {
                text = "<html><body>Load failed!</body></html>"
            }
        }
    }

    private companion object {
        private val logger = LogManager.getLogger()
    }
}
