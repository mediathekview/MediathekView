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

package mediathek.gui.tabs.tab_film.context

import mediathek.daten.DatenFilm
import mediathek.daten.FilmResolution
import mediathek.gui.actions.UrlHyperlinkAction
import mediathek.gui.tabs.tab_film.JDownloadHelper
import mediathek.gui.tabs.tab_film.OnlineSearchProviders
import mediathek.gui.tabs.tab_film.PyLoadHelper
import mediathek.tool.GuiFunktionen
import java.awt.event.ActionListener
import java.awt.event.KeyEvent
import java.net.URLEncoder
import java.nio.charset.StandardCharsets
import javax.swing.JMenu
import javax.swing.JMenuItem
import javax.swing.JPopupMenu
import javax.swing.KeyStroke

class FilmSpecificContextMenuBuilder(
    private val host: TableContextMenuHandler.Host,
    private val jDownloadHelper: JDownloadHelper,
    private val pyLoadHelper: PyLoadHelper,
) {
    fun addFilmSpecificContextActions(popupMenu: JPopupMenu, film: DatenFilm) {
        popupMenu.addSeparator()
        jDownloadHelper.installContextMenu(film, popupMenu)
        popupMenu.addSeparator()
        pyLoadHelper.installContextMenu(film, popupMenu)
        popupMenu.addSeparator()
        setupCopyToClipboardContextMenu(film, popupMenu)
        popupMenu.addSeparator()
        setupSearchEntries(popupMenu, film)

        if (film.hasSubtitle()) {
            popupMenu.add(host.actions().downloadSubtitle)
            popupMenu.addSeparator()
        }
    }

    private fun setupCopyToClipboardContextMenu(film: DatenFilm, popupMenu: JPopupMenu) {
        val copyToClipboardMenu = JMenu("In Zwischenablage kopieren")

        JMenuItem("Titel").also {
            it.addActionListener { GuiFunktionen.copyToClipboard(film.title) }
            copyToClipboardMenu.add(it)
        }

        JMenuItem("Thema").also {
            it.addActionListener { GuiFunktionen.copyToClipboard(film.thema) }
            copyToClipboardMenu.add(it)
        }

        JMenuItem("Thema - Titel").also {
            it.addActionListener { GuiFunktionen.copyToClipboard("${film.thema} - ${film.title}") }
            copyToClipboardMenu.add(it)
        }

        JMenuItem("Sender - Thema - Titel").also {
            it.addActionListener { GuiFunktionen.copyToClipboard("${film.sender} - ${film.thema} - ${film.title}") }
            copyToClipboardMenu.add(it)
        }

        JMenuItem("Beschreibung").also {
            it.addActionListener { GuiFunktionen.copyToClipboard(film.description) }
            copyToClipboardMenu.add(it)
        }

        setupFilmUrlCopyToClipboardEntries(copyToClipboardMenu, film)
        popupMenu.add(copyToClipboardMenu)
    }

    private fun setupFilmUrlCopyToClipboardEntries(parentMenu: JMenu, film: DatenFilm) {
        parentMenu.addSeparator()

        val normalUrl = film.getUrlFuerAufloesung(FilmResolution.Enum.NORMAL)
        var highQualityUrl = film.getUrlFuerAufloesung(FilmResolution.Enum.HIGH_QUALITY)
        var lowQualityUrl = film.getUrlFuerAufloesung(FilmResolution.Enum.LOW)
        if (highQualityUrl == normalUrl) {
            highQualityUrl = ""
        }
        if (lowQualityUrl == normalUrl) {
            lowQualityUrl = ""
        }

        if (normalUrl.isNotEmpty()) {
            val copyNormalUrlListener = ActionListener { GuiFunktionen.copyToClipboard(normalUrl) }
            if (highQualityUrl.isNotEmpty() || lowQualityUrl.isNotEmpty()) {
                val submenuUrl = JMenu("Film-URL")
                if (highQualityUrl.isNotEmpty()) {
                    JMenuItem("höchste/hohe Qualität").also {
                        it.accelerator = KeyStroke.getKeyStroke(
                            KeyEvent.VK_H,
                            GuiFunktionen.getPlatformControlKey() or KeyEvent.SHIFT_DOWN_MASK or KeyEvent.ALT_DOWN_MASK,
                        )
                        it.addActionListener {
                            GuiFunktionen.copyToClipboard(film.getUrlFuerAufloesung(FilmResolution.Enum.HIGH_QUALITY))
                        }
                        submenuUrl.add(it)
                    }
                }

                JMenuItem("mittlere Qualität").also {
                    it.addActionListener(copyNormalUrlListener)
                    it.accelerator = KeyStroke.getKeyStroke(
                        KeyEvent.VK_N,
                        GuiFunktionen.getPlatformControlKey() or KeyEvent.SHIFT_DOWN_MASK or KeyEvent.ALT_DOWN_MASK,
                    )
                    submenuUrl.add(it)
                }

                if (lowQualityUrl.isNotEmpty()) {
                    JMenuItem("niedrige Qualität").also {
                        it.addActionListener {
                            GuiFunktionen.copyToClipboard(film.getUrlFuerAufloesung(FilmResolution.Enum.LOW))
                        }
                        submenuUrl.add(it)
                    }
                }
                parentMenu.add(submenuUrl)
            } else {
                JMenuItem("Verfügbare URL").also {
                    it.addActionListener(copyNormalUrlListener)
                    parentMenu.add(it)
                }
            }
        }

        if (film.subtitleUrl.isNotEmpty()) {
            JMenuItem("Untertitel-URL").also {
                it.addActionListener { GuiFunktionen.copyToClipboard(film.subtitleUrl) }
                parentMenu.add(it)
            }
        }
    }

    private fun setupSearchEntries(popupMenu: JPopupMenu, film: DatenFilm) {
        val onlineSearchMenu = JMenu("Online-Suche nach")
        val themaMenu = JMenu("Thema")
        val titelMenu = JMenu("Titel")

        for (provider in OnlineSearchProviders.entries) {
            if (!film.isLivestream) {
                JMenuItem(provider.toString()).also {
                    it.addActionListener {
                        val url = provider.queryUrl + URLEncoder.encode(film.thema, StandardCharsets.UTF_8)
                        UrlHyperlinkAction.openURL(url)
                    }
                    themaMenu.add(it)
                }
            }

            JMenuItem(provider.toString()).also {
                it.addActionListener {
                    val url = provider.queryUrl + URLEncoder.encode(film.title, StandardCharsets.UTF_8)
                    UrlHyperlinkAction.openURL(url)
                }
                titelMenu.add(it)
            }
        }

        if (!film.isLivestream) {
            onlineSearchMenu.add(themaMenu)
        }
        onlineSearchMenu.add(titelMenu)
        popupMenu.add(onlineSearchMenu)
        popupMenu.addSeparator()
    }
}
