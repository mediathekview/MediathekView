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

import mediathek.config.Daten
import mediathek.config.application.ApplicationConfiguration
import mediathek.daten.DatenFilm
import mediathek.daten.blacklist.BlacklistRule
import mediathek.gui.actions.CreateNewAboAction
import java.awt.event.ActionEvent
import java.awt.event.ActionListener
import java.util.*
import javax.swing.JMenu
import javax.swing.JMenuItem
import javax.swing.JPopupMenu

class FilmAboAndBlacklistContextActions(
    private val host: TableContextMenuHandler.Host,
    private val daten: Daten,
    private val selectedFilmAtPopupPoint: () -> DatenFilm?,
) {
    private val createAboAction = CreateNewAboAction(daten.listeAbo) { host.gui() }
    private val aboWithoutTitleAction = AboActionListener(false)
    private val aboWithTitleAction = AboActionListener(true)

    fun addAboMenu(popupMenu: JPopupMenu, selectedFilm: Optional<DatenFilm>) {
        val submenuAbo = JMenu("Abo")
        popupMenu.add(submenuAbo)

        val itemAbo = JMenuItem("Abo mit Sender und Thema anlegen")
        val itemAboMitTitel = JMenuItem("Abo mit Sender und Thema und Titel anlegen")

        selectedFilm.ifPresent { film -> configureAboMenuItems(film, itemAbo, itemAboMitTitel) }

        submenuAbo.add(itemAbo)
        submenuAbo.add(itemAboMitTitel)
    }

    fun addBlacklistMenu(popupMenu: JPopupMenu) {
        val submenuBlack = JMenu("Blacklist")
        popupMenu.add(submenuBlack)

        val itemBlackSender = JMenuItem("Sender in die Blacklist einfügen")
        itemBlackSender.addActionListener {
            addBlacklistRuleForSelectedFilm { film ->
                daten.listeBlacklist.add(BlacklistRule(film.sender, "", "", ""))
            }
        }
        submenuBlack.add(itemBlackSender)

        val itemBlackThema = JMenuItem("Thema in die Blacklist einfügen")
        itemBlackThema.addActionListener {
            addBlacklistRuleForSelectedFilm { film ->
                daten.listeBlacklist.add(BlacklistRule("", film.thema, "", ""))
            }
        }
        submenuBlack.add(itemBlackThema)

        val itemAddTitleToBlacklist = JMenuItem("Titel in die Blacklist einfügen")
        itemAddTitleToBlacklist.addActionListener {
            addBlacklistRuleForSelectedFilm { film ->
                daten.listeBlacklist.add(BlacklistRule("", "", film.title, ""))
            }
        }
        submenuBlack.add(itemAddTitleToBlacklist)

        val itemBlackSenderThema = JMenuItem("Sender und Thema in die Blacklist einfügen")
        itemBlackSenderThema.addActionListener {
            addBlacklistRuleForSelectedFilm { film ->
                daten.listeBlacklist.add(BlacklistRule(film.sender, film.thema, "", ""))
            }
        }
        submenuBlack.add(itemBlackSenderThema)
    }

    private fun configureAboMenuItems(
        film: DatenFilm,
        itemAbo: JMenuItem,
        itemAboMitTitel: JMenuItem,
    ) {
        if (daten.listeAbo.getAboFuerFilm_schnell(film, false) != null) {
            itemAbo.isEnabled = false
            itemAboMitTitel.isEnabled = false
        } else {
            itemAbo.addActionListener(aboWithoutTitleAction)
            itemAboMitTitel.addActionListener(aboWithTitleAction)
        }
    }

    private inner class AboActionListener(
        private val mitTitel: Boolean,
    ) : ActionListener {
        override fun actionPerformed(event: ActionEvent?) {
            selectedFilmAtPopupPoint()?.let { film ->
                host.setSelectionUpdatesSuspended(true)
                try {
                    val datenAbo = daten.listeAbo.getAboFuerFilm_schnell(film, false)
                    if (datenAbo != null) {
                        daten.listeAbo.aboLoeschen(datenAbo)
                    } else {
                        createAboAction.createAbo(
                            aboname = film.thema,
                            filmSender = film.sender,
                            filmThema = film.thema,
                            filmTitel = if (mitTitel) film.title else "",
                        )
                    }
                } finally {
                    host.setSelectionUpdatesSuspended(false)
                }
            }
        }
    }

    private fun turnOnBlacklist() {
        ApplicationConfiguration.getInstance().isBlacklistEnabled = true
    }

    private fun addBlacklistRuleForSelectedFilm(blacklistRuleAppender: (DatenFilm) -> Unit) {
        selectedFilmAtPopupPoint()?.let { film ->
            turnOnBlacklist()
            blacklistRuleAppender(film)
        }
    }
}
