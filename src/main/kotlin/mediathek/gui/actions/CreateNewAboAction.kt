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

import mediathek.config.MVConfig
import mediathek.daten.ListeAbo
import mediathek.daten.abo.DatenAbo
import mediathek.daten.abo.FilmLengthState
import mediathek.gui.dialog.DialogAboNoSet
import mediathek.gui.dialog.DialogEditAbo
import mediathek.mainwindow.MediathekGui
import mediathek.tool.FilenameUtils
import mediathek.tool.MVMessageDialog
import mediathek.tool.SVGIconUtilities
import java.awt.event.ActionEvent
import java.util.*
import javax.swing.AbstractAction
import javax.swing.JFrame
import javax.swing.JOptionPane

class CreateNewAboAction @JvmOverloads constructor(
    private val listeAbo: ListeAbo,
    private val parentProvider: () -> JFrame = { MediathekGui.ui() },
) : AbstractAction() {
    override fun actionPerformed(e: ActionEvent?) {
        createAbo()
    }

    @JvmOverloads
    fun createAbo(
        aboname: String = "Neu",
        filmSender: String = "",
        filmThema: String = "",
        filmTitel: String = "",
    ) {
        val parent = parentProvider()
        val datenAbo = createAboDraft(aboname, filmSender, filmThema, filmTitel)

        if (!DialogAboNoSet.ensureAboProgramSetAvailable(parent)) {
            return
        }

        val dialogEditAbo = DialogEditAbo(parent, datenAbo, false)
        dialogEditAbo.isVisible = true
        if (!dialogEditAbo.successful()) {
            return
        }

        if (listeAbo.existsAlready(datenAbo)) {
            MVMessageDialog.showMessageDialog(
                parent,
                "Abo existiert bereits",
                "Abo anlegen",
                JOptionPane.INFORMATION_MESSAGE,
            )
            return
        }

        MVConfig.add(MVConfig.Configs.SYSTEM_ABO_MIN_SIZE, datenAbo.mindestDauerMinuten.toString())
        listeAbo.addAbo(datenAbo)
        Collections.sort(listeAbo)
        listeAbo.aenderungMelden()
    }

    private fun createAboDraft(
        aboname: String,
        filmSender: String,
        filmThema: String,
        filmTitel: String,
    ): DatenAbo {
        val sanitizedAboName = FilenameUtils.replaceLeerDateiname(
            aboname,
            false,
            MVConfig.get(MVConfig.Configs.SYSTEM_USE_REPLACETABLE).toBoolean(),
            MVConfig.get(MVConfig.Configs.SYSTEM_ONLY_ASCII).toBoolean(),
        )

        return DatenAbo().apply {
            name = sanitizedAboName
            sender = filmSender
            thema = filmThema
            title = filmTitel
            themaTitel = ""
            irgendwo = ""
            mindestDauerMinuten = parseMinSize()
            filmLengthState = FilmLengthState.MINIMUM
            zielpfad = sanitizedAboName
            psetName = ""
        }
    }

    private fun parseMinSize(): Int {
        return runCatching {
            MVConfig.get(MVConfig.Configs.SYSTEM_ABO_MIN_SIZE).toInt()
        }.getOrElse {
            MVConfig.add(MVConfig.Configs.SYSTEM_ABO_MIN_SIZE, "0")
            0
        }
    }

    init {
        putValue(NAME, "Abo anlegen...")
        putValue(SHORT_DESCRIPTION, "Abo anlegen")
        putValue(SMALL_ICON, SVGIconUtilities.createSVGIcon("icons/fontawesome/plus.svg"))
    }
}
