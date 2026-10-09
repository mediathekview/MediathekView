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

import mediathek.config.application.ApplicationConfiguration
import mediathek.daten.ProgramSetRepository
import mediathek.daten.abo.AboServices
import mediathek.daten.abo.DatenAbo
import mediathek.daten.abo.FilmLengthState
import mediathek.filmlisten.FilmCatalog
import mediathek.gui.dialog.DialogEditAbo
import mediathek.tool.FilenameUtils
import mediathek.tool.SVGIconUtilities
import java.awt.event.ActionEvent
import javax.swing.AbstractAction
import javax.swing.JFrame
import javax.swing.JOptionPane

class CreateNewAboAction(
    private val programSets: ProgramSetRepository,
    private val filmCatalog: FilmCatalog,
    private val abos: AboServices,
    private val parentProvider: () -> JFrame,
    private val ensureAboProgramSetAvailable: (JFrame) -> Boolean,
) : AbstractAction() {
    override fun actionPerformed(e: ActionEvent?) {
        createAbo()
    }

    fun createAbo(
        aboname: String = "Neu",
        filmSender: String = "",
        filmThema: String = "",
        filmTitel: String = "",
    ) {
        val parent = parentProvider()
        val datenAbo = createAboDraft(aboname, filmSender, filmThema, filmTitel)

        if (!ensureAboProgramSetAvailable(parent)) {
            return
        }

        val dialogEditAbo = DialogEditAbo(parent, programSets, filmCatalog, abos, datenAbo, false)
        dialogEditAbo.isVisible = true
        if (!dialogEditAbo.successful()) {
            return
        }

        if (abos.list.existsAlready(datenAbo)) {
            JOptionPane.showMessageDialog(
                parent,
                "Abo existiert bereits",
                "Abo anlegen",
                JOptionPane.INFORMATION_MESSAGE,
            )
            return
        }

        ApplicationConfiguration.getInstance().defaultAboMinimumDurationMinutes = datenAbo.mindestDauerMinuten
        abos.list.addAbo(datenAbo)
    }

    private fun createAboDraft(
        aboname: String,
        filmSender: String,
        filmThema: String,
        filmTitel: String,
    ): DatenAbo {
        val applicationConfiguration = ApplicationConfiguration.getInstance()
        val sanitizedAboName = FilenameUtils.replaceLeerDateiname(
            aboname,
            false,
            applicationConfiguration.useFilenameReplaceTable,
            applicationConfiguration.onlyAsciiFilenames,
        )

        return DatenAbo().apply {
            name = sanitizedAboName
            sender = filmSender
            thema = filmThema
            title = filmTitel
            themaTitel = ""
            irgendwo = ""
            mindestDauerMinuten = ApplicationConfiguration.getInstance().defaultAboMinimumDurationMinutes
            filmLengthState = FilmLengthState.MINIMUM
            zielpfad = sanitizedAboName
            psetName = ""
        }
    }

    init {
        putValue(NAME, "Abo anlegen...")
        putValue(SHORT_DESCRIPTION, "Abo anlegen")
        putValue(SMALL_ICON, SVGIconUtilities.createSVGIcon("icons/fontawesome/plus.svg"))
    }
}
