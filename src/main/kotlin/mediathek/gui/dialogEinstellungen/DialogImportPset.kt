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

package mediathek.gui.dialogEinstellungen

import mediathek.daten.DatenPset
import mediathek.daten.ListePset
import mediathek.daten.ProgramSetRepository
import mediathek.gui.dialogEinstellungen.pset.PanelPsetKurz
import mediathek.gui.dialogEinstellungen.pset.PanelPsetLang
import mediathek.tool.EscapeKeyHandler
import java.util.function.BiConsumer
import javax.swing.JFrame

class DialogImportPset(
    private val parentComponent: JFrame?,
    modal: Boolean,
    private val programSets: ProgramSetRepository,
    private val liste: ListePset,
    private val programSetExporter: BiConsumer<Array<DatenPset>, String>,
) : DialogImportPsetBase(parentComponent, modal) {
    var ok: Boolean = false
        private set

    init {
        title = "Programmset"
        jScrollPane1.setViewportView(PanelPsetKurz(parentComponent, liste))
        jButtonOk.addActionListener { disposeWithCode(true) }
        jButtonAbbrechen.addActionListener { disposeWithCode(false) }

        EscapeKeyHandler.installHandler(this) { disposeWithCode(false) }

        jCheckBoxAlleEinstellungen.addActionListener {
            if (jCheckBoxAlleEinstellungen.isSelected) {
                jScrollPane1.setViewportView(PanelPsetLang(parentComponent, programSets, liste, programSetExporter))
            } else {
                jScrollPane1.setViewportView(PanelPsetKurz(parentComponent, liste))
            }
        }
    }

    private fun disposeWithCode(ok: Boolean) {
        this.ok = ok
        dispose()
    }
}
