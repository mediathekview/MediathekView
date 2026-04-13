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

package mediathek.tool.table

import mediathek.audiothek.ui.table.TriStateTableRowSorter
import mediathek.config.Daten
import mediathek.config.MVConfig
import mediathek.daten.abo.DatenAbo
import mediathek.tool.models.TModelAbo
import java.util.*
import javax.swing.table.TableModel

class MVAbosTable : PersistentColumnConfigurationTable(
    DatenAbo.MAX_ELEM,
    DatenAbo.getColumnVisibilityStore(),
    Optional.of(MVConfig.Configs.SYSTEM_TAB_ABO_ICON_ANZEIGEN),
    Optional.of(MVConfig.Configs.SYSTEM_TAB_ABO_ICON_KLEIN),
    MVConfig.Configs.SYSTEM_EIGENSCHAFTEN_TABELLE_ABOS,
) {
    init {
        model = TModelAbo(Daten.getInstance().listeAbo)
    }

    override fun setModel(dataModel: TableModel) {
        super.setModel(dataModel)

        if (dataModel is TModelAbo) {
            autoCreateRowSorter = false
            rowSorter = TriStateTableRowSorter(dataModel)
        }
    }

    override fun resetTabelle() {
        for (i in 0 until maxSpalten) {
            resetAbosTab(i)
        }

        super.resetTabelle()
        autoCreateRowSorter = false
        rowSorter = TriStateTableRowSorter(model)
    }

    private fun resetAbosTab(i: Int) {
        reihe[i] = i
        breite[i] = 200
        if (i == DatenAbo.ABO_NR ||
            i == DatenAbo.ABO_EINGESCHALTET ||
            i == DatenAbo.ABO_MIN ||
            i == DatenAbo.ABO_DO_NOT_START_AUTOMATICALLY
        ) {
            breite[i] = 75
        } else if (i == DatenAbo.ABO_DOWN_DATUM || i == DatenAbo.ABO_SENDER) {
            breite[i] = 100
        }
    }

    private fun spaltenAusschaltenAbos(i: Int) {
        if (i == DatenAbo.ABO_ZIELPFAD ||
            i == DatenAbo.ABO_PSET ||
            i == DatenAbo.ABO_MINDESTDAUER ||
            i == DatenAbo.ABO_MIN ||
            i == DatenAbo.ABO_DOWN_DATUM ||
            i == DatenAbo.ABO_REF
        ) {
            breite[i] = 0
        }
    }

    override fun spaltenAusschalten() {
        for (i in 0 until maxSpalten) {
            spaltenAusschaltenAbos(i)
        }
    }
}
