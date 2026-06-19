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

package mediathek.gui.dialog.reset

import mediathek.config.Daten
import mediathek.config.Konstanten
import mediathek.daten.ListePset
import mediathek.daten.ListePsetVorlagen
import mediathek.gui.dialog.DialogHilfe
import mediathek.mainwindow.SettingsResetHost
import mediathek.tool.GetFile
import mediathek.tool.GuiFunktionenProgramme
import mediathek.tool.SVGIconUtilities
import javax.swing.JOptionPane

class ResetSettingsPanel(
    private val host: SettingsResetHost,
) : ResetSettingsPanelBase() {
    private val parent = host.ownerFrame()

    init {
        jButtonHilfeReset.icon = SVGIconUtilities.createSVGIcon("icons/fontawesome/circle-question.svg")
        jButtonHilfeReset.addActionListener {
            DialogHilfe(parent, true, GetFile.getHilfeSuchen(Konstanten.PFAD_HILFETEXT_RESET)).isVisible = true
        }
        jButtonResetSets.addActionListener {
            val listePset = Daten.getInstance().listePset
            val previousPsets = ListePset()
            previousPsets.addAll(listePset)

            listePset.clear()
            if (!GuiFunktionenProgramme.addSetVorlagen(
                    parent,
                    Daten.getInstance(),
                    ListePsetVorlagen.getStandarset(parent, true),
                    true,
                )
            ) {
                listePset.clear()
                listePset.addAll(previousPsets)
            }
        }
        jButtonResetAll.addActionListener {
            val ret = JOptionPane.showConfirmDialog(
                parent,
                RESET_MESSAGE,
                "Einstellungen zurücksetzen",
                JOptionPane.YES_NO_OPTION,
            )
            if (ret == JOptionPane.OK_OPTION) {
                // damit wird vor dem Beenden das Konfig-Verzeichnis umbenannt und so startet das
                // Programm wie beim ersten Start
                host.requestSettingsResetOnQuit()
                host.quitApplication()
            }
        }
    }

    private companion object {
        private const val RESET_MESSAGE = "<html>Es werden <b>ALLE</b> von Ihnen erzeugten Änderungen gelöscht.<br>" +
            "Möchten Sie wirklich alle Einstellungen zurücksetzen?<br></html>"
    }
}
