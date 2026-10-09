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

package mediathek.gui.dialog

import mediathek.config.Konstanten
import mediathek.daten.ListePset
import mediathek.daten.ListePsetVorlagen
import mediathek.daten.ProgramSetRepository
import javax.swing.JFrame
import javax.swing.JLabel
import javax.swing.JOptionPane

object MissingProgramSetDialog {
    private const val IMPORT_OPTION_INDEX = 0
    private const val CLOSE_OPTION_INDEX = 1

    private val options = arrayOf("Standardsets importieren", "Schließen")

    fun ensureAboProgramSetAvailable(
        parent: JFrame?,
        programSets: ProgramSetRepository,
        importStandardProgramSets: (JFrame?, ListePset?) -> Unit,
    ): Boolean {
        if (hasAboProgramSet(programSets)) {
            return true
        }

        showMissingAboProgramSet(parent, programSets, importStandardProgramSets)
        return hasAboProgramSet(programSets)
    }

    fun showMissingAboProgramSet(
        parent: JFrame?,
        programSets: ProgramSetRepository,
        importStandardProgramSets: (JFrame?, ListePset?) -> Unit,
    ) {
        showMissingProgramSetIfNeeded(
            parent,
            programSets,
            ::hasAboProgramSet,
            ::createAboMessageLabel,
            importStandardProgramSets,
        )
    }

    fun showMissingDownloadProgramSet(
        parent: JFrame?,
        programSets: ProgramSetRepository,
        importStandardProgramSets: (JFrame?, ListePset?) -> Unit,
    ) {
        showMissingProgramSetIfNeeded(
            parent,
            programSets,
            ::hasDownloadProgramSet,
            ::createDownloadMessageLabel,
            importStandardProgramSets,
        )
    }

    private fun showMissingProgramSetIfNeeded(
        parent: JFrame?,
        programSets: ProgramSetRepository,
        hasProgramSet: (ProgramSetRepository) -> Boolean,
        createMessageLabel: () -> JLabel,
        importStandardProgramSets: (JFrame?, ListePset?) -> Unit,
    ) {
        if (hasProgramSet(programSets)) {
            return
        }

        if (showImportPrompt(parent, createMessageLabel()) == IMPORT_OPTION_INDEX) {
            importStandardProgramSets(parent, ListePsetVorlagen.getStandarset(parent, true))
        }
    }

    private fun hasAboProgramSet(programSets: ProgramSetRepository): Boolean =
        programSets.list.hasAboProgramSet()

    private fun hasDownloadProgramSet(programSets: ProgramSetRepository): Boolean =
        programSets.list.hasDownloadProgramSet()

    private fun showImportPrompt(parent: JFrame?, messageLabel: JLabel): Int =
        JOptionPane.showOptionDialog(
            parent,
            messageLabel,
            Konstanten.PROGRAMMNAME,
            JOptionPane.DEFAULT_OPTION,
            JOptionPane.WARNING_MESSAGE,
            null,
            options,
            options[CLOSE_OPTION_INDEX],
        )

    private fun createAboMessageLabel(): JLabel =
        JLabel(
            "<html>" +
                "Ein Set von Programmen zum Aufzeichnen wurde nicht angelegt.<br>" +
                "<br>" +
                "Im Menü unter:<br>" +
                "&quot;Datei-&gt;Einstellungen-&gt;Aufzeichnen und Abspielen&quot;<br>" +
                "ein Programm zum Aufzeichnen für Abos festlegen.<br>" +
                "Oder die Standardsets importieren." +
                "</html>"
        )

    private fun createDownloadMessageLabel(): JLabel =
        JLabel(
            "<html>" +
                "Ein Set von Programmen zum Speichern wurde nicht angelegt.<br>" +
                "<br>" +
                "Im Menü unter:<br>" +
                "&quot;Datei-&gt;Einstellungen-&gt;Aufzeichnen und Abspielen&quot;<br>" +
                "ein Programm zum Speichern festlegen.<br>" +
                "Oder die Standardsets importieren." +
                "</html>"
        )
}
