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
import mediathek.daten.ProgramSetRepository
import mediathek.i18n.UiMessages
import mediathek.tool.EscapeKeyHandler
import mediathek.tool.GetFile
import mediathek.tool.SVGIconUtilities
import javax.swing.JFrame
import javax.swing.JOptionPane

class DialogNewSet(
    private val parent: JFrame?,
    private val programSets: ProgramSetRepository,
) : DialogNewSetBase(parent) {
    private var selectedAction: DialogNewSetAction = DialogNewSetAction.CANCEL

    val decision: DialogNewSetDecision
        get() = DialogNewSetDecision(
            action = selectedAction,
            askAgainTomorrow = jCheckBoxMorgen.isSelected,
        )

    init {
        setLocationRelativeTo(parent)
        title = UiMessages.text("sets.update.title")
        jTextArea3.text = UiMessages.text("sets.update.intro")
        jCheckBoxMorgen.isSelected = true

        jTextArea1.text = UiMessages.text("sets.update.add")

        jTextArea2.text = UiMessages.text("sets.update.replace")

        jButtonAdd.addActionListener {
            selectedAction = DialogNewSetAction.ADD
            closeDialog()
        }
        jButtonAbbrechen.addActionListener {
            selectedAction = DialogNewSetAction.CANCEL
            closeDialog()
        }
        jButtonReplace.addActionListener {
            val result = JOptionPane.showConfirmDialog(
                parent,
                UiMessages.text("sets.update.confirm"),
                UiMessages.text("sets.update.reset"),
                JOptionPane.YES_NO_OPTION,
            )
            if (result == JOptionPane.OK_OPTION) {
                programSets.list.clear()
                selectedAction = DialogNewSetAction.REPLACE
                closeDialog()
            }
        }
        jButtonSetHelp.icon = SVGIconUtilities.createSVGIcon("icons/fontawesome/circle-question.svg")
        jButtonSetHelp.addActionListener {
            HelpTextDialog.show(parent, GetFile.getHilfeSuchen(Konstanten.PFAD_HILFETEXT_RESET_SET))
        }

        EscapeKeyHandler.installHandler(this) {
            selectedAction = DialogNewSetAction.CANCEL
            dispose()
        }

        pack()
    }

    private fun closeDialog() {
        dispose()
    }
}

enum class DialogNewSetAction {
    ADD,
    REPLACE,
    CANCEL,
}

data class DialogNewSetDecision(
    val action: DialogNewSetAction,
    val askAgainTomorrow: Boolean,
) {
    val accepted: Boolean
        get() = action != DialogNewSetAction.CANCEL
}
