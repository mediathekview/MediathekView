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

package mediathek.tool

import java.awt.Toolkit
import java.awt.datatransfer.DataFlavor
import java.awt.event.ActionEvent
import javax.swing.AbstractAction
import javax.swing.Action
import javax.swing.JPopupMenu
import javax.swing.event.PopupMenuEvent
import javax.swing.event.PopupMenuListener
import javax.swing.text.JTextComponent

class TextCopyPasteHandler<T : JTextComponent>(
    private val textComponent: T,
) {
    private val cutAction: Action
    private val copyAction: Action
    private val pasteAction: Action
    private val undoAction: Action
    private val selectAllAction: Action
    private val popup = JPopupMenu()
    private var savedString = ""
    private var lastActionSelected: Actions? = null

    init {
        undoAction = object : AbstractAction("Widerrufen") {
            override fun actionPerformed(ae: ActionEvent) {
                textComponent.text = ""
                textComponent.replaceSelection(savedString)

                lastActionSelected = Actions.UNDO
            }
        }

        popup.add(undoAction)
        popup.addSeparator()

        cutAction = object : AbstractAction("Ausschneiden") {
            override fun actionPerformed(ae: ActionEvent) {
                lastActionSelected = Actions.CUT
                savedString = textComponent.text
                textComponent.cut()
            }
        }

        popup.add(cutAction)

        copyAction = object : AbstractAction("Kopieren") {
            override fun actionPerformed(ae: ActionEvent) {
                lastActionSelected = Actions.COPY
                textComponent.copy()
            }
        }

        popup.add(copyAction)

        pasteAction = object : AbstractAction("Einfügen") {
            override fun actionPerformed(ae: ActionEvent) {
                lastActionSelected = Actions.PASTE
                savedString = textComponent.text
                textComponent.paste()
            }
        }

        popup.add(pasteAction)
        popup.addSeparator()

        selectAllAction = object : AbstractAction("Alles markieren") {
            override fun actionPerformed(ae: ActionEvent) {
                lastActionSelected = Actions.SELECT_ALL
                textComponent.selectAll()
            }
        }

        popup.add(selectAllAction)

        popup.addPopupMenuListener(object : PopupMenuListener {
            override fun popupMenuWillBecomeVisible(e: PopupMenuEvent) {
                val enabled = textComponent.isEnabled
                val editable = textComponent.isEditable
                val nonempty = !textComponent.text.isNullOrEmpty()
                val marked = textComponent.selectedText != null
                val pasteAvailable = Toolkit.getDefaultToolkit()
                    .systemClipboard
                    .getContents(null)
                    ?.isDataFlavorSupported(DataFlavor.stringFlavor) == true

                textComponent.requestFocusInWindow()

                undoAction.isEnabled = enabled &&
                    editable &&
                    (lastActionSelected == Actions.CUT || lastActionSelected == Actions.PASTE)
                cutAction.isEnabled = enabled && editable && marked
                copyAction.isEnabled = enabled && marked
                pasteAction.isEnabled = enabled && editable && pasteAvailable
                selectAllAction.isEnabled = enabled && nonempty
            }

            override fun popupMenuWillBecomeInvisible(e: PopupMenuEvent) {
            }

            override fun popupMenuCanceled(e: PopupMenuEvent) {
            }
        })
    }

    fun getPopupMenu(): JPopupMenu = popup

    private enum class Actions {
        UNDO,
        CUT,
        COPY,
        PASTE,
        SELECT_ALL,
    }
}
