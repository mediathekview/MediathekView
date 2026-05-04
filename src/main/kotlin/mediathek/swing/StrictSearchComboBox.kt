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

package mediathek.swing

import com.jidesoft.swing.AutoCompletion
import javax.swing.DefaultComboBoxModel
import javax.swing.JComboBox
import javax.swing.JTextField
import javax.swing.event.PopupMenuEvent
import javax.swing.event.PopupMenuListener

/**
 * A strict searchable combo box that allows typing for quick search, but does not
 * treat key-navigation or in-progress auto-completion as a committed selection.
 */
class StrictSearchComboBox : JComboBox<String>(DefaultComboBoxModel()) {
    private lateinit var autoCompletion: AutoCompletion
    private var committingSelection = false
    private var selectionBeforePopupOpened: Any? = null

    init {
        initialize()
    }

    fun setNoActionOnKeyNavigation(value: Boolean) {
        searchable.setNoActionOnKeyNavigation(value)
    }

    override fun setSelectedItem(anObject: Any?) {
        super.setSelectedItem(anObject)
        syncEditorWithSelection()
    }

    override fun setSelectedIndex(index: Int) {
        super.setSelectedIndex(index)
        syncEditorWithSelection()
    }

    override fun fireActionEvent() {
        if (isPreventingActionEvent() || !committingSelection) {
            return
        }

        resetCaretPosition()
        super.fireActionEvent()
    }

    private val searchable: NoFireOnKeyComboBoxSearchable
        get() = autoCompletion.searchable as NoFireOnKeyComboBoxSearchable

    private fun initialize() {
        isEditable = true
        updateUI()

        autoCompletion = AutoCompletion(this, NoFireOnKeyComboBoxSearchable(this)).apply {
            isStrict = true
            isStrictCompletion = true
        }
        setNoActionOnKeyNavigation(true)

        installCommitListeners()
    }

    private fun isPreventingActionEvent(): Boolean = searchable.isPreventActionEvent

    private fun installCommitListeners() {
        (editor.editorComponent as? JTextField)?.addActionListener { fireCommittedActionEvent() }

        addPopupMenuListener(object : PopupMenuListener {
            override fun popupMenuWillBecomeVisible(event: PopupMenuEvent) {
                selectionBeforePopupOpened = selectedItem
            }

            override fun popupMenuWillBecomeInvisible(event: PopupMenuEvent) {
                if (selectionBeforePopupOpened != selectedItem) {
                    fireCommittedActionEvent()
                }
            }

            override fun popupMenuCanceled(event: PopupMenuEvent) = Unit
        })
    }

    private fun fireCommittedActionEvent() {
        if (committingSelection) {
            return
        }

        committingSelection = true
        try {
            resetCaretPosition()
            super.fireActionEvent()
        } finally {
            committingSelection = false
        }
    }

    private fun resetCaretPosition() {
        val textField = editor.editorComponent as? JTextField ?: return
        val textLength = textField.text.length
        if (textLength > 0) {
            textField.caretPosition = textLength
        }
    }

    private fun syncEditorWithSelection() {
        val currentEditor = editor ?: return
        currentEditor.item = selectedItem ?: ""
        resetCaretPosition()
    }
}
