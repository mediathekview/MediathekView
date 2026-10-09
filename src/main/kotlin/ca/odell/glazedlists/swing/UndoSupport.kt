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
package ca.odell.glazedlists.swing

import ca.odell.glazedlists.EventList
import ca.odell.glazedlists.UndoRedoSupport
import javax.swing.SwingUtilities
import javax.swing.undo.AbstractUndoableEdit
import javax.swing.undo.UndoManager
import javax.swing.undo.UndoableEdit

/** Adapts Glazed Lists undo/redo edits to Swing's [UndoManager]. */
class UndoSupport<E> private constructor(
    undoManager: UndoManager,
    source: EventList<E>,
    editAdapter: (UndoRedoSupport.Edit) -> UndoableEdit,
) {
    private var undoManager: UndoManager? = undoManager
    private var undoRedoSupport: UndoRedoSupport<E>? = UndoRedoSupport.install(source)
    private var undoSupportHandler: UndoRedoSupport.Listener? = UndoRedoSupport.Listener { edit ->
        this@UndoSupport.undoManager!!.addEdit(this@UndoSupport.editAdapter!!(edit))
    }
    private var editAdapter: ((UndoRedoSupport.Edit) -> UndoableEdit)? = editAdapter

    init {
        undoRedoSupport!!.addUndoSupportListener(requireNotNull(undoSupportHandler))
    }

    fun uninstall() {
        checkAccessThread()

        val support = undoRedoSupport!!
        support.removeUndoSupportListener(requireNotNull(undoSupportHandler))
        support.uninstall()

        undoSupportHandler = null
        undoRedoSupport = null
        undoManager = null
        editAdapter = null
    }


    private class DefaultEditAdapter : (UndoRedoSupport.Edit) -> UndoableEdit {
        override fun invoke(edit: UndoRedoSupport.Edit): UndoableEdit = EditAdapter(edit)

        private class EditAdapter(
            private val edit: UndoRedoSupport.Edit,
        ) : AbstractUndoableEdit() {
            override fun undo() = edit.undo()

            override fun canUndo(): Boolean = edit.canUndo()

            override fun redo() = edit.redo()

            override fun canRedo(): Boolean = edit.canRedo()
        }
    }

    companion object {
        fun <E> install(
            undoManager: UndoManager,
            source: EventList<E>,
        ): UndoSupport<E> {
            checkAccessThread()
            return UndoSupport(undoManager, source, DefaultEditAdapter())
        }

        private fun checkAccessThread() {
            check(SwingUtilities.isEventDispatchThread()) {
                "UndoRedoSupport must be accessed from the Swing Event Dispatch Thread, but was called on Thread \"${Thread.currentThread().name}\""
            }
        }
    }
}
