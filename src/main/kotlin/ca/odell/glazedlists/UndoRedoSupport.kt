package ca.odell.glazedlists

import ca.odell.glazedlists.event.ListEvent
import ca.odell.glazedlists.event.ListEventListener
import java.util.*
import java.util.concurrent.CopyOnWriteArrayList

class UndoRedoSupport<E> private constructor(source: EventList<E>) {
    private var txSource: TransactionList<E>? = TransactionList.withoutRollback(source)
    private val txSourceListener = TXSourceListener()
    private val listenerList = CopyOnWriteArrayList<Listener>()
    private var ignoreListEvent = 0
    private var priorElements: MutableList<E>? = source.toMutableList()

    init {
        txSource?.addListEventListener(txSourceListener)
    }

    fun addUndoSupportListener(listener: Listener) {
        listenerList += listener
    }

    fun removeUndoSupportListener(listener: Listener) {
        listenerList -= listener
    }

    fun uninstall() {
        val transactionalSource = txSource ?: return
        transactionalSource.removeListEventListener(txSourceListener)
        transactionalSource.dispose()
        txSource = null
        priorElements = null
    }

    internal inner class CompositeEdit internal constructor() : AbstractEdit() {
        private val edits = mutableListOf<Edit>()

        internal fun add(edit: Edit) {
            edits += edit
        }

        internal val isEmpty: Boolean
            get() = edits.isEmpty()

        internal val simplestEdit: Edit
            get() = edits.singleOrNull() ?: this

        override fun undoImpl() {
            val source = requireTxSource()
            source.beginEvent()
            try {
                edits.asReversed().forEach(Edit::undo)
            } finally {
                source.commitEvent()
            }
        }

        override fun redoImpl() {
            val source = requireTxSource()
            source.beginEvent()
            try {
                edits.forEach(Edit::redo)
            } finally {
                source.commitEvent()
            }
        }
    }

    fun interface Listener : EventListener {
        fun undoableEditHappened(edit: Edit)
    }

    interface Edit {
        fun undo()
        fun canUndo(): Boolean
        fun redo()
        fun canRedo(): Boolean
    }

    private inner class TXSourceListener : ListEventListener<E> {
        override fun listChanged(listChanges: ListEvent<E>) {
            val source = requireTxSource()
            val previousElements = requireNotNull(priorElements) { "Undo support has been uninstalled" }

            if (ignoreListEvent > 0) {
                synchronizeSnapshot(listChanges, source, previousElements)
                return
            }

            val edit = CompositeEdit()

            while (listChanges.next()) {
                val changeIndex = listChanges.index
                when (listChanges.type) {
                    ListEvent.INSERT -> {
                        val inserted = source[changeIndex]
                        previousElements.add(changeIndex, inserted)
                        edit.add(AddEdit(source, changeIndex, inserted))
                    }

                    ListEvent.DELETE -> {
                        val deleted = previousElements.removeAt(changeIndex)
                        edit.add(RemoveEdit(source, changeIndex, deleted))
                    }

                    ListEvent.UPDATE -> {
                        val previousValue = previousElements[changeIndex]
                        val newValue = source[changeIndex]
                        if (newValue !== previousValue) {
                            previousElements[changeIndex] = newValue
                            edit.add(UpdateEdit(source, changeIndex, newValue, previousValue))
                        }
                    }
                }
            }

            if (!edit.isEmpty) fireUndoableEditHappened(edit.simplestEdit)
        }

        private fun synchronizeSnapshot(
            listChanges: ListEvent<E>,
            source: EventList<E>,
            previousElements: MutableList<E>,
        ) {
            while (listChanges.next()) {
                val changeIndex = listChanges.index
                when (listChanges.type) {
                    ListEvent.INSERT -> previousElements.add(changeIndex, source[changeIndex])
                    ListEvent.DELETE -> previousElements.removeAt(changeIndex)
                    ListEvent.UPDATE -> previousElements[changeIndex] = source[changeIndex]
                }
            }
        }
    }

    internal abstract inner class AbstractEdit : Edit {
        private var undoAvailable = true

        final override fun undo() {
            check(canUndo()) { "The Edit is in an incorrect state for undoing" }
            ignoreListEvent++
            try {
                undoImpl()
            } finally {
                ignoreListEvent--
            }
            undoAvailable = false
        }

        final override fun redo() {
            check(canRedo()) { "The Edit is in an incorrect state for redoing" }
            ignoreListEvent++
            try {
                redoImpl()
            } finally {
                ignoreListEvent--
            }
            undoAvailable = true
        }

        final override fun canUndo(): Boolean = undoAvailable
        final override fun canRedo(): Boolean = !undoAvailable

        protected abstract fun undoImpl()
        protected abstract fun redoImpl()
    }

    private abstract inner class AbstractSimpleEdit(
        protected val source: EventList<E>,
        protected val index: Int,
        protected val value: E,
    ) : AbstractEdit()

    private inner class AddEdit(source: EventList<E>, index: Int, value: E) :
        AbstractSimpleEdit(source, index, value) {
        override fun undoImpl() {
            source.removeAt(index)
        }

        override fun redoImpl() {
            source.add(index, value)
        }
    }

    private inner class RemoveEdit(source: EventList<E>, index: Int, value: E) :
        AbstractSimpleEdit(source, index, value) {
        override fun undoImpl() {
            source.add(index, value)
        }

        override fun redoImpl() {
            source.removeAt(index)
        }
    }

    private inner class UpdateEdit(
        source: EventList<E>,
        index: Int,
        value: E,
        private val oldValue: E,
    ) : AbstractSimpleEdit(source, index, value) {
        override fun undoImpl() {
            source[index] = oldValue
        }

        override fun redoImpl() {
            source[index] = value
        }
    }

    private fun fireUndoableEditHappened(edit: Edit) {
        listenerList.toList().asReversed().forEach { it.undoableEditHappened(edit) }
    }

    private fun requireTxSource(): TransactionList<E> =
        requireNotNull(txSource) { "Undo support has been uninstalled" }

    companion object {
        fun <E> install(source: EventList<E>): UndoRedoSupport<E> = UndoRedoSupport(source)
    }
}
