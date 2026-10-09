package ca.odell.glazedlists

import org.junit.jupiter.api.Assertions.*
import org.junit.jupiter.api.Test

internal class TransactionAndUndoSupportTest {
    @Test
    fun bufferedTransactionPublishesOneAggregateEvent() {
        val source = basicEventListOf("A")
        val transaction = TransactionList(source)
        var eventCount = 0
        transaction.addListEventListener { eventCount++ }

        transaction.beginEvent()
        transaction += "B"
        transaction[0] = "C"
        transaction.commitEvent()

        assertEquals(listOf("C", "B"), source)
        assertEquals(1, eventCount)
    }

    @Test
    fun rollbackRestoresContentsWithoutPublishingBufferedChanges() {
        val source = basicEventListOf("A", "B")
        val transaction = TransactionList(source)
        var eventCount = 0
        transaction.addListEventListener { eventCount++ }

        transaction.beginEvent()
        transaction.removeAt(0)
        transaction += "C"
        transaction.rollbackEvent()

        assertEquals(listOf("A", "B"), source)
        assertEquals(0, eventCount)
    }

    @Test
    fun transactionBlockCommitsAndRollsBackOnFailure() {
        val source = basicEventListOf("A")
        val transaction = TransactionList(source)

        val result = transaction.withTransaction {
            add("B")
            size
        }
        assertEquals(2, result)
        assertEquals(listOf("A", "B"), source)

        assertThrows(IllegalArgumentException::class.java) {
            transaction.withTransaction {
                clear()
                throw IllegalArgumentException("abort")
            }
        }
        assertEquals(listOf("A", "B"), source)
    }

    @Test
    fun consecutiveFailedTransactionsRollbackToTheLatestCommittedState() {
        val source = basicEventListOf("A")
        val transaction = TransactionList(source)

        listOf("B", "C").forEach { replacement ->
            assertThrows(IllegalArgumentException::class.java) {
                transaction.withTransaction {
                    this[0] = replacement
                    throw IllegalArgumentException("abort")
                }
            }
            assertEquals(listOf("A"), source)
        }
    }

    @Test
    fun committedNestedTransactionRemainsPartOfOuterRollback() {
        val source = basicEventListOf("A")
        val transaction = TransactionList(source)
        var eventCount = 0
        transaction.addListEventListener { eventCount++ }

        transaction.beginEvent()
        transaction += "B"
        transaction.beginEvent()
        transaction += "C"
        transaction.commitEvent()
        transaction.rollbackEvent()

        assertEquals(listOf("A"), source)
        assertEquals(0, eventCount)
    }

    @Test
    fun rolledBackNestedTransactionDoesNotPolluteOuterCommit() {
        val source = basicEventListOf("A")
        val transaction = TransactionList(source)
        var eventCount = 0
        transaction.addListEventListener { eventCount++ }

        transaction.beginEvent()
        transaction += "B"
        transaction.beginEvent()
        transaction += "C"
        transaction.rollbackEvent()
        transaction += "D"
        transaction.commitEvent()

        assertEquals(listOf("A", "B", "D"), source)
        assertEquals(1, eventCount)
    }

    @Test
    fun rollbackDisabledConstructionIsNotPublic() {
        assertEquals(
            listOf(listOf(EventList::class.java)),
            TransactionList::class.java.constructors
                .filterNot { it.isSynthetic }
                .map { it.parameterTypes.toList() },
        )
    }

    @Test
    fun rollbackDisabledTransactionRejectsTransactionBlockBeforeMutation() {
        val source = basicEventListOf("A")
        val transaction = TransactionList.withoutRollback(source)
        var blockExecuted = false

        assertThrows(IllegalStateException::class.java) {
            transaction.withTransaction {
                blockExecuted = true
                add("B")
            }
        }

        assertFalse(blockExecuted)
        assertEquals(listOf("A"), source)
    }

    @Test
    fun undoAndRedoPreserveCompoundChangeBehavior() {
        val source = basicEventListOf("A", "B")
        val support = UndoRedoSupport.install(source)
        val edits = mutableListOf<UndoRedoSupport.Edit>()
        support.addUndoSupportListener(edits::add)

        source.add("C")
        val addEdit = edits.single()
        assertTrue(addEdit.canUndo())
        assertFalse(addEdit.canRedo())

        addEdit.undo()
        assertEquals(listOf("A", "B"), source)
        assertFalse(addEdit.canUndo())
        assertTrue(addEdit.canRedo())

        addEdit.redo()
        assertEquals(listOf("A", "B", "C"), source)
        assertEquals(1, edits.size, "undo and redo must not create new edits")
    }

    @Test
    fun undoAndRedoUseTheSnapshotForUpdatesAndRemovals() {
        val source = basicEventListOf("A", "B")
        val support = UndoRedoSupport.install(source)
        val edits = mutableListOf<UndoRedoSupport.Edit>()
        support.addUndoSupportListener(edits::add)

        source[0] = "C"
        val updateEdit = edits.removeLast()
        updateEdit.undo()
        assertEquals(listOf("A", "B"), source)
        updateEdit.redo()
        assertEquals(listOf("C", "B"), source)

        source.removeAt(1)
        val removeEdit = edits.removeLast()
        removeEdit.undo()
        assertEquals(listOf("C", "B"), source)
        removeEdit.redo()
        assertEquals(listOf("C"), source)
        assertTrue(edits.isEmpty(), "undo and redo must not create new edits")
    }

    private fun <E> basicEventListOf(vararg elements: E): BasicEventList<E> =
        BasicEventList<E>().apply { addAll(elements) }
}
