package ca.odell.glazedlists

import ca.odell.glazedlists.event.ListEvent
import ca.odell.glazedlists.event.ListEventListener
import ca.odell.glazedlists.matchers.Matcher
import ca.odell.glazedlists.matchers.MatcherEditor
import ca.odell.glazedlists.swing.SortableRenderer
import ca.odell.glazedlists.swing.TableModelEventAdapter
import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Assertions.assertSame
import org.junit.jupiter.api.Test
import javax.swing.Icon
import javax.swing.table.AbstractTableModel

internal class FunctionalInterfaceBehaviorTest {
    @Test
    fun functionListUsesKotlinFunctionsDirectly() {
        val source = BasicEventList<String>().apply { add("aa") }
        val forward: (String) -> Int = String::length
        val reverse: (Int) -> String = { "x".repeat(it) }

        val mapped = FunctionList(source, forward, reverse)

        assertSame(forward, mapped.forwardFunction)
        assertSame(reverse, mapped.reverseFunction)
        assertEquals(listOf(2), mapped.toList())

        mapped.add(3)

        assertEquals(listOf("aa", "xxx"), source)
    }

    @Test
    fun advancedFunctionRetainsItsLifecycleHooks() {
        val source = BasicEventList<String>().apply { add("aa") }
        val reevaluations = mutableListOf<Pair<String, Int>>()
        val disposals = mutableListOf<Pair<String, Int>>()
        val function = object : FunctionList.AdvancedFunction<String, Int> {
            override fun invoke(sourceValue: String): Int = sourceValue.length

            override fun reevaluate(sourceValue: String, transformedValue: Int): Int {
                reevaluations += sourceValue to transformedValue
                return sourceValue.length
            }

            override fun dispose(sourceValue: String, transformedValue: Int) {
                disposals += sourceValue to transformedValue
            }
        }
        val mapped = FunctionList(source, function)

        source[0] = "bbbb"
        source.clear()

        assertSame(function, mapped.forwardFunction)
        assertEquals(listOf("bbbb" to 2), reevaluations)
        assertEquals(listOf("bbbb" to 4), disposals)
    }

    @Test
    fun extractionContractsRetainKotlinSamConstruction() {
        val values = mutableListOf<String>()
        val textFilterator = TextFilterator<String> { target, element -> target += "text:$element" }
        val textFilterable = TextFilterable { target -> target += "self" }

        textFilterator.getFilterStrings(values, "one")
        textFilterable.getFilterStrings(values)

        assertEquals(listOf("text:one", "self"), values)
    }

    @Test
    fun extractionAndElementChangeInterfacesRetainSamConstruction() {
        val extracted = mutableListOf<Int>()
        val filterator = Filterator<Int, String> { target, element -> target += element.length }
        var changedElement: Any? = null
        val changeHandler = ObservableElementChangeHandler<String> { changedElement = it }

        filterator.getFilterValues(extracted, "value")
        changeHandler.elementChanged(42)

        assertEquals(listOf(5), extracted)
        assertEquals(42, changedElement)
    }

    @Test
    fun mappingContractsRetainKotlinSamConstruction() {
        val model = CollectionList.Model<String, Int> { parent -> parent.indices.toList() }
        val evaluator = ThresholdList.Evaluator(String::length)

        assertEquals(listOf(0, 1, 2), model.getChildren("abc"))
        assertEquals(3, evaluator.evaluate("abc"))
    }

    @Test
    fun listenerContractsRetainKotlinSamConstruction() {
        val source = BasicEventList<String>()
        lateinit var listEvent: ListEvent<String>
        source.addListEventListener { listEvent = it }
        source.add("value")

        var receivedListEvent: ListEvent<String>? = null
        val listListener = ListEventListener { receivedListEvent = it }
        listListener.listChanged(listEvent)
        assertSame(listEvent, receivedListEvent)

        val matcher = Matcher<String> { true }
        val matcherEditor = MatcherEditor.fromMatcher(matcher)
        val matcherEvent = MatcherEditor.Event(matcherEditor, MatcherEditor.Event.CHANGED, matcher)
        var receivedMatcherEvent: MatcherEditor.Event<String>? = null
        val matcherListener = MatcherEditor.Listener { receivedMatcherEvent = it }
        matcherListener.changedMatcher(matcherEvent)
        assertSame(matcherEvent, receivedMatcherEvent)

        val edit = TestEdit()
        var receivedEdit: UndoRedoSupport.Edit? = null
        val undoListener = UndoRedoSupport.Listener { receivedEdit = it }
        undoListener.undoableEditHappened(edit)
        assertSame(edit, receivedEdit)

        var receivedIcon: Icon? = TestIcon
        val renderer = SortableRenderer { receivedIcon = it }
        renderer.setSortIcon(null)
        assertEquals(null, receivedIcon)
    }

    @Test
    fun tableAdapterFactoryRetainsKotlinSamConstruction() {
        val tableModel = TestTableModel()
        val adapter = TestTableModelEventAdapter()
        val factory = TableModelEventAdapter.Factory { adapter }

        assertSame(adapter, factory.create(tableModel))
    }

    private class TestEdit : UndoRedoSupport.Edit {
        override fun undo() = Unit
        override fun canUndo() = true
        override fun redo() = Unit
        override fun canRedo() = false
    }

    private object TestIcon : Icon {
        override fun paintIcon(component: java.awt.Component?, graphics: java.awt.Graphics?, x: Int, y: Int) = Unit
        override fun getIconWidth() = 1
        override fun getIconHeight() = 1
    }

    private class TestTableModel : AbstractTableModel() {
        override fun getRowCount() = 0
        override fun getColumnCount() = 0
        override fun getValueAt(rowIndex: Int, columnIndex: Int): Any? = null
    }

    private class TestTableModelEventAdapter : TableModelEventAdapter<String> {
        override fun listChanged(listChanges: ListEvent<String>) = Unit
        override fun fireTableStructureChanged() = Unit
        override fun fireTableDataChanged() = Unit
        override fun fireTableChanged(startIndex: Int, endIndex: Int, listChangeType: Int) = Unit
    }
}
