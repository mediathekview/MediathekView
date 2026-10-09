package ca.odell.glazedlists

import ca.odell.glazedlists.impl.filter.SearchTerm
import ca.odell.glazedlists.impl.filter.TextMatchers
import ca.odell.glazedlists.impl.filter.TextSearchStrategy
import ca.odell.glazedlists.matchers.TextMatcherEditor
import ca.odell.glazedlists.swing.AutoCompleteSupport
import ca.odell.glazedlists.swing.EventTableColumnModel
import ca.odell.glazedlists.swing.TextComponentMatcherEditor
import com.formdev.flatlaf.FlatLaf
import com.formdev.flatlaf.themes.FlatMacLightLaf
import org.junit.jupiter.api.Assertions.*
import org.junit.jupiter.api.Test
import java.beans.PropertyChangeEvent
import java.text.FieldPosition
import java.text.Format
import java.text.ParsePosition
import java.time.Instant
import java.time.ZoneId
import java.time.ZonedDateTime
import java.util.*
import java.util.concurrent.locks.Lock
import java.util.concurrent.locks.ReadWriteLock
import java.util.concurrent.locks.ReentrantReadWriteLock
import javax.swing.JComboBox
import javax.swing.JTextField
import javax.swing.SwingUtilities
import javax.swing.UIManager
import javax.swing.event.ChangeEvent
import javax.swing.event.ListSelectionEvent
import javax.swing.event.TableColumnModelEvent
import javax.swing.event.TableColumnModelListener
import javax.swing.table.TableColumn
import javax.swing.text.PlainDocument

internal class ProbableBugsBehaviorTest {
    @Test
    fun sequenceListAcceptsAnyComparatorMagnitude() {
        val source = BasicEventList<Int>()
        source.addAll(listOf(5, 25))
        val sequencer = object : SequenceList.Sequencer<Int> {
            override fun previous(value: Int): Int = Math.floorDiv(value - 1, 10) * 10

            override fun next(value: Int): Int = (Math.floorDiv(value, 10) + 1) * 10
        }
        val comparator = Comparator<Int> { left, right -> left.compareTo(right) * 7 }

        SequenceList(source, sequencer, comparator).use { sequence ->
            assertEquals(listOf(0, 10, 20, 30), sequence.toList())

            source[1] = 45
            assertEquals(listOf(0, 10, 20, 30, 40, 50), sequence.toList())

            source[1] = 15
            assertEquals(listOf(0, 10, 20), sequence.toList())
        }
    }

    @Test
    fun monthSequencerKeepsMonthBoundariesInItsConstructionTimeZone() {
        val originalTimeZone = TimeZone.getDefault()
        try {
            val berlin = ZoneId.of("Europe/Berlin")
            TimeZone.setDefault(TimeZone.getTimeZone(berlin))
            val sequencer = Sequencers.monthSequencer()
            val februaryStart = ZonedDateTime.of(2006, 2, 1, 0, 0, 0, 0, berlin)
            val februaryMiddle = ZonedDateTime.of(2006, 2, 15, 3, 21, 22, 234_000_000, berlin)

            TimeZone.setDefault(TimeZone.getTimeZone("America/New_York"))

            assertEquals(
                Date.from(februaryStart.toInstant()),
                sequencer.previous(Date.from(februaryMiddle.toInstant()))
            )
            assertEquals(
                Date.from(februaryStart.minusMonths(1).toInstant()),
                sequencer.previous(Date.from(februaryStart.toInstant())),
            )
            assertEquals(
                Date.from(februaryStart.plusMonths(1).toInstant()),
                sequencer.next(Date.from(februaryMiddle.toInstant())),
            )

            TimeZone.setDefault(TimeZone.getTimeZone("America/Managua"))
            val overlapSequencer = Sequencers.monthSequencer()
            assertEquals(
                Date.from(Instant.parse("2006-10-01T06:00:00Z")),
                overlapSequencer.next(Date.from(Instant.parse("2006-09-01T05:17:23.456Z"))),
            )
        } finally {
            TimeZone.setDefault(originalTimeZone)
        }
    }

    @Test
    fun textMatcherNormalizationDoesNotSkipAdjacentRedundantTerms() {
        val positive = arrayOf("a", "ab", "abc").map { SearchTerm<Any>(it) }.toTypedArray()
        val negative = arrayOf("a", "ab", "abc")
            .map { SearchTerm<Any>(it, isNegated = true, isRequired = false, field = null) }
            .toTypedArray()

        val strategy = TextMatcherEditor.IDENTICAL_STRATEGY as TextSearchStrategy.Factory
        val normalizedPositive = TextMatchers.normalizeSearchTerms(positive, strategy)
        val normalizedNegative = TextMatchers.normalizeSearchTerms(negative, strategy)

        assertEquals(listOf("abc"), normalizedPositive.map { it.text })
        assertEquals(listOf("a"), normalizedNegative.map { it.text })
    }

    @Test
    fun columnModelRecognizesEquivalentNonInternedPropertyNames() {
        SwingUtilities.invokeAndWait {
            val source = BasicEventList<TableColumn>().apply { addAll(listOf(TableColumn(0), TableColumn(1))) }
            val model = EventTableColumnModel(source)
            try {
                var marginChanges = 0
                model.addColumnModelListener(object : TableColumnModelListener {
                    override fun columnMarginChanged(event: ChangeEvent) {
                        marginChanges++
                    }

                    override fun columnAdded(event: TableColumnModelEvent) = Unit
                    override fun columnRemoved(event: TableColumnModelEvent) = Unit
                    override fun columnMoved(event: TableColumnModelEvent) = Unit
                    override fun columnSelectionChanged(event: ListSelectionEvent) = Unit
                })

                model.propertyChange(
                    PropertyChangeEvent(source[0], charArrayOf('w', 'i', 'd', 't', 'h').concatToString(), 75, 100),
                )

                assertEquals(1, marginChanges)

                val enumeratedColumns = buildList {
                    val columns = model.columns
                    while (columns.hasMoreElements()) add(columns.nextElement())
                }
                assertEquals(source.toList(), enumeratedColumns)
            } finally {
                model.dispose()
            }
        }
    }

    @Test
    fun textComponentEditorRecognizesEquivalentNonInternedDocumentPropertyName() {
        SwingUtilities.invokeAndWait {
            val field = NonInterningTextField()
            val editor = TextComponentMatcherEditor<String>(field) { strings, value -> strings += value }
            try {
                val replacement = PlainDocument()
                var matcherChanges = 0
                editor.addMatcherEditorListener { matcherChanges++ }

                field.document = replacement
                replacement.insertString(0, "new filter", null)

                assertEquals(1, matcherChanges)
            } finally {
                editor.dispose()
            }
        }
    }

    @Test
    fun autoCompleteSupportInstallsWithFlatLaf() {
        SwingUtilities.invokeAndWait {
            val previousLookAndFeel = UIManager.getLookAndFeel()
            val comboBox = JComboBox<String>()
            val source = BasicEventList<String>().apply { addAll(listOf("alpha", "beta")) }

            try {
                FlatLaf.setup(FlatMacLightLaf())
                SwingUtilities.updateComponentTreeUI(comboBox)

                val support = AutoCompleteSupport.install(comboBox, source)
                assertTrue(support.isInstalled)
                assertTrue(comboBox.isEditable)
                assertEquals(listOf("alpha", "beta"), (0 until comboBox.itemCount).map(comboBox::getItemAt))
            } finally {
                UIManager.setLookAndFeel(previousLookAndFeel)
            }
        }
    }

    @Test
    fun autoCompleteSupportUninstallsWithoutUpgradingAReadLock() {
        SwingUtilities.invokeAndWait {
            val comboBox = JComboBox(arrayOf("original"))
            val originalModel = comboBox.model
            val source = BasicEventList<String>(UpgradeDetectingReadWriteLock()).apply { add("alpha") }
            val support = AutoCompleteSupport.install(comboBox, source)

            support.uninstall()

            assertFalse(support.isInstalled)
            assertSame(originalModel, comboBox.model)
            assertFalse(comboBox.isEditable)
        }
    }

    @Test
    fun autoCompleteInstallationUsesTheSourceWriteLock() {
        SwingUtilities.invokeAndWait {
            val comboBox = JComboBox<String>()
            val source = BasicEventList<String>(WriteLockedConstructionReadWriteLock()).apply { add("alpha") }

            val support = AutoCompleteSupport.install(comboBox, source)

            support.uninstall()
        }
    }

    @Test
    fun autoCompleteFormatParsesEachEditedValueFromTheBeginning() {
        SwingUtilities.invokeAndWait {
            val comboBox = JComboBox<Number>(arrayOf(0))
            val source = BasicEventList<Number>().apply { add(0) }
            val integerFormat = object : Format() {
                override fun format(obj: Any?, toAppendTo: StringBuffer, pos: FieldPosition): StringBuffer =
                    toAppendTo.append(obj?.toString().orEmpty())

                override fun parseObject(source: String, pos: ParsePosition): Any? {
                    if (pos.index != 0) {
                        pos.errorIndex = pos.index
                        return null
                    }
                    pos.index = source.length
                    return source.toLong()
                }
            }
            AutoCompleteSupport.install(
                comboBox,
                source,
                GlazedLists.toStringTextFilterator(),
                integerFormat,
            )

            val editor = comboBox.editor
            val editorComponent = editor.editorComponent as JTextField
            editor.item = 0

            editorComponent.text = "12"
            assertEquals(12L, editor.item)

            editorComponent.text = "34"
            assertEquals(34L, editor.item)
        }
    }

    private class NonInterningTextField : JTextField() {
        override fun firePropertyChange(propertyName: String?, oldValue: Any?, newValue: Any?) {
            super.firePropertyChange(propertyName?.toCharArray()?.concatToString(), oldValue, newValue)
        }
    }

    private class UpgradeDetectingReadWriteLock : ReadWriteLock {
        private val delegate = ReentrantReadWriteLock()
        private val guardedWriteLock = object : Lock by delegate.writeLock() {
            override fun lock() {
                check(delegate.readHoldCount == 0 || delegate.isWriteLockedByCurrentThread) {
                    "read-to-write lock upgrade attempted"
                }
                delegate.writeLock().lock()
            }
        }

        override fun readLock(): Lock = delegate.readLock()

        override fun writeLock(): Lock = guardedWriteLock
    }

    private class WriteLockedConstructionReadWriteLock : ReadWriteLock {
        private val delegate = ReentrantReadWriteLock()
        private val guardedReadLock = object : Lock by delegate.readLock() {
            override fun lock() {
                check(delegate.isWriteLockedByCurrentThread) {
                    "construction must hold the write lock before reading"
                }
                delegate.readLock().lock()
            }
        }

        override fun readLock(): Lock = guardedReadLock

        override fun writeLock(): Lock = delegate.writeLock()
    }
}
