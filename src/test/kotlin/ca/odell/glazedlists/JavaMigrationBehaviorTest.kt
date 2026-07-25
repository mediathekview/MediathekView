package ca.odell.glazedlists

import ca.odell.glazedlists.gui.TableFormat
import ca.odell.glazedlists.impl.beans.BeanTableFormat
import ca.odell.glazedlists.impl.filter.StringLengthComparator
import ca.odell.glazedlists.impl.filter.StringTextFilterator
import ca.odell.glazedlists.impl.functions.ConstantFunction
import ca.odell.glazedlists.impl.sort.*
import ca.odell.glazedlists.swing.isSwingThreadProxyList
import org.junit.jupiter.api.Assertions.*
import org.junit.jupiter.api.Test

internal class JavaMigrationBehaviorTest {
    @Test
    fun glazedListsSwingProxyCheckPreservesNullBehavior() {
        assertFalse(null.isSwingThreadProxyList())
    }

    @Test
    fun tableColumnComparatorPreservesTheHelpfulMessageAndCause() {
        val format = object : TableFormat<NonComparable> {
            override fun getColumnCount(): Int = 1
            override fun getColumnName(column: Int): String = "Value"
            override fun getColumnValue(baseObject: NonComparable, column: Int): Any = baseObject
        }
        val comparator = TableColumnComparator(format, 0)

        val failure = assertThrows(IllegalStateException::class.java) {
            comparator.compare(NonComparable("left"), NonComparable("right"))
        }

        assertEquals(
            "TableComparatorChooser can not sort objects \"left\", \"right\" that do not implement Comparable.",
            failure.message,
        )
        assertInstanceOf(ClassCastException::class.java, failure.cause)
    }

    @Test
    fun tableFormatPreservesNullableCellValuesForKotlinCallers() {
        val format: TableFormat<String> = object : TableFormat<String> {
            override fun getColumnCount(): Int = 1

            override fun getColumnName(column: Int): String = "Optional value"

            override fun getColumnValue(baseObject: String, column: Int): Any? = null
        }

        val value: Any? = format.getColumnValue("row", 0)

        assertNull(value)
    }

    @Test
    fun comparatorChainKeepsOrderingCopyAndEqualityContracts() {
        val byLength = compareBy<String> { it.length }
        val alphabetically = Comparator.naturalOrder<String>()
        val source = mutableListOf(byLength, alphabetically)
        val chain = ComparatorChain(source)

        source.clear()

        assertEquals(-1, chain.compare("b", "aa"))
        assertEquals(-1, chain.compare("a", "b"))
        assertEquals(2, chain.comparators.size)
        assertEquals(chain, ComparatorChain(listOf(byLength, alphabetically)))
        assertNotEquals(chain, ComparatorChain(listOf(alphabetically)))
        assertEquals(chain.hashCode(), ComparatorChain(listOf(byLength, alphabetically)).hashCode())

        chain.comparators()[0] = alphabetically
        chain.comparators[0] = alphabetically

        assertEquals(-1, chain.compare("b", "aa"))

        val sourceArray = arrayOf<Comparator<String>>(byLength, alphabetically)
        val arrayChain = ComparatorChain(sourceArray)
        sourceArray[0] = alphabetically

        assertEquals(-1, arrayChain.compare("b", "aa"))
    }

    @Test
    fun statelessComparatorsKeepEqualsAndHashCodeConsistent() {
        val booleanComparator = BooleanComparator()
        val equalBooleanComparator = BooleanComparator()
        val comparableComparator = ComparableComparator<String>()
        val equalComparableComparator = ComparableComparator<String>()

        assertEquals(booleanComparator, equalBooleanComparator)
        assertEquals(booleanComparator.hashCode(), equalBooleanComparator.hashCode())
        assertTrue(hashSetOf(booleanComparator).contains(equalBooleanComparator))

        assertEquals(comparableComparator, equalComparableComparator)
        assertEquals(comparableComparator.hashCode(), equalComparableComparator.hashCode())
        assertTrue(hashSetOf(comparableComparator).contains(equalComparableComparator))

        assertTrue(booleanComparator.compare(null, false) < 0)
        assertTrue(booleanComparator.compare(false, true) < 0)
        assertEquals(0, booleanComparator.compare(null, null))
        assertTrue(comparableComparator.compare(null, "a") < 0)
        assertTrue(comparableComparator.compare("a", "b") < 0)
        assertEquals(0, comparableComparator.compare(null, null))
    }

    @Test
    fun statelessFactoriesKeepStableSingletonIdentity() {
        assertSame(GlazedLists.booleanComparator(), GlazedLists.booleanComparator())
        assertSame(GlazedLists.comparableComparator<String>(), GlazedLists.comparableComparator<String>())
        assertSame(GlazedLists.reverseComparator<String>(), GlazedLists.reverseComparator<String>())
        assertSame(GlazedLists.toStringTextFilterator<String>(), GlazedLists.toStringTextFilterator<String>())
    }

    @Test
    fun stringTextFilteratorAppendsNonNullStringValuesOnly() {
        val filterStrings = mutableListOf("existing")
        val filterator = StringTextFilterator<Any?>()

        filterator.getFilterStrings(filterStrings, null)
        filterator.getFilterStrings(filterStrings, 42)

        assertEquals(listOf("existing", "42"), filterStrings)
    }

    @Test
    fun convertedLeafFunctionsAndComparatorsKeepTheirContracts() {
        val constant = ConstantFunction<String, Int?>(null)
        assertNull(constant("ignored"))

        val naturalOrder = Comparator.naturalOrder<String>()
        val reverse = ReverseComparator(naturalOrder)
        val equalReverse = ReverseComparator(naturalOrder)
        assertSame(naturalOrder, reverse.sourceComparator)
        assertTrue(reverse.compare("a", "b") > 0)
        assertEquals(reverse, equalReverse)
        assertEquals(reverse.hashCode(), equalReverse.hashCode())

        val byDescendingLength = StringLengthComparator()
        assertTrue(byDescendingLength.compare("long", "x") < 0)
        assertTrue(byDescendingLength.compare("x", "long") > 0)
        assertEquals(0, byDescendingLength.compare("aa", "bb"))
    }

    @Test
    fun beanPropertyComparatorKeepsNullOrderingEqualityAndFacadeBehavior() {
        val nullsFirstByInteger = Comparator<Any?> { left, right ->
            when {
                left === right -> 0
                left == null -> -1
                right == null -> 1
                else -> (left as Int).compareTo(right as Int)
            }
        }
        val comparator = BeanPropertyComparator(SampleBean::class.java, "count", nullsFirstByInteger)
        val equalComparator = BeanPropertyComparator(SampleBean::class.java, "count", nullsFirstByInteger)

        assertTrue(comparator.compare(SampleBean(1, "one"), SampleBean(2, "two")) < 0)
        assertTrue(comparator.compare(null, SampleBean(1, "one")) < 0)
        assertEquals(comparator, equalComparator)
        assertEquals(comparator.hashCode(), equalComparator.hashCode())
        assertNotEquals(
            comparator,
            BeanPropertyComparator(SampleBean::class.java, "count", Comparator<Any?> { _, _ -> 0 }),
        )

        val facadeComparator = GlazedLists.beanPropertyComparator(
            SampleBean::class.java,
            "count",
            nullsFirstByInteger,
        )
        assertTrue(facadeComparator.compare(SampleBean(2, "two"), SampleBean(1, "one")) > 0)
    }

    @Test
    fun functionListKeepsItsMappedResults() {
        val mappedSource = BasicEventList<String>().apply { addAll(listOf("a", "bbbb")) }
        val mapped = FunctionList(mappedSource, String::length)

        assertEquals(listOf(1, 4), mapped.toList())
        mappedSource[0] = "ccc"
        assertEquals(listOf(3, 4), mapped.toList())
    }

    @Test
    fun beanTableFormatStillBoxesPrimitivesAndKeepsReferenceTypes() {
        val format = BeanTableFormat(
            SampleBean::class.java,
            arrayOf("count", "label"),
            arrayOf("Count", "Label"),
        )

        assertEquals(Int::class.javaObjectType, format.getColumnClass(0))
        assertEquals(String::class.java, format.getColumnClass(1))
        assertEquals(7, format.getColumnValue(SampleBean(7, "seven"), 0))
        assertEquals("seven", format.getColumnValue(SampleBean(7, "seven"), 1))
    }

    @Test
    fun thresholdListKeepsInclusiveOrderingAtIntegerExtremes() {
        val source = BasicEventList<Int>().apply {
            addAll(listOf(Int.MAX_VALUE, 0, Int.MIN_VALUE, -1))
        }
        val threshold = ThresholdList(source, ThresholdList.Evaluator<Int> { it })

        threshold.lowerThreshold = -1
        threshold.upperThreshold = Int.MAX_VALUE

        assertEquals(listOf(-1, 0, Int.MAX_VALUE), threshold.toList())

        threshold.setHeadRange(1, 2)

        assertEquals(listOf(-1, 0), threshold.toList())
    }

    class SampleBean(val count: Int, val label: String)

    private data class NonComparable(val label: String) {
        override fun toString(): String = label
    }
}
