package ca.odell.glazedlists.matchers

import org.junit.jupiter.api.Assertions.*
import org.junit.jupiter.api.Test
import java.beans.PropertyChangeEvent

internal class MatchersFacadeMigrationBehaviorTest {
    private val evenNumber = Matcher<Number> { it.toInt() % 2 == 0 }

    @Test
    fun collectionUtilitiesKeepContravarianceMutationAndResultIdentity() {
        val numbers = mutableListOf(1, 2, 3, 4)

        assertEquals(2, Matchers.count(numbers, evenNumber))
        assertTrue(Matchers.contains(numbers, evenNumber))
        assertEquals(1, Matchers.indexOf(numbers, evenNumber))

        val selected = Matchers.select(numbers, evenNumber)
        assertEquals(listOf(2, 4), selected)

        val destination: MutableCollection<Number> = mutableListOf(10)
        assertSame(destination, Matchers.select(numbers, evenNumber, destination))
        assertEquals(listOf(10, 2, 4), destination)

        assertTrue(Matchers.filter(numbers, evenNumber))
        assertEquals(listOf(2, 4), numbers)
        assertFalse(Matchers.filter(numbers, evenNumber))
    }

    @Test
    fun arraySelectionKeepsRuntimeComponentTypeAndOrder() {
        val values = arrayOf("a", "bb", "ccc")

        val selected = Matchers.select(values, Matcher<CharSequence> { it.length > 1 })

        assertArrayEquals(arrayOf("bb", "ccc"), selected)
        assertSame(values.javaClass, selected.javaClass)
    }

    @Test
    fun propertyEventMatcherKeepsIncludeAndExcludeBehavior() {
        val nameEvent = PropertyChangeEvent(this, "name", "old", "new")
        val ageEvent = PropertyChangeEvent(this, "age", 1, 2)

        val included = Matchers.propertyEventNameMatcher(true, "name")
        assertTrue(included.matches(nameEvent))
        assertFalse(included.matches(ageEvent))

        val excluded = Matchers.propertyEventNameMatcher(false, "name")
        assertFalse(excluded.matches(nameEvent))
        assertTrue(excluded.matches(ageEvent))
    }

    @Test
    fun editorProxyAndDirectRangeFactoriesKeepDelegating() {
        val source = TestMatcherEditor<String>()
        val proxy = Matchers.weakReferenceProxy(source)

        assertNotSame(source, proxy)
        assertSame(source.matcher, proxy.matcher)

        val range = Matchers.rangeMatcher<Int, Int>(2, 4)
        assertFalse(range.matches(1))
        assertTrue(range.matches(2))
        assertTrue(range.matches(4))
        assertFalse(range.matches(5))
    }

    private class TestMatcherEditor<E> : AbstractMatcherEditor<E>()
}
