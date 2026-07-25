package ca.odell.glazedlists.matchers

import ca.odell.glazedlists.Filterator
import ca.odell.glazedlists.TextFilterator
import ca.odell.glazedlists.impl.matchers.*
import org.junit.jupiter.api.Assertions.*
import org.junit.jupiter.api.Test

internal class MatcherMigrationBehaviorTest {
    @Test
    fun searchFieldEqualityStillDependsOnlyOnItsName() {
        val firstFilterator = TextFilterator<String> { baseList, element -> baseList.add(element) }
        val secondFilterator = TextFilterator<String> { baseList, element -> baseList.add(element.uppercase()) }
        val first = SearchEngineTextMatcherEditor.Field("title", firstFilterator)
        val sameName = SearchEngineTextMatcherEditor.Field("title", secondFilterator)
        val differentName = SearchEngineTextMatcherEditor.Field("topic", firstFilterator)

        assertEquals(first, sameName)
        assertEquals(first.hashCode(), sameName.hashCode())
        assertNotEquals(first, differentName)
        assertEquals("title", first.name)
        assertSame(firstFilterator, first.textFilterator)
    }

    @Test
    fun setMatcherKeepsItsOwnCopyOfTheMatchSet() {
        val editor = SetMatcherEditor.create<String, String>(SetMatcherEditor.Mode.WHITELIST_EMPTY_MATCH_NONE) { it }
        val selected = mutableSetOf("one")

        editor.setMatchSet(selected)
        selected.clear()

        assertTrue(editor.matcher.matches("one"))
        assertFalse(editor.matcher.matches("two"))
    }

    @Test
    fun setMatcherModesKeepTheirEmptyAndPopulatedBehavior() {
        val blacklist = SetMatcherEditor.create<String, String>(SetMatcherEditor.Mode.BLACKLIST) { it }
        assertTrue(blacklist.matcher.matches("one"))
        blacklist.setMatchSet(setOf("one"))
        assertFalse(blacklist.matcher.matches("one"))
        assertTrue(blacklist.matcher.matches("two"))
        blacklist.setMatchSet(emptySet())
        assertTrue(blacklist.matcher.matches("one"))

        val whitelistNone = SetMatcherEditor.create<String, String>(
            SetMatcherEditor.Mode.WHITELIST_EMPTY_MATCH_NONE,
        ) { it }
        assertFalse(whitelistNone.matcher.matches("one"))
        whitelistNone.setMatchSet(setOf("one"))
        assertTrue(whitelistNone.matcher.matches("one"))
        assertFalse(whitelistNone.matcher.matches("two"))
        whitelistNone.setMatchSet(emptySet())
        assertFalse(whitelistNone.matcher.matches("one"))

        val whitelistAll = SetMatcherEditor.create<String, String>(
            SetMatcherEditor.Mode.WHITELIST_EMPTY_MATCH_ALL,
        ) { it }
        assertTrue(whitelistAll.matcher.matches("one"))
        whitelistAll.setMatchSet(setOf("one"))
        assertTrue(whitelistAll.matcher.matches("one"))
        assertFalse(whitelistAll.matcher.matches("two"))
        whitelistAll.setMatchSet(emptySet())
        assertTrue(whitelistAll.matcher.matches("two"))
    }

    @Test
    fun whitelistSetChangesKeepMatcherEventClassifications() {
        val editor = SetMatcherEditor.create<String, String>(
            SetMatcherEditor.Mode.WHITELIST_EMPTY_MATCH_ALL,
        ) { it }
        val eventTypes = mutableListOf<Int>()
        editor.addMatcherEditorListener { event -> eventTypes += event.type }

        editor.setMatchSet(setOf("one", "two"))
        editor.setMatchSet(setOf("one"))
        editor.setMatchSet(setOf("one", "two", "three"))
        editor.setMatchSet(setOf("four"))
        editor.setMatchSet(emptySet())
        editor.setMatchSet(emptySet())

        assertEquals(
            listOf(
                MatcherEditor.Event.CHANGED,
                MatcherEditor.Event.CONSTRAINED,
                MatcherEditor.Event.RELAXED,
                MatcherEditor.Event.CHANGED,
                MatcherEditor.Event.MATCH_ALL,
            ),
            eventTypes,
        )
    }

    @Test
    fun blacklistSetChangesInvertConstrainedAndRelaxedEvents() {
        val editor = SetMatcherEditor.create<String, String>(SetMatcherEditor.Mode.BLACKLIST) { it }
        val eventTypes = mutableListOf<Int>()
        editor.addMatcherEditorListener { event -> eventTypes += event.type }

        editor.setMatchSet(setOf("one", "two"))
        editor.setMatchSet(setOf("one"))
        editor.setMatchSet(setOf("one", "two", "three"))
        editor.setMatchSet(setOf("four"))
        editor.setMatchSet(emptySet())

        assertEquals(
            listOf(
                MatcherEditor.Event.CHANGED,
                MatcherEditor.Event.RELAXED,
                MatcherEditor.Event.CONSTRAINED,
                MatcherEditor.Event.CHANGED,
                MatcherEditor.Event.MATCH_ALL,
            ),
            eventTypes,
        )
    }

    @Test
    fun emptyWhitelistNoneSetFiresMatchNoneAfterPopulatedSet() {
        val editor = SetMatcherEditor.create<String, String>(
            SetMatcherEditor.Mode.WHITELIST_EMPTY_MATCH_NONE,
        ) { it }
        val eventTypes = mutableListOf<Int>()
        editor.addMatcherEditorListener { event -> eventTypes += event.type }

        editor.setMatchSet(setOf("one"))
        editor.setMatchSet(emptySet())

        assertEquals(listOf(MatcherEditor.Event.CHANGED, MatcherEditor.Event.MATCH_NONE), eventTypes)
    }

    @Test
    fun thresholdMatcherKeepsAllComparisonOperationsAndExtraction() {
        val editor = ThresholdMatcherEditor<String, Int>(
            3,
            ThresholdMatcherEditor.GREATER_THAN_OR_EQUAL,
            Comparator.naturalOrder(),
            String::length,
        )

        assertFalse(editor.matcher.matches("ab"))
        assertTrue(editor.matcher.matches("abc"))
        assertTrue(editor.matcher.matches("abcd"))

        editor.matchOperation = ThresholdMatcherEditor.LESS_THAN

        assertTrue(editor.matcher.matches("ab"))
        assertFalse(editor.matcher.matches("abc"))
    }

    @Test
    fun singletonMatchersKeepIdentityNullBehaviorAndRepresentations() {
        val trueMatcher = Matchers.trueMatcher<String?>()
        assertSame(trueMatcher, Matchers.trueMatcher<Int?>())
        assertTrue(trueMatcher.matches(null))
        assertTrue(trueMatcher.matches("anything"))

        val falseMatcher = Matchers.falseMatcher<String?>()
        assertSame(falseMatcher, Matchers.falseMatcher<Int?>())
        assertFalse(falseMatcher.matches(null))
        assertFalse(falseMatcher.matches("anything"))

        val nullMatcher = Matchers.isNull<String?>()
        assertSame(nullMatcher, Matchers.isNull<Int?>())
        assertTrue(nullMatcher.matches(null))
        assertFalse(nullMatcher.matches("anything"))
        assertEquals("[NullMatcher]", nullMatcher.toString())

        val notNullMatcher = Matchers.isNotNull<String?>()
        assertSame(notNullMatcher, Matchers.isNotNull<Int?>())
        assertFalse(notNullMatcher.matches(null))
        assertTrue(notNullMatcher.matches("anything"))
        assertEquals("[NotNullMatcher]", notNullMatcher.toString())

        val nonEmptyStringMatcher = Matchers.nonNullAndNonEmptyString()
        assertFalse(nonEmptyStringMatcher.matches(null))
        assertFalse(nonEmptyStringMatcher.matches(""))
        assertTrue(nonEmptyStringMatcher.matches("anything"))
    }

    @Test
    fun matcherCombinatorsKeepVarianceShortCircuitAndTypeBehavior() {
        val inspected = mutableListOf<String>()
        val nonEmpty = Matcher<CharSequence> {
            inspected += "nonEmpty"
            it.isNotEmpty()
        }
        val startsWithA = Matcher<String> {
            inspected += "startsWithA"
            it.startsWith('a')
        }

        val andMatcher = AndMatcher(nonEmpty, startsWithA)
        assertFalse(andMatcher.matches(""))
        assertEquals(listOf("nonEmpty"), inspected)
        inspected.clear()
        assertTrue(andMatcher.matches("apple"))
        assertEquals(listOf("nonEmpty", "startsWithA"), inspected)
        assertTrue(AndMatcher<String>().matches("anything"))

        inspected.clear()
        val orMatcher = OrMatcher(startsWithA, nonEmpty)
        assertTrue(orMatcher.matches("apple"))
        assertEquals(listOf("startsWithA"), inspected)
        inspected.clear()
        assertFalse(orMatcher.matches(""))
        assertEquals(listOf("startsWithA", "nonEmpty"), inspected)
        assertFalse(OrMatcher<String>().matches("anything"))

        val inverted = NotMatcher(startsWithA)
        assertFalse(inverted.matches("apple"))
        assertTrue(inverted.matches("pear"))
        assertEquals("[NotMatcher parent:$startsWithA]", inverted.toString())
        val typeMatcher = TypeMatcher<Any?>(Number::class.java, CharSequence::class.java)
        assertTrue(typeMatcher.matches(42))
        assertTrue(typeMatcher.matches("text"))
        assertFalse(typeMatcher.matches(Unit))
        assertFalse(typeMatcher.matches(null))

        assertTrue(Matchers.and(nonEmpty, startsWithA).matches("apple"))
        assertTrue(Matchers.or(startsWithA, nonEmpty).matches("pear"))
        assertTrue(Matchers.types<Any>(Number::class.java).matches(42))
        assertTrue(Matchers.invert(startsWithA).matches("pear"))
    }

    @Test
    fun beanPropertyMatcherKeepsReflectionNullAndFacadeBehavior() {
        val categoryMatcher = BeanPropertyMatcher(MatcherBean::class.java, "category", "news")
        assertTrue(categoryMatcher.matches(MatcherBean("news", emptyList())))
        assertFalse(categoryMatcher.matches(MatcherBean("sports", emptyList())))
        assertFalse(categoryMatcher.matches(null))

        val nullCategoryMatcher = BeanPropertyMatcher(MatcherBean::class.java, "category", null)
        assertTrue(nullCategoryMatcher.matches(MatcherBean(null, emptyList())))
        assertFalse(nullCategoryMatcher.matches(null))

        val facadeMatcher = Matchers.beanPropertyMatcher(MatcherBean::class.java, "category", "news")
        assertTrue(facadeMatcher.matches(MatcherBean("news", emptyList())))
    }

    @Test
    fun rangeMatcherKeepsInclusiveUnboundedExtractionAndNullBehavior() {
        val directMatcher = RangeMatcher<Int, Int?>(2, 4)
        assertFalse(directMatcher.matches(1))
        assertTrue(directMatcher.matches(2))
        assertTrue(directMatcher.matches(4))
        assertFalse(directMatcher.matches(5))
        assertTrue(directMatcher.matches(null))
        assertEquals("[RangeMatcher between 2 and 4]", directMatcher.toString())

        val noLowerBound = RangeMatcher<Int, Int>(null, 3)
        assertTrue(noLowerBound.matches(Int.MIN_VALUE))
        assertFalse(noLowerBound.matches(4))

        val scoreFilterator = Filterator<Int, MatcherBean> { values, bean ->
            values.addAll(bean.scores)
        }
        val extractedMatcher = RangeMatcher(2, 4, scoreFilterator)
        assertFalse(extractedMatcher.matches(MatcherBean("empty", emptyList())))
        assertFalse(extractedMatcher.matches(MatcherBean("outside", listOf(1, 5))))
        assertTrue(extractedMatcher.matches(MatcherBean("inside", listOf(1, 3, 5))))

        val facadeMatcher = Matchers.rangeMatcher(2, 4, scoreFilterator)
        assertTrue(facadeMatcher.matches(MatcherBean("inside", listOf(4))))
    }

    class MatcherBean(val category: String?, val scores: List<Int>)
}
