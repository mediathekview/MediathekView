package mediathek.daten.blacklist

import org.junit.jupiter.api.Assertions.assertFalse
import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Test
import kotlin.test.assertTrue

internal class BlacklistRuleTest {
    @Test
    fun test_distinct_with_duplicates() {
        val rule1 = BlacklistRule("ZDF")
        val rule2 = BlacklistRule("ZDF")
        val list = listOf(rule1, rule2)

        assertTrue { list.size == 2 }

        // remove duplicates
        val distinctList = list.stream().distinct().toList()
        assertTrue { distinctList.size == 1 }
    }

    @Test
    fun test_distinct_no_duplicates() {
        val rule1 = BlacklistRule("ZDF")
        val rule2 = BlacklistRule("ARD")
        val list = listOf(rule1, rule2)

        assertTrue { list.size == 2 }

        // try to remove duplicates
        val distinctList = list.stream().distinct().toList()

        // rule objects are not equal therefore we must have 2 if comparator works
        assertTrue { distinctList.size == 2 }
    }

    @Test
    fun test_comparator() {
        val rule1 = BlacklistRule("ARD", "thema1", "titel1", "thema_titel1")
        val rule2 = BlacklistRule("ARD1", "thema1", "titel1", "thema_titel1")
        // copy of rule1
        val rule3 = BlacklistRule("ARD", "thema1", "titel1", "thema_titel1")

        assertFalse { rule1 == rule2 }
        assertFalse { rule2 == rule3 }
        assertTrue { rule1 == rule3 }
    }

    @Test
    fun activeDefaultsToTrue() {
        assertTrue(BlacklistRule("ZDF").active)
    }

    @Test
    fun addRejectsDuplicateRules() {
        val list = ListeBlacklist()

        assertTrue(list.add(BlacklistRule("ZDF")))
        assertFalse(list.add(BlacklistRule("ZDF")))

        assertEquals(listOf(BlacklistRule("ZDF")), list)
    }

    @Test
    fun addRejectsDuplicateRulesWithDifferentActiveState() {
        val list = ListeBlacklist()

        assertTrue(list.add(BlacklistRule("ZDF", active = true)))
        assertFalse(list.add(BlacklistRule("ZDF", active = false)))

        assertEquals(listOf(BlacklistRule("ZDF", active = true)), list)
    }

    @Test
    fun addAllOnlyAddsUniqueRules() {
        val list = ListeBlacklist()

        assertTrue(
            list.addAll(
                listOf(
                    BlacklistRule("ZDF"),
                    BlacklistRule("ZDF"),
                    BlacklistRule("ARD"),
                )
            )
        )

        assertEquals(listOf(BlacklistRule("ZDF"), BlacklistRule("ARD")), list)
    }

    @Test
    fun addWithoutNotificationRejectsDuplicateRules() {
        val list = ListeBlacklist()

        assertTrue(list.addWithoutNotification(BlacklistRule("ZDF")))
        assertFalse(list.addWithoutNotification(BlacklistRule("ZDF")))

        assertEquals(listOf(BlacklistRule("ZDF")), list)
    }

    @Test
    fun addAllWithoutNotificationOnlyAddsUniqueRules() {
        val list = ListeBlacklist()

        assertTrue(
            list.addAllWithoutNotification(
                listOf(
                    BlacklistRule("ZDF"),
                    BlacklistRule("ZDF"),
                    BlacklistRule("ARD"),
                )
            )
        )

        assertEquals(listOf(BlacklistRule("ZDF"), BlacklistRule("ARD")), list)
    }

    @Test
    fun replaceAtIfUniqueRejectsDuplicateRule() {
        val list = ListeBlacklist()
        list.addWithoutNotification(BlacklistRule("ZDF"))
        list.addWithoutNotification(BlacklistRule("ARD"))

        assertFalse(list.replaceAtIfUnique(1, BlacklistRule("ZDF")))

        assertEquals(listOf(BlacklistRule("ZDF"), BlacklistRule("ARD")), list)
    }

    @Test
    fun replaceAtIfUniqueUpdatesRuleWhenUnique() {
        val list = ListeBlacklist()
        list.addWithoutNotification(BlacklistRule("ZDF"))
        list.addWithoutNotification(BlacklistRule("ARD"))

        assertTrue(list.replaceAtIfUnique(1, BlacklistRule("ORF")))

        assertEquals(listOf(BlacklistRule("ZDF"), BlacklistRule("ORF")), list)
    }

    @Test
    fun getReturnsRuleCopy() {
        val list = ListeBlacklist()
        list.addWithoutNotification(BlacklistRule("ZDF", active = false))

        val rule = list[0]
        rule.sender = "ARD"
        rule.active = true

        assertEquals(BlacklistRule("ZDF", active = false), list[0])
    }
}
