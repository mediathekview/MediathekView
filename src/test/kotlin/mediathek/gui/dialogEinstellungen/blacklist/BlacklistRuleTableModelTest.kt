package mediathek.gui.dialogEinstellungen.blacklist

import mediathek.daten.DatenFilm
import mediathek.daten.blacklist.BlacklistRule
import mediathek.daten.blacklist.ListeBlacklist
import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Assertions.assertFalse
import org.junit.jupiter.api.Assertions.assertTrue
import org.junit.jupiter.api.Test

class BlacklistRuleTableModelTest {
    @Test
    fun firstColumnShowsActiveStateAsBoolean() {
        val blacklist = ListeBlacklist()
        blacklist.addWithoutNotification(BlacklistRule("ARD", active = false))
        val model = BlacklistRuleTableModel(blacklist)

        assertEquals("aktiv", model.getColumnName(0))
        assertEquals(Boolean::class.javaObjectType, model.getColumnClass(0))
        assertEquals(false, model.getValueAt(0, 0))
    }

    @Test
    fun potentialCountsIncludeInactiveRules() {
        val blacklist = ListeBlacklist()
        blacklist.addWithoutNotification(BlacklistRule("ARD", titel = "tagesschau", active = false))
        val model = BlacklistRuleTableModel(blacklist)

        model.applyFilteredCounts(
            model.calculateFilteredCounts(
                listOf(film(sender = "ARD", title = "Tagesschau um acht"))
            )
        )

        assertEquals(1, model.getValueAt(0, 5))
    }

    @Test
    fun reportsZeroFilteredCountFromAppliedCounts() {
        val blacklist = ListeBlacklist()
        blacklist.addWithoutNotification(BlacklistRule("ARD"))
        blacklist.addWithoutNotification(BlacklistRule("ZDF"))
        val model = BlacklistRuleTableModel(blacklist)

        model.applyFilteredCounts(intArrayOf(0, 3))

        assertTrue(model.hasZeroFilteredCount(0))
        assertFalse(model.hasZeroFilteredCount(1))
    }

    @Test
    fun deactivatesOnlyActiveRulesWithZeroFilteredCount() {
        val blacklist = ListeBlacklist()
        blacklist.addWithoutNotification(BlacklistRule("ARD", active = true))
        blacklist.addWithoutNotification(BlacklistRule("ZDF", active = true))
        blacklist.addWithoutNotification(BlacklistRule("MDR", active = false))
        val model = BlacklistRuleTableModel(blacklist)
        model.applyFilteredCounts(intArrayOf(0, 4, 0))

        val changedRows = BlacklistRuleBulkActions.deactivateActiveRulesWithZeroFilteredCount(blacklist, model)

        assertEquals(listOf(0), changedRows)
        assertFalse(blacklist[0].active)
        assertTrue(blacklist[1].active)
        assertFalse(blacklist[2].active)
    }

    @Test
    fun removesAllRulesWithZeroFilteredCount() {
        val blacklist = ListeBlacklist()
        blacklist.addWithoutNotification(BlacklistRule("ARD", active = true))
        blacklist.addWithoutNotification(BlacklistRule("ZDF", active = true))
        blacklist.addWithoutNotification(BlacklistRule("MDR", active = false))
        val model = BlacklistRuleTableModel(blacklist)
        model.applyFilteredCounts(intArrayOf(0, 2, 0))

        val changed = BlacklistRuleBulkActions.removeRulesWithZeroFilteredCount(blacklist, model)

        assertTrue(changed)
        assertEquals(listOf(BlacklistRule("ZDF", active = true)), blacklist)
    }

    private fun film(sender: String, title: String): DatenFilm =
        DatenFilm().apply {
            this.sender = sender
            this.title = title
        }
}
