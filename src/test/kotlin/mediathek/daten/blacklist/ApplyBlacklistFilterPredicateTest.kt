package mediathek.daten.blacklist

import mediathek.config.Daten
import mediathek.config.application.ApplicationConfiguration
import mediathek.daten.DatenFilm
import org.junit.jupiter.api.AfterEach
import org.junit.jupiter.api.Assertions.assertFalse
import org.junit.jupiter.api.Assertions.assertTrue
import org.junit.jupiter.api.BeforeEach
import org.junit.jupiter.api.Test

internal class ApplyBlacklistFilterPredicateTest {
    private var previousWhitelistMode = false

    @BeforeEach
    fun setUp() {
        previousWhitelistMode = ApplicationConfiguration.getInstance().blacklistWhitelistMode
        ApplicationConfiguration.getInstance().blacklistWhitelistMode = false
    }

    @AfterEach
    fun tearDown() {
        ApplicationConfiguration.getInstance().blacklistWhitelistMode = previousWhitelistMode
    }

    @Test
    fun filtersRulesByExactSenderAndCaseInsensitiveThemaBeforeTextMatching() {
        val predicate = ApplyBlacklistFilterPredicate(
            listOf(
                BlacklistRule("ZDF", "Culture", "", ""),
                BlacklistRule("ARD", "Culture", "", ""),
                BlacklistRule("ARD", "News", "tagesschau", ""),
            )
        )

        assertFalse(predicate.test(film(sender = "ARD", thema = "news", title = "Tagesschau um acht")))
        assertTrue(predicate.test(film(sender = "ZDF", thema = "news", title = "Tagesschau um acht")))
        assertTrue(predicate.test(film(sender = "ARD", thema = "Sport", title = "Tagesschau um acht")))
    }

    @Test
    fun whitelistModeUsesSameMatcherWithInvertedPolicy() {
        ApplicationConfiguration.getInstance().blacklistWhitelistMode = true
        val predicate = ApplyBlacklistFilterPredicate(
            listOf(BlacklistRule("ARD", "News", "tagesschau", ""))
        )

        assertTrue(predicate.test(film(sender = "ARD", thema = "news", title = "Tagesschau um acht")))
        assertFalse(predicate.test(film(sender = "ZDF", thema = "news", title = "Heute Journal")))
    }

    @Test
    fun inactiveBlacklistRuleDoesNotFilterFilm() {
        val predicate = ApplyBlacklistFilterPredicate(
            listOf(BlacklistRule("ARD", "News", "tagesschau", "", active = false))
        )

        assertTrue(predicate.test(film(sender = "ARD", thema = "news", title = "Tagesschau um acht")))
    }

    @Test
    fun inactiveWhitelistRuleDoesNotKeepFilm() {
        ApplicationConfiguration.getInstance().blacklistWhitelistMode = true
        val predicate = ApplyBlacklistFilterPredicate(
            listOf(BlacklistRule("ARD", "News", "tagesschau", "", active = false))
        )

        assertFalse(predicate.test(film(sender = "ARD", thema = "news", title = "Tagesschau um acht")))
    }

    @Test
    fun downloadsPredicateUsesCompiledSnapshot() {
        val blacklist = BlacklistServices(Daten().filmCatalog)
        blacklist.rules.addWithoutNotification(BlacklistRule("ARD", "", "tagesschau", ""))

        val predicate = blacklist.createDownloadsPredicate()

        assertFalse(predicate.test(film(sender = "ARD", thema = "News", title = "Tagesschau um acht")))
        assertTrue(predicate.test(film(sender = "ZDF", thema = "News", title = "Heute Journal")))

        blacklist.rules.addWithoutNotification(BlacklistRule("ZDF", "", "", ""))

        assertTrue(predicate.test(film(sender = "ZDF", thema = "News", title = "Heute Journal")))
        assertFalse(blacklist.createDownloadsPredicate().test(film(sender = "ZDF", thema = "News", title = "Heute Journal")))
    }

    @Test
    fun downloadsPredicateIgnoresInactiveRules() {
        val blacklist = BlacklistServices(Daten().filmCatalog)
        blacklist.rules.addWithoutNotification(BlacklistRule("ARD", "", "tagesschau", "", active = false))

        val predicate = blacklist.createDownloadsPredicate()

        assertTrue(predicate.test(film(sender = "ARD", thema = "News", title = "Tagesschau um acht")))
    }

    private fun film(sender: String, thema: String, title: String): DatenFilm =
        DatenFilm().apply {
            this.sender = sender
            this.thema = thema
            this.title = title
        }
}
