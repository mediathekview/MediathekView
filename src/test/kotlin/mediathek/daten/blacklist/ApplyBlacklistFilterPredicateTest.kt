package mediathek.daten.blacklist

import mediathek.config.MVConfig
import mediathek.daten.DatenFilm
import org.junit.jupiter.api.AfterEach
import org.junit.jupiter.api.Assertions.assertFalse
import org.junit.jupiter.api.Assertions.assertTrue
import org.junit.jupiter.api.BeforeEach
import org.junit.jupiter.api.Test

internal class ApplyBlacklistFilterPredicateTest {
    private var previousWhitelistMode = ""

    @BeforeEach
    fun setUp() {
        previousWhitelistMode = MVConfig.get(MVConfig.Configs.SYSTEM_BLACKLIST_IST_WHITELIST)
        MVConfig.add(MVConfig.Configs.SYSTEM_BLACKLIST_IST_WHITELIST, "false")
    }

    @AfterEach
    fun tearDown() {
        MVConfig.add(MVConfig.Configs.SYSTEM_BLACKLIST_IST_WHITELIST, previousWhitelistMode)
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
        MVConfig.add(MVConfig.Configs.SYSTEM_BLACKLIST_IST_WHITELIST, "true")
        val predicate = ApplyBlacklistFilterPredicate(
            listOf(BlacklistRule("ARD", "News", "tagesschau", ""))
        )

        assertTrue(predicate.test(film(sender = "ARD", thema = "news", title = "Tagesschau um acht")))
        assertFalse(predicate.test(film(sender = "ZDF", thema = "news", title = "Heute Journal")))
    }

    @Test
    fun downloadsPredicateUsesCompiledSnapshot() {
        val blacklist = ListeBlacklist()
        blacklist.addWithoutNotification(BlacklistRule("ARD", "", "tagesschau", ""))

        val predicate = blacklist.createDownloadsPredicate()

        assertFalse(predicate.test(film(sender = "ARD", thema = "News", title = "Tagesschau um acht")))
        assertTrue(predicate.test(film(sender = "ZDF", thema = "News", title = "Heute Journal")))

        blacklist.addWithoutNotification(BlacklistRule("ZDF", "", "", ""))

        assertTrue(predicate.test(film(sender = "ZDF", thema = "News", title = "Heute Journal")))
        assertFalse(blacklist.createDownloadsPredicate().test(film(sender = "ZDF", thema = "News", title = "Heute Journal")))
    }

    private fun film(sender: String, thema: String, title: String): DatenFilm =
        DatenFilm().apply {
            this.sender = sender
            this.thema = thema
            this.title = title
        }
}
