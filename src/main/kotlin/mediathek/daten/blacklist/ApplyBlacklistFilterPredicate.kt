package mediathek.daten.blacklist

import mediathek.daten.DatenFilm
import java.util.function.Predicate

class ApplyBlacklistFilterPredicate(blacklistRules: List<BlacklistRule>) : Predicate<DatenFilm> {
    private val mode = BlacklistMode.fromConfig()
    private val matcher = CompiledBlacklistMatcher(blacklistRules)

    override fun test(film: DatenFilm): Boolean =
        mode.keepFilm(matcher.matches(film))
}
