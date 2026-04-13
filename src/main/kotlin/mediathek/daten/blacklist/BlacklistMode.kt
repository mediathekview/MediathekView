package mediathek.daten.blacklist

import mediathek.config.MVConfig

enum class BlacklistMode {
    BLACKLIST,
    WHITELIST;

    fun keepFilm(ruleMatched: Boolean): Boolean =
        when (this) {
            BLACKLIST -> !ruleMatched
            WHITELIST -> ruleMatched
        }

    companion object {
        @JvmStatic
        fun fromConfig(): BlacklistMode =
            if (MVConfig.get(MVConfig.Configs.SYSTEM_BLACKLIST_IST_WHITELIST).toBoolean()) {
                WHITELIST
            } else {
                BLACKLIST
            }
    }
}
