package mediathek.config.application

import org.apache.commons.configuration2.XMLConfiguration

class ApplicationOnlineSearchConfiguration(
    private val config: XMLConfiguration,
) {
    var onlineSearchTabVisible: Boolean
        get() = config.getBoolean(APPLICATION_UI_ONLINE_SEARCH_SHOW, true)
        set(newValue) {
            config.setProperty(APPLICATION_UI_ONLINE_SEARCH_SHOW, newValue)
        }

    var ardSearchHistory: String
        get() = config.getString(APPLICATION_UI_ONLINE_SEARCH_ARD_SEARCH_HISTORY, "[]")
        set(newValue) {
            config.setProperty(APPLICATION_UI_ONLINE_SEARCH_ARD_SEARCH_HISTORY, newValue)
        }

    var ardUrlHistory: String
        get() = config.getString(APPLICATION_UI_ONLINE_SEARCH_ARD_URL_HISTORY, "[]")
        set(newValue) {
            config.setProperty(APPLICATION_UI_ONLINE_SEARCH_ARD_URL_HISTORY, newValue)
        }

    var zdfSearchHistory: String
        get() = config.getString(APPLICATION_UI_ONLINE_SEARCH_ZDF_SEARCH_HISTORY, "[]")
        set(newValue) {
            config.setProperty(APPLICATION_UI_ONLINE_SEARCH_ZDF_SEARCH_HISTORY, newValue)
        }

    var zdfUrlHistory: String
        get() = config.getString(APPLICATION_UI_ONLINE_SEARCH_ZDF_URL_HISTORY, "[]")
        set(newValue) {
            config.setProperty(APPLICATION_UI_ONLINE_SEARCH_ZDF_URL_HISTORY, newValue)
        }

    var arteSearchHistory: String
        get() = config.getString(APPLICATION_UI_ONLINE_SEARCH_ARTE_SEARCH_HISTORY, "[]")
        set(newValue) {
            config.setProperty(APPLICATION_UI_ONLINE_SEARCH_ARTE_SEARCH_HISTORY, newValue)
        }

    var arteUrlHistory: String
        get() = config.getString(APPLICATION_UI_ONLINE_SEARCH_ARTE_URL_HISTORY, "[]")
        set(newValue) {
            config.setProperty(APPLICATION_UI_ONLINE_SEARCH_ARTE_URL_HISTORY, newValue)
        }

    var arteRequestDelayMillis: Long
        get() = config.getLong(APPLICATION_UI_ONLINE_SEARCH_ARTE_REQUEST_DELAY_MILLIS, 0L)
        set(newValue) {
            config.setProperty(APPLICATION_UI_ONLINE_SEARCH_ARTE_REQUEST_DELAY_MILLIS, newValue)
        }

    private companion object {
        @field:ApplicationConfigKey
        private const val APPLICATION_UI_ONLINE_SEARCH_SHOW = "application.ui.online_search.show"
        @field:ApplicationConfigKey
        private const val APPLICATION_UI_ONLINE_SEARCH_ARD_SEARCH_HISTORY = "application.ui.online_search.ard.search.history"
        @field:ApplicationConfigKey
        private const val APPLICATION_UI_ONLINE_SEARCH_ARD_URL_HISTORY = "application.ui.online_search.ard.url.history"
        @field:ApplicationConfigKey
        private const val APPLICATION_UI_ONLINE_SEARCH_ZDF_SEARCH_HISTORY = "application.ui.online_search.zdf.search.history"
        @field:ApplicationConfigKey
        private const val APPLICATION_UI_ONLINE_SEARCH_ZDF_URL_HISTORY = "application.ui.online_search.zdf.url.history"
        @field:ApplicationConfigKey
        private const val APPLICATION_UI_ONLINE_SEARCH_ARTE_SEARCH_HISTORY = "application.ui.online_search.arte.search.history"
        @field:ApplicationConfigKey
        private const val APPLICATION_UI_ONLINE_SEARCH_ARTE_URL_HISTORY = "application.ui.online_search.arte.url.history"
        @field:ApplicationConfigKey
        private const val APPLICATION_UI_ONLINE_SEARCH_ARTE_REQUEST_DELAY_MILLIS =
            "application.ui.online_search.arte.request.delay.millis"
    }
}
