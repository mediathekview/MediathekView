package mediathek.gui.tabs.tab_online_search

import mediathek.config.application.ApplicationConfiguration

interface OnlineSearchHistoryStore {
    fun readQueryHistory(provider: OnlineSearchProvider): OnlineSearchHistory
    fun readUrlHistory(provider: OnlineSearchProvider): OnlineSearchHistory
    fun writeQueryHistory(provider: OnlineSearchProvider, history: OnlineSearchHistory)
    fun writeUrlHistory(provider: OnlineSearchProvider, history: OnlineSearchHistory)
}

object ApplicationOnlineSearchHistoryStore : OnlineSearchHistoryStore {
    override fun readQueryHistory(provider: OnlineSearchProvider): OnlineSearchHistory =
        OnlineSearchHistory.decode(configuration.queryHistory(provider))

    override fun readUrlHistory(provider: OnlineSearchProvider): OnlineSearchHistory =
        OnlineSearchHistory.decode(configuration.urlHistory(provider))

    override fun writeQueryHistory(provider: OnlineSearchProvider, history: OnlineSearchHistory) {
        configuration.setQueryHistory(provider, history.encode())
    }

    override fun writeUrlHistory(provider: OnlineSearchProvider, history: OnlineSearchHistory) {
        configuration.setUrlHistory(provider, history.encode())
    }

    private val configuration: ApplicationConfiguration
        get() = ApplicationConfiguration.getInstance()

    private fun ApplicationConfiguration.queryHistory(provider: OnlineSearchProvider): String = when (provider) {
        OnlineSearchProvider.ARD -> onlineSearchArdSearchHistory
        OnlineSearchProvider.ZDF -> onlineSearchZdfSearchHistory
        OnlineSearchProvider.ARTE -> onlineSearchArteSearchHistory
    }

    private fun ApplicationConfiguration.urlHistory(provider: OnlineSearchProvider): String = when (provider) {
        OnlineSearchProvider.ARD -> onlineSearchArdUrlHistory
        OnlineSearchProvider.ZDF -> onlineSearchZdfUrlHistory
        OnlineSearchProvider.ARTE -> onlineSearchArteUrlHistory
    }

    private fun ApplicationConfiguration.setQueryHistory(provider: OnlineSearchProvider, value: String) {
        when (provider) {
            OnlineSearchProvider.ARD -> onlineSearchArdSearchHistory = value
            OnlineSearchProvider.ZDF -> onlineSearchZdfSearchHistory = value
            OnlineSearchProvider.ARTE -> onlineSearchArteSearchHistory = value
        }
    }

    private fun ApplicationConfiguration.setUrlHistory(provider: OnlineSearchProvider, value: String) {
        when (provider) {
            OnlineSearchProvider.ARD -> onlineSearchArdUrlHistory = value
            OnlineSearchProvider.ZDF -> onlineSearchZdfUrlHistory = value
            OnlineSearchProvider.ARTE -> onlineSearchArteUrlHistory = value
        }
    }
}
