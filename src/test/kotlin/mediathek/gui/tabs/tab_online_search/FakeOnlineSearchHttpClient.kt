package mediathek.gui.tabs.tab_online_search

class FakeOnlineSearchHttpClient(
    private val responses: Map<String, String>,
) : OnlineSearchHttpClient {
    val requestedUrls = ArrayList<String>()
    val requestedHeaders = ArrayList<Map<String, String>>()

    override suspend fun get(url: String, headers: Map<String, String>): String {
        requestedUrls.add(url)
        requestedHeaders.add(headers)
        return responses[url] ?: error("No fake response registered for $url")
    }
}
