package mediathek.gui.tabs.tab_film

enum class OnlineSearchProviders(private val displayName: String, val queryUrl: String) {
    AMAZON("Amazon", "https://www.amazon.de/s?k="),
    BING("Bing", "https://www.bing.com/search?q="),
    DUCKDUCKGO("DuckDuckGo", "https://duckduckgo.com/?q="),
    GOOGLE("Google", "https://www.google.de/search?q="),
    STARTPAGE("Startpage", "https://www.startpage.com/do/search?q="),
    YOUTUBE("YouTube", "https://www.youtube.com/results?search_query=");

    override fun toString(): String {
        return displayName
    }
}