package feed

class Feed(
    val title: String, val link: String, val description: String, val language: String,
    val copyright: String, val pubDate: String
) {
    val messages: ArrayList<FeedMessage> = ArrayList()
    override fun toString(): String {
        return ("Feed [copyright=" + copyright + ", description=" + description
                + ", language=" + language + ", link=" + link + ", pubDate="
                + pubDate + ", title=" + title + "]")
    }
}