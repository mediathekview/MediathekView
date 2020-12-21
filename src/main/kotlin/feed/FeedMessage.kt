package feed

/*
 * A RSS message
 */
data class FeedMessage(
    val title: String, val description: String, val link: String, val author: String,
    val guid: String
) {
    override fun toString(): String {
        return ("FeedMessage [title=" + title + ", description=" + description
                + ", link=" + link + ", author=" + author + ", guid=" + guid
                + "]")
    }
}