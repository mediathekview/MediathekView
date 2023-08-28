package feed

import org.junit.jupiter.api.Assertions
import org.junit.jupiter.api.Test
import java.net.URI

internal class RSSFeedParserTest {
    @Test
    fun mvwTest() {
        try {
            URI("https://mediathekviewweb.de/feed").toURL().openStream().use {
                val parser = RSSFeedParser()
                val feed = parser.readFeed(it)
                Assertions.assertNotNull(feed)
                //println(feed);
                Assertions.assertNotNull(feed!!.messages)
                Assertions.assertFalse(feed.messages.isEmpty())
                for (message in feed.messages) {
                    //println(message);
                    Assertions.assertNotNull(message)
                    Assertions.assertFalse(message.guid.isBlank())
                }
            }
        } catch (e: Exception) {
            Assertions.fail<Any>(e)
        }
    }
}