package feed

import org.junit.jupiter.api.Assertions
import org.junit.jupiter.api.Test
import java.io.File

internal class RSSFeedParserTest {
    @Test
    fun mvwTest() {
        try {
            File("src/test/resources/rss/mediathekviewweb.xml").inputStream().use {
                val parser = RSSFeedParser()
                val feed = parser.readFeed(it)
                Assertions.assertNotNull(feed)
                //println(feed);
                Assertions.assertNotNull(feed!!.messages)
                Assertions.assertFalse(feed.messages.isEmpty())
                for (message in feed.messages) {
                    //println(message);
                    Assertions.assertNotNull(message)
                    Assertions.assertTrue(message.author.isBlank())
                    Assertions.assertFalse(message.guid.isBlank())
                    Assertions.assertFalse(message.link.isBlank())
                    Assertions.assertFalse(message.description.isBlank())
                    Assertions.assertFalse(message.title.isBlank())
                    Assertions.assertTrue(message.guid.endsWith('='))
                }
            }
        } catch (e: Exception) {
            Assertions.fail<Any>(e)
        }
    }
}