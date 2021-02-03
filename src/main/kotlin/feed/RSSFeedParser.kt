package feed

import java.io.InputStream
import javax.xml.stream.XMLEventReader
import javax.xml.stream.XMLInputFactory
import javax.xml.stream.XMLStreamException
import javax.xml.stream.events.Characters

class RSSFeedParser {
    @Throws(XMLStreamException::class)
    fun readFeed(inputStream: InputStream?): Feed? {
        val inputFactory = XMLInputFactory.newInstance()
        var feed: Feed? = null
        var eventReader: XMLEventReader? = null
        try {
            var isFeedHeader = true
            var description = ""
            var title = ""
            var link = ""
            var language = ""
            var copyright = ""
            var author = ""
            var pubdate = ""
            var guid = ""
            eventReader = inputFactory.createXMLEventReader(inputStream)

            while (eventReader.hasNext()) {
                val event = eventReader.nextEvent()
                if (event.isStartElement) {
                    when (event.asStartElement().name.localPart) {
                        ITEM -> {
                            if (isFeedHeader) {
                                isFeedHeader = false
                                feed = Feed(title, link, description, language, copyright, pubdate)
                            }
                            eventReader.nextEvent()
                        }
                        TITLE -> title = getCharacterData(eventReader)
                        DESCRIPTION -> description = getCharacterData(eventReader)
                        LINK -> link = getCharacterData(eventReader)
                        GUID -> guid = getCharacterData(eventReader)
                        LANGUAGE -> language = getCharacterData(eventReader)
                        AUTHOR -> author = getCharacterData(eventReader)
                        PUB_DATE -> pubdate = getCharacterData(eventReader)
                        COPYRIGHT -> copyright = getCharacterData(eventReader)
                    }
                } else if (event.isEndElement) {
                    if (event.asEndElement().name.localPart == ITEM) {
                        feed?.messages?.add(FeedMessage(title, description, link, author, guid))
                        eventReader.nextEvent()
                        continue
                    }
                }
            }
        } finally {
            if (eventReader != null) {
                try {
                    eventReader.close()
                } catch (ignored: XMLStreamException) {
                }
            }
        }
        return feed
    }

    @Throws(XMLStreamException::class)
    private fun getCharacterData(eventReader: XMLEventReader): String {
        var result = ""
        val event = eventReader.nextEvent()
        if (event is Characters) {
            result = event.asCharacters().data
        }
        return result
    }

    companion object {
        private const val TITLE = "title"
        private const val DESCRIPTION = "description"
        private const val LANGUAGE = "language"
        private const val COPYRIGHT = "copyright"
        private const val LINK = "link"
        private const val AUTHOR = "author"
        private const val ITEM = "item"
        private const val PUB_DATE = "pubDate"
        private const val GUID = "guid"
    }
}