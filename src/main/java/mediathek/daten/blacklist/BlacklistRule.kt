package mediathek.daten.blacklist

import mediathek.daten.blacklist.BlacklistTags.Companion.fromXmlTag
import mediathek.tool.Filter
import org.apache.logging.log4j.LogManager
import java.util.*
import java.util.function.BiConsumer
import javax.xml.stream.XMLStreamConstants
import javax.xml.stream.XMLStreamException
import javax.xml.stream.XMLStreamReader
import javax.xml.stream.XMLStreamWriter

data class BlacklistRule(
    var sender: String = "",
    var thema: String = "",
    var titel: String = "",
    var thema_titel: String = ""
) {
    private var patternTitle = true
    private var patternThema = true

    fun hasTitlePattern(): Boolean {
        return patternTitle
    }

    fun hasThemaPattern(): Boolean {
        return patternThema
    }

    fun convertToLowerCase() {
        titel = titel.lowercase(Locale.getDefault())
        thema_titel = thema_titel.lowercase(Locale.getDefault())
    }

    /**
     * Determine if we have regexp patterns somewhere and also precompile the pattern into the cache to speed up
     * operations a bit.
     */
    fun checkPatterns() {
        patternTitle = Filter.isPattern(titel)
        patternThema = Filter.isPattern(thema_titel)

        //precompile and cache the regexp patterns if needed...
        if (patternTitle)
            Filter.makePattern(titel)

        if (patternThema)
            Filter.makePattern(thema_titel)
    }

    /**
     * Write all data to config.
     *
     * @param writer the writer used.
     */
    fun writeToConfig(writer: XMLStreamWriter) {
        val writeElement =
            BiConsumer { tagName: String, content: String ->
                if (content.isNotEmpty()) {
                    try {
                        writer.writeCharacters("\t")
                        writer.writeStartElement(tagName)
                        writer.writeCharacters(content)
                        writer.writeEndElement()
                        writer.writeCharacters("\n")
                    } catch (e: XMLStreamException) {
                        logger.error("writeElement failed", e)
                    }
                }
            }
        try {
            writer.writeStartElement(TAG)
            writer.writeCharacters("\n")
            writeElement.accept(BlacklistTags.SENDER.xmlName, sender)
            writeElement.accept(BlacklistTags.THEMA.xmlName, thema)
            writeElement.accept(BlacklistTags.TITEL.xmlName, titel)
            writeElement.accept(BlacklistTags.THEMA_TITEL.xmlName, thema_titel)
            writer.writeEndElement()
            writer.writeCharacters("\n")
        } catch (ex: Exception) {
            logger.error("writeToConfig", ex)
        }
    }

    /**
     * Read data from config.
     *
     * @param parser the xml file parser.
     */
    @Throws(XMLStreamException::class)
    fun readFromConfig(parser: XMLStreamReader) {
        while (parser.hasNext()) {
            val event = parser.next()
            if (event == XMLStreamConstants.END_ELEMENT && parser.localName == TAG) {
                break
            }

            if (event == XMLStreamConstants.START_ELEMENT) {
                fromXmlTag(parser.localName).ifPresent { tag: BlacklistTags ->
                    try {
                        val text = parser.elementText
                        when (tag) {
                            BlacklistTags.SENDER -> sender = text
                            BlacklistTags.THEMA -> thema = text
                            BlacklistTags.TITEL -> titel = text
                            BlacklistTags.THEMA_TITEL -> thema_titel = text
                        }
                    } catch (e: XMLStreamException) {
                        logger.error("Error reading blacklist rule entry", e)
                    }
                }
            }
        }
    }

    companion object {
        const val TAG = "Blacklist"
        private val logger = LogManager.getLogger()
    }
}
