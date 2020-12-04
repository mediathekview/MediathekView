package mediathek.gui.actions.import_actions

import mediathek.config.Daten
import mediathek.daten.DatenAbo
import mediathek.daten.blacklist.BlacklistRule
import mediathek.gui.messages.ReplaceListChangedEvent
import mediathek.tool.ReplaceList
import org.apache.commons.lang3.tuple.ImmutableTriple
import org.apache.logging.log4j.LogManager
import java.io.FileInputStream
import java.io.IOException
import java.io.InputStreamReader
import java.nio.charset.StandardCharsets
import javax.xml.stream.XMLInputFactory
import javax.xml.stream.XMLStreamConstants
import javax.xml.stream.XMLStreamException
import javax.xml.stream.XMLStreamReader

class OldConfigFileImporter {
    private val daten: Daten = Daten.getInstance()
    private val inFactory: XMLInputFactory = XMLInputFactory.newInstance()

    @Throws(IOException::class, XMLStreamException::class)
    fun importAboBlacklist(
        datei: String,
        importAbo: Boolean,
        importBlacklist: Boolean,
        importReplaceList: Boolean
    ): ImmutableTriple<Int, Int, Int> {
        var foundAbos = 0
        var foundBlacklistEntries = 0
        var foundReplaceListEntries = 0
        var parser: XMLStreamReader? = null
        try {
            FileInputStream(datei).use { fis ->
                InputStreamReader(fis, StandardCharsets.UTF_8).use { isr ->
                    parser = inFactory.createXMLStreamReader(isr)
                    while (parser!!.hasNext()) {
                        val event = parser!!.next()
                        if (event == XMLStreamConstants.START_ELEMENT) {
                            if (importAbo && parser!!.localName == DatenAbo.TAG) {
                                if (importAboEntry(parser!!))
                                    foundAbos++
                            } else if (importBlacklist && parser!!.localName == BlacklistRule.TAG) {
                                if (importBlacklistEntry(parser!!))
                                    foundBlacklistEntries++
                            } else if (importReplaceList && parser!!.localName == ReplaceList.REPLACELIST) {
                                if (importReplaceList(parser!!))
                                    foundReplaceListEntries++
                            }
                        }
                    }
                }
            }
        } finally {
            if (parser != null) {
                try {
                    parser!!.close()
                } catch (ignored: XMLStreamException) {
                }
            }
        }

        if (foundAbos > 0)
            daten.listeAbo.aenderungMelden()
        if (foundBlacklistEntries > 0)
            daten.listeBlacklist.filterListAndNotifyListeners()
        if (foundReplaceListEntries > 0)
            daten.messageBus.publishAsync(ReplaceListChangedEvent())

        return ImmutableTriple(foundAbos, foundBlacklistEntries, foundReplaceListEntries)
    }

    private fun importAboEntry(parser: XMLStreamReader): Boolean {
        return try {
            val datenAbo = DatenAbo()
            datenAbo.readFromConfig(parser)
            daten.listeAbo.addAbo(datenAbo)
            true
        } catch (e: Exception) {
            logger.error("Error importing abo entry")
            false
        }
    }

    private fun importBlacklistEntry(parser: XMLStreamReader): Boolean {
        val blacklist = daten.listeBlacklist
        val blacklistRule = BlacklistRule()
        val success = get(parser, BlacklistRule.TAG, BlacklistRule.XML_NAMES, blacklistRule.arr)
        return if (success) {
            blacklist.addWithoutNotification(blacklistRule)
            true
        } else
            false
    }

    private fun importReplaceList(parser: XMLStreamReader): Boolean {
        val sa = arrayOfNulls<String>(ReplaceList.MAX_ELEM)
        val success = get(parser, ReplaceList.REPLACELIST, ReplaceList.COLUMN_NAMES, sa)
        return if (success) {
            ReplaceList.list.add(sa)
            true
        } else
            false
    }

    private operator fun get(
        parser: XMLStreamReader, xmlElem: String, xmlNames: Array<String>,
        strRet: Array<String?>
    ): Boolean {
        val maxElem = strRet.size
        for (i in 0 until maxElem) {
            if (strRet[i] == null) {
                strRet[i] = ""
            }
        }

        return try {
            while (parser.hasNext()) {
                val event = parser.next()
                if (event == XMLStreamConstants.END_ELEMENT) {
                    if (parser.localName == xmlElem) {
                        break
                    }
                }
                if (event == XMLStreamConstants.START_ELEMENT) {
                    for (i in 0 until maxElem) {
                        if (parser.localName == xmlNames[i]) {
                            strRet[i] = parser.elementText
                            break
                        }
                    }
                }
            }
            true
        } catch (ex: Exception) {
            logger.error("get", ex)
            false
        }
    }

    companion object {
        private val logger = LogManager.getLogger()
    }

    init {
        inFactory.setProperty(XMLInputFactory.IS_COALESCING, java.lang.Boolean.FALSE)
    }
}