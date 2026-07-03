package mediathek.daten

import mediathek.config.Konstanten
import mediathek.tool.NetUtils
import mediathek.tool.http.MVHttpClient
import mediathek.tool.models.NonEditableTableModel
import okhttp3.Request
import org.apache.commons.lang3.SystemUtils
import org.apache.logging.log4j.LogManager
import java.io.FileInputStream
import java.io.IOException
import java.io.InputStreamReader
import java.io.StringReader
import java.net.ConnectException
import java.net.UnknownHostException
import java.nio.charset.StandardCharsets
import javax.swing.JFrame
import javax.swing.table.TableModel
import javax.xml.stream.XMLInputFactory
import javax.xml.stream.XMLStreamConstants
import javax.xml.stream.XMLStreamException
import javax.xml.stream.XMLStreamReader

class ListePsetVorlagen : ArrayList<Array<String>>() {
    fun createModel(bs: String): TableModel {
        if (isEmpty()) {
            return NonEditableTableModel(emptyArray(), programGroupColumnNames())
        }

        val rows = if (bs.isNotEmpty()) {
            filter { row -> row[PGR_BS_NR].contains(bs) }
        } else {
            this
        }
        val data = Array(rows.size) { index ->
            Array<Any?>(PGR_MAX_ELEM) { column -> rows[index][column] }
        }
        return NonEditableTableModel(data, programGroupColumnNames())
    }

    fun loadListOfSets(): Boolean {
        try {
            clear()

            val inFactory = xmlInputFactory()
            val url = requireNotNull(Konstanten.URL_MEDIATHEKVIEW_RESOURCES.resolve(Konstanten.PSET_PROGRAM_GROUP_LIST_PATH))
            val request = Request.Builder().url(url).get().build()
            MVHttpClient.httpClient.newCall(request).execute().use { response ->
                response.body.use { body ->
                    if (!response.isSuccessful) {
                        return false
                    }

                    body.byteStream().use { input ->
                        InputStreamReader(input, StandardCharsets.UTF_8).use { reader ->
                            inFactory.createXMLStreamReader(reader).use { parser ->
                                while (parser.hasNext()) {
                                    if (parser.next() == XMLStreamConstants.START_ELEMENT && parser.localName == PGR) {
                                        val programGroup = Array(PGR_MAX_ELEM) { "" }
                                        get(parser, PGR, PGR_COLUMN_NAMES, programGroup)
                                        if (programGroup[PGR_URL_NR].isNotEmpty()) {
                                            add(programGroup)
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
        } catch (_: UnknownHostException) {
            return false
        } catch (_: ConnectException) {
            return false
        } catch (ex: Exception) {
            logger.error("loadListOfSets()", ex)
            return false
        }

        return true
    }

    companion object {
        private const val BS_WIN_64 = "Windows-64Bit"
        private const val BS_LINUX = "Linux"
        private const val BS_MAC = "Mac"

        val BS: Array<String> = arrayOf("", BS_WIN_64, BS_LINUX, BS_MAC)

        const val PGR = "Vorlage"
        const val PGR_NAME = "Name"
        const val PGR_NAME_NR = 0
        const val PGR_BESCHREIBUNG = "Beschreibung"
        const val PGR_BESCHREIBUNG_NR = 1
        const val PGR_VERSION = "Version"
        const val PGR_VERSION_NR = 2
        const val PGR_BS = "Bs"
        const val PGR_BS_NR = 3
        const val PGR_URL = "URL"
        const val PGR_URL_NR = 4
        const val PGR_INFO = "Info"
        const val PGR_INFO_NR = 5
        const val PGR_MAX_ELEM = 6

        val PGR_COLUMN_NAMES: Array<String> = arrayOf(PGR_NAME, PGR_BESCHREIBUNG, PGR_VERSION, PGR_BS, PGR_URL, PGR_INFO)

        private val logger = LogManager.getLogger(ListePsetVorlagen::class.java)

        fun getStandarset(parent: JFrame?, replaceMuster: Boolean): ListePset? {
            var listePset: ListePset? = null
            var vorlage: Array<String>? = null
            val listePsetVorlagen = ListePsetVorlagen()
            if (listePsetVorlagen.loadListOfSets()) {
                for (row in listePsetVorlagen) {
                    if (row[PGR_NAME_NR].equals("Standardset ${getOperatingSystemString()}", ignoreCase = true)) {
                        vorlage = row
                        break
                    }
                }
                if (vorlage != null && vorlage[PGR_URL_NR].isNotEmpty()) {
                    listePset = importPsetFile(vorlage[PGR_URL_NR], true)
                    if (listePset != null) {
                        listePset.version = vorlage[PGR_VERSION_NR]
                    }
                }
            }
            if (listePset == null) {
                // dann nehmen wir halt die im jar-File
                // liefert das Standard Programmset für das entsprechende BS
                // Standardgruppen laden
                listePset = importPset(getLocalPsetTemplate(), true)
            }

            if (replaceMuster && listePset != null) {
                // damit die Variablen ersetzt werden
                ProgramSetTemplateResolver.replaceTemplates(parent, listePset)
            }
            return listePset
        }

        fun importPsetFile(dateiUrl: String, log: Boolean): ListePset? =
            try {
                if (NetUtils.isUrl(dateiUrl)) {
                    val request = Request.Builder().url(dateiUrl).get().build()
                    MVHttpClient.httpClient.newCall(request).execute().use { response ->
                        response.body.use { body ->
                            if (response.isSuccessful) {
                                body.byteStream().use { input ->
                                    InputStreamReader(input, StandardCharsets.UTF_8).use { reader ->
                                        importPset(reader, log)
                                    }
                                }
                            } else {
                                null
                            }
                        }
                    }
                } else {
                    FileInputStream(dateiUrl).use { input ->
                        InputStreamReader(input, StandardCharsets.UTF_8).use { reader ->
                            importPset(reader, log)
                        }
                    }
                }
            } catch (ex: Exception) {
                if (log) {
                    logger.error("importPsetFile(..)", ex)
                }
                null
            }

        fun importPsetText(text: String, log: Boolean): ListePset? =
            try {
                importPset(StringReader(text), log)
            } catch (_: IOException) {
                null
            }

        private fun getOperatingSystemString(): String =
            when {
                SystemUtils.IS_OS_MAC_OSX -> "Mac"
                SystemUtils.IS_OS_WINDOWS -> "Windows"
                SystemUtils.IS_OS_LINUX -> "Linux"
                else -> ""
            }

        private fun getProgramSetTemplateFromLocalResources(): String =
            when {
                SystemUtils.IS_OS_LINUX -> "/mediathek/file/pset_linux.xml"
                SystemUtils.IS_OS_MAC_OSX -> "/mediathek/file/pset_mac.xml"
                SystemUtils.IS_OS_WINDOWS -> "/mediathek/file/pset_windows.xml"
                else -> error("Unsupported OS")
            }

        private fun getLocalPsetTemplate(): InputStreamReader? =
            try {
                val path = getProgramSetTemplateFromLocalResources()
                val stream = requireNotNull(ListePsetVorlagen::class.java.getResource(path)).openStream()
                InputStreamReader(stream, StandardCharsets.UTF_8)
            } catch (ex: IOException) {
                logger.error("getLocalPsetTemplate()", ex)
                null
            }

        private fun importPset(reader: java.io.Reader?, log: Boolean): ListePset? {
            if (reader == null) {
                return null
            }

            var datenPset: DatenPset? = null
            val liste = ListePset()

            try {
                xmlInputFactory().createXMLStreamReader(reader).use { parser ->
                    while (parser.hasNext()) {
                        if (parser.next() == XMLStreamConstants.START_ELEMENT) {
                            when (parser.localName) {
                                DatenPset.TAG -> {
                                    datenPset = DatenPset()
                                    val psetValues = Array(DatenPset.MAX_ELEM) { "" }
                                    if (!get(parser, DatenPset.TAG, DatenPset.XML_NAMES, psetValues)) {
                                        datenPset = null
                                    } else {
                                        datenPset.copyFrom(psetValues)
                                        if (!datenPset.isEmpty()) {
                                            // kann beim Einlesen der Konfigdatei vorkommen
                                            liste.add(datenPset)
                                        }
                                    }
                                }

                                DatenProg.TAG -> {
                                    val currentPset = datenPset
                                    if (currentPset != null) {
                                        val datenProg = DatenProg()
                                        val progValues = datenProg.toArray()
                                        if (get(parser, DatenProg.TAG, DatenProg.XML_NAMES, progValues)) {
                                            datenProg.copyFrom(progValues)
                                            currentPset.addProg(datenProg)
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            } catch (ex: Exception) {
                if (log) {
                    logger.error("importPset", ex)
                }
                return null
            } finally {
                try {
                    reader.close()
                } catch (_: IOException) {
                }
            }

            return liste.ifEmpty { null }
        }

        private fun get(
            parser: XMLStreamReader,
            xmlElem: String,
            xmlNames: Array<String>,
            result: Array<String>,
        ): Boolean {
            result.fill("")
            return try {
                while (parser.hasNext()) {
                    val event = parser.next()
                    if (event == XMLStreamConstants.END_ELEMENT && parser.localName == xmlElem) {
                        break
                    }
                    if (event == XMLStreamConstants.START_ELEMENT) {
                        val index = xmlNames.indexOf(parser.localName)
                        if (index != -1 && index < result.size) {
                            result[index] = parser.elementText
                        }
                    }
                }
                true
            } catch (ex: Exception) {
                logger.error("get", ex)
                false
            }
        }

        private fun xmlInputFactory(): XMLInputFactory =
            XMLInputFactory.newInstance().apply {
                setProperty(XMLInputFactory.IS_COALESCING, false)
                setProperty(XMLInputFactory.SUPPORT_DTD, false)
                setProperty(XMLInputFactory.IS_SUPPORTING_EXTERNAL_ENTITIES, false)
            }

        private fun programGroupColumnNames(): Array<Any?> =
            Array(PGR_COLUMN_NAMES.size) { index -> PGR_COLUMN_NAMES[index] }

        private inline fun XMLStreamReader.use(block: (XMLStreamReader) -> Unit) {
            try {
                block(this)
            } finally {
                try {
                    close()
                } catch (_: XMLStreamException) {
                }
            }
        }
    }
}
