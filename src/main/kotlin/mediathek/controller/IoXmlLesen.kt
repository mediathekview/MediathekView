/*
 * Copyright (c) 2026 derreisende77.
 * This code was developed as part of the MediathekView project https://github.com/mediathekview/MediathekView
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */
package mediathek.controller

import mediathek.config.StandardLocations
import mediathek.daten.DatenDownload
import mediathek.daten.DatenProg
import mediathek.daten.DatenPset
import mediathek.tool.ReplaceList
import org.apache.logging.log4j.LogManager
import java.io.InputStreamReader
import java.nio.charset.StandardCharsets
import java.nio.file.Files
import java.nio.file.Path
import javax.xml.stream.XMLInputFactory
import javax.xml.stream.XMLStreamConstants
import javax.xml.stream.XMLStreamException
import javax.xml.stream.XMLStreamReader

class IoXmlLesen @JvmOverloads constructor(
    private val configData: XmlConfigData,
    private val downloadStoragePath: Path = StandardLocations.getDownloadsFilePath(),
    private val blacklistRuleStoragePath: Path = StandardLocations.getBlacklistRulesFilePath(),
    private val aboRuleStoragePath: Path = StandardLocations.getAboRulesFilePath(),
) {
    private val inFactory: XMLInputFactory = XMLInputFactory.newInstance().apply {
        setProperty(XMLInputFactory.IS_COALESCING, false)
        setProperty(XMLInputFactory.SUPPORT_DTD, false)
        setProperty(XMLInputFactory.IS_SUPPORTING_EXTERNAL_ENTITIES, false)
    }
    fun datenLesen(xmlFilePath: Path): Boolean {
        var ret = false
        if (Files.exists(xmlFilePath)) {
            var datenPset: DatenPset? = null
            var legacyDownloadsRead = false
            var legacyBlacklistRulesRead = false
            var legacyAboRulesRead = false
            val readDownloadsFromJson = Files.exists(downloadStoragePath)
            val readBlacklistRulesFromJson = Files.exists(blacklistRuleStoragePath)
            val readAboRulesFromJson = Files.exists(aboRuleStoragePath)

            try {
                Files.newInputStream(xmlFilePath).use { input ->
                    InputStreamReader(input, StandardCharsets.UTF_8).use { reader ->
                        inFactory.createXMLStreamReader(reader).use { parser ->
                            while (parser.hasNext()) {
                                if (parser.next() == XMLStreamConstants.START_ELEMENT) {
                                    when (parser.localName) {
                                        SYSTEM_ELEMENT -> skipElement(parser)
                                        DatenPset.TAG -> {
                                            datenPset = readProgramSet(parser)
                                            val currentPset = datenPset
                                            if (currentPset != null) {
                                                configData.programSets.add(currentPset)
                                            }
                                        }

                                        DatenProg.TAG -> {
                                            readProgramEntry(parser, datenPset)
                                        }

                                        ReplaceList.REPLACELIST -> readReplacementList(parser)
                                        LegacyAboRuleXml.TAG -> {
                                            legacyAboRulesRead =
                                                readAboEntry(parser, readLegacyAboRule = !readAboRulesFromJson) ||
                                                    legacyAboRulesRead
                                        }

                                        DatenDownload.TAG -> {
                                            legacyDownloadsRead =
                                                readDownloadEntry(parser, readLegacyDownload = !readDownloadsFromJson) ||
                                                    legacyDownloadsRead
                                        }

                                        LegacyBlacklistRuleXml.TAG -> {
                                            legacyBlacklistRulesRead =
                                                readBlacklistRuleEntry(
                                                    parser,
                                                    readLegacyBlacklistRule = !readBlacklistRulesFromJson,
                                                ) || legacyBlacklistRulesRead
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
                ret = true
            } catch (ex: Exception) {
                ret = false
                logger.error("datenLesen", ex)
            }

            if (readDownloadsFromJson) {
                readDownloadsFromJson()
            }

            if (readBlacklistRulesFromJson) {
                readBlacklistRulesFromJson()
            }

            if (readAboRulesFromJson) {
                readAboRulesFromJson()
            }

            sortLists()

            if (!readDownloadsFromJson && legacyDownloadsRead) {
                writeMigratedDownloads()
            }

            if (!readBlacklistRulesFromJson && legacyBlacklistRulesRead) {
                writeMigratedBlacklistRules()
            }

            if (!readAboRulesFromJson && legacyAboRulesRead) {
                writeMigratedAboRules()
            }
        }

        return ret
    }

    private fun skipElement(parser: XMLStreamReader) {
        var depth = 1
        while (depth > 0 && parser.hasNext()) {
            when (parser.next()) {
                XMLStreamConstants.START_ELEMENT -> depth++
                XMLStreamConstants.END_ELEMENT -> depth--
            }
        }
    }

    private fun get(
        parser: XMLStreamReader,
        xmlElem: String,
        xmlNames: Array<String>,
        result: Array<String>,
    ): Boolean {
        return try {
            while (parser.hasNext()) {
                val event = parser.next()
                if (event == XMLStreamConstants.END_ELEMENT && parser.localName == xmlElem) {
                    break
                }
                if (event == XMLStreamConstants.START_ELEMENT) {
                    val index = xmlNames.indexOf(parser.localName)
                    if (index in result.indices) {
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

    private fun readProgramEntry(parser: XMLStreamReader, datenPset: DatenPset?) {
        val datenProg = DatenProg()
        val progValues = datenProg.toArray()
        if (get(parser, DatenProg.TAG, DatenProg.XML_NAMES, progValues) && datenPset != null) {
            datenProg.copyFrom(progValues)
            datenPset.addProg(datenProg)
        }
    }

    private fun readReplacementList(parser: XMLStreamReader) {
        val values = Array(ReplaceList.MAX_ELEM) { "" }
        if (get(parser, ReplaceList.REPLACELIST, ReplaceList.columnNames(), values)) {
            ReplaceList.add(values)
        }
    }

    private fun readAboEntry(parser: XMLStreamReader, readLegacyAboRule: Boolean): Boolean {
        try {
            val datenAbo = LegacyAboRuleXml.readAbo(parser)
            if (readLegacyAboRule) {
                configData.abos.addAboFromConfig(datenAbo)
                return true
            }
        } catch (ex: XMLStreamException) {
            logger.error("Failed to read abo entry", ex)
        }
        return false
    }

    private fun readBlacklistRuleEntry(parser: XMLStreamReader, readLegacyBlacklistRule: Boolean): Boolean {
        try {
            val rule = LegacyBlacklistRuleXml.readRule(parser)
            if (readLegacyBlacklistRule) {
                return configData.blacklistRules.addWithoutNotification(rule)
            }
        } catch (ex: XMLStreamException) {
            logger.error("Failed to read blacklist rule", ex)
        }
        return false
    }

    private fun readDownloadEntry(parser: XMLStreamReader, readLegacyDownload: Boolean): Boolean {
        try {
            val download = DatenDownload.readFromConfig(parser)
            // abo entries will be generated...but we need this for CLI so far
            if (readLegacyDownload && !download.isFromAbo) {
                configData.downloads.addLoadedDownload(download)
                return true
            }
        } catch (ex: Exception) {
            logger.error("readDownloadEntry", ex)
        }
        return false
    }

    private fun readDownloadsFromJson() {
        try {
            configData.downloads.addLoadedDownloads(DownloadStorage.read(downloadStoragePath))
        } catch (ex: Exception) {
            logger.error("Failed to read downloads from {}", downloadStoragePath, ex)
        }
    }

    private fun readBlacklistRulesFromJson() {
        try {
            configData.blacklistRules.addAllWithoutNotification(BlacklistRuleStorage.read(blacklistRuleStoragePath))
        } catch (ex: Exception) {
            logger.error("Failed to read blacklist rules from {}", blacklistRuleStoragePath, ex)
        }
    }

    private fun readAboRulesFromJson() {
        try {
            AboRuleStorage.read(aboRuleStoragePath).forEach(configData.abos::addAboFromConfig)
        } catch (ex: Exception) {
            logger.error("Failed to read abo rules from {}", aboRuleStoragePath, ex)
        }
    }

    private fun writeMigratedDownloads() {
        try {
            DownloadStorage.write(downloadStoragePath, configData.downloads.queuedDownloads())
        } catch (ex: Exception) {
            logger.error("Failed to migrate downloads to {}", downloadStoragePath, ex)
        }
    }

    private fun writeMigratedAboRules() {
        try {
            AboRuleStorage.write(aboRuleStoragePath, configData.abos)
        } catch (ex: Exception) {
            logger.error("Failed to migrate abo rules to {}", aboRuleStoragePath, ex)
        }
    }

    private fun writeMigratedBlacklistRules() {
        try {
            BlacklistRuleStorage.write(blacklistRuleStoragePath, configData.blacklistRules)
        } catch (ex: Exception) {
            logger.error("Failed to migrate blacklist rules to {}", blacklistRuleStoragePath, ex)
        }
    }

    private fun readProgramSet(parser: XMLStreamReader): DatenPset? {
        val psetValues = Array(DatenPset.MAX_ELEM) { "" }
        if (!get(parser, DatenPset.TAG, DatenPset.XML_NAMES, psetValues)) {
            return null
        }

        return DatenPset().apply {
            copyFrom(psetValues)
        }
    }

    private fun sortLists() {
        configData.downloads.renumberQueuedDownloads()
        configData.abos.finishLoading()
    }

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

    companion object {
        private const val SYSTEM_ELEMENT = "system"
        private val logger = LogManager.getLogger(IoXmlLesen::class.java)
    }
}
