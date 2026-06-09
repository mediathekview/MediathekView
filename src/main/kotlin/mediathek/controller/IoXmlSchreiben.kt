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

import mediathek.config.Daten
import mediathek.config.StandardLocations
import mediathek.daten.DatenProg
import mediathek.daten.DatenPset
import mediathek.tool.ReplaceList
import org.apache.logging.log4j.LogManager
import java.io.OutputStreamWriter
import java.nio.charset.StandardCharsets
import java.nio.file.Files
import java.nio.file.Path
import java.nio.file.Paths
import javax.xml.stream.XMLOutputFactory
import javax.xml.stream.XMLStreamWriter

class IoXmlSchreiben(
    private val downloadStoragePath: Path = StandardLocations.getDownloadsFilePath(),
) {
    private val outFactory: XMLOutputFactory = XMLOutputFactory.newInstance()

    @Synchronized
    fun writeConfigurationFile(xmlFilePath: Path) {
        xmlDatenSchreiben(xmlFilePath)
        writeDownloads()
    }

    @Synchronized
    fun exportPset(pSet: Array<DatenPset>, datei: String) {
        val xmlFilePath = Paths.get(datei)
        try {
            Files.newOutputStream(xmlFilePath).use { output ->
                OutputStreamWriter(output, StandardCharsets.UTF_8).use { writer ->
                    val xmlWriter = outFactory.createXMLStreamWriter(writer)
                    logger.info("Pset exportieren nach: {}", xmlFilePath.toString())
                    logger.debug("Start Schreiben nach: {}", xmlFilePath.toAbsolutePath())

                    writeFileHeader(xmlWriter)

                    xmlSchreibenPset(xmlWriter, pSet)

                    writeFileEnd(xmlWriter)
                    logger.debug("geschrieben!")
                }
            }
        } catch (ex: Exception) {
            logger.error("nach {}", datei, ex)
        }
    }

    private fun writeFileHeader(writer: XMLStreamWriter) {
        writer.writeStartDocument(StandardCharsets.UTF_8.name(), "1.0")
        writeNewLine(writer)
        writer.writeStartElement("Mediathek")
        writeNewLine(writer)
    }

    private fun writeFileEnd(writer: XMLStreamWriter) {
        writer.writeEndElement()
        writer.writeEndDocument()
        writer.flush()
        writer.close()
    }

    private fun writeAbos(writer: XMLStreamWriter) {
        writer.writeCharacters("\n\n")
        writeNewLine(writer)

        for (datenAbo in Daten.getInstance().listeAbo) {
            datenAbo.writeToConfig(writer)
        }
    }

    private fun writeBlacklistRules(writer: XMLStreamWriter) {
        writer.writeCharacters("\n\n")
        writeNewLine(writer)

        // remove duplicates
        val distinctBlacklistRules = Daten.getInstance().listeBlacklist.stream().distinct().toList()
        for (rule in distinctBlacklistRules) {
            rule.writeToConfig(writer)
        }
    }

    private fun writeProgramSets(writer: XMLStreamWriter) {
        writer.writeCharacters("\n\n")
        writeNewLine(writer)
        // Proggruppen schreiben, bei Konfig-Datei
        for (datenPset in Daten.getInstance().listePset) {
            writeProgramSet(writer, datenPset)
            for (datenProg in datenPset.listeProg) {
                xmlSchreibenDaten(writer, DatenProg.TAG, DatenProg.XML_NAMES, datenProg.toArray(), true)
            }
        }
    }

    private fun writeReplacementTable(writer: XMLStreamWriter) {
        writer.writeCharacters("\n\n")
        // writer.writeComment("Ersetzungstabelle");
        writeNewLine(writer)

        for (values in ReplaceList.valuesForXml()) {
            xmlSchreibenDaten(writer, ReplaceList.REPLACELIST, ReplaceList.columnNames(), values, false)
        }
    }

    private fun writeDownloads() {
        try {
            DownloadStorage.write(downloadStoragePath, Daten.getInstance().listeDownloads)
        } catch (ex: Exception) {
            logger.error("writeDownloads error!", ex)
        }
    }

    private fun xmlSchreibenPset(writer: XMLStreamWriter, psetArray: Array<DatenPset>) {
        // wird beim Export Sets verwendet
        writer.writeCharacters("\n\n")
        for (pset in psetArray) {
            writeProgramSet(writer, pset)
            for (datenProg in pset.listeProg) {
                xmlSchreibenDaten(writer, DatenProg.TAG, DatenProg.XML_NAMES, datenProg.toArray(), true)
            }
            writer.writeCharacters("\n\n")
        }
    }

    private fun writeProgramSet(writer: XMLStreamWriter, pset: DatenPset) {
        try {
            val values = pset.toArray()
            writer.writeStartElement(DatenPset.TAG)
            writeNewLine(writer)

            for (i in values.indices) {
                writeProgramSetField(writer, DatenPset.XML_NAMES[i], values[i])
            }

            writer.writeEndElement()
            writeNewLine(writer)
        } catch (ex: Exception) {
            logger.error("writeProgramSet", ex)
        }
    }

    private fun writeProgramSetField(writer: XMLStreamWriter, xmlName: String, value: String?) {
        if (value.isNullOrEmpty()) {
            return
        }
        writer.writeCharacters("\t")
        writer.writeStartElement(xmlName)
        writer.writeCharacters(value)
        writer.writeEndElement()
        writeNewLine(writer)
    }

    private fun xmlSchreibenDaten(
        writer: XMLStreamWriter,
        xmlName: String,
        xmlColumns: Array<String>,
        data: Array<String>,
        newLine: Boolean,
    ) {
        try {
            writer.writeStartElement(xmlName)
            if (newLine) {
                writeNewLine(writer)
            }
            for (i in data.indices) {
                if (data[i].isNotEmpty()) {
                    if (newLine) {
                        writer.writeCharacters("\t") // Tab
                    }
                    writer.writeStartElement(xmlColumns[i])
                    writer.writeCharacters(data[i])
                    writer.writeEndElement()
                    if (newLine) {
                        writeNewLine(writer)
                    }
                }
            }
            writer.writeEndElement()
            writeNewLine(writer)
        } catch (ex: Exception) {
            logger.error("xmlSchreibenDaten", ex)
        }
    }

    private fun writeNewLine(writer: XMLStreamWriter) {
        writer.writeCharacters("\n") // neue Zeile
    }

    private fun xmlDatenSchreiben(xmlFilePath: Path) {
        logger.debug("Config Schreiben nach: {} startet", xmlFilePath.toAbsolutePath())

        try {
            Files.newOutputStream(xmlFilePath).use { output ->
                OutputStreamWriter(output, StandardCharsets.UTF_8).use { writer ->
                    val xmlWriter = outFactory.createXMLStreamWriter(writer)

                    writeFileHeader(xmlWriter)

                    writeAbos(xmlWriter)

                    writeBlacklistRules(xmlWriter)

                    writeProgramSets(xmlWriter)

                    writeReplacementTable(xmlWriter)

                    xmlWriter.writeCharacters("\n\n")

                    writeFileEnd(xmlWriter)
                }
            }
        } catch (ex: Exception) {
            logger.error("xmlDatenSchreiben", ex)
        }
    }

    companion object {
        private val logger = LogManager.getLogger(IoXmlSchreiben::class.java)
    }
}
