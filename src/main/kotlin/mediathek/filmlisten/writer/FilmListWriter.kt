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

package mediathek.filmlisten.writer

import mediathek.daten.DatenFilm
import mediathek.daten.ListeFilme
import mediathek.gui.messages.FilmListWriteStartEvent
import mediathek.gui.messages.FilmListWriteStopEvent
import mediathek.tool.FileUtils
import mediathek.tool.MessageBus
import org.apache.commons.lang3.SystemUtils
import org.apache.logging.log4j.LogManager
import tools.jackson.core.JsonEncoding
import tools.jackson.core.JsonGenerator
import tools.jackson.core.ObjectWriteContext
import tools.jackson.core.json.JsonFactory
import tools.jackson.core.util.DefaultPrettyPrinter
import java.io.BufferedOutputStream
import java.io.IOException
import java.io.OutputStream
import java.nio.file.Files
import java.nio.file.Path
import kotlin.time.Duration.Companion.milliseconds
import kotlin.time.Duration.Companion.nanoseconds

class FilmListWriter(private val readable: Boolean) {
    private var sender = ""
    private var thema = ""
    var compressSenderTag = true
    var compressThemaTag = true
    var decompressUrls = false

    private fun getJsonGenerator(os: OutputStream): JsonGenerator {
        val context: ObjectWriteContext = if (readable) {
            PrettyObjectWriteContext()
        } else {
            ObjectWriteContext.empty()
        }

        return JsonFactory().createGenerator(context, os, JsonEncoding.UTF8)
    }

    private fun checkOsxCacheDirectory() {
        val filePath = Path.of(SystemUtils.USER_HOME, "Library", "Caches", "MediathekView")
        if (Files.notExists(filePath)) {
            try {
                Files.createDirectories(filePath)
            } catch (e: IOException) {
                logger.error("checkOsxCacheDirectory", e)
            }
        }
    }

    private fun writeFormatHeader(jg: JsonGenerator, listeFilme: ListeFilme) {
        val meta = listeFilme.metaData

        jg.writeArrayPropertyStart(FILMLISTE)
        jg.writeString("") // ListeFilme.FILMLISTE_DATUM_NR unused in newer versions
        jg.writeString(meta.datum)
        jg.writeString(meta.version)
        jg.writeString("")
        jg.writeString(meta.id)
        jg.writeEndArray()
    }

    @Throws(IOException::class)
    fun writeFilmList(datei: String, listeFilme: ListeFilme, progressListener: ((Double) -> Unit)? = null) {
        MessageBus.messageBus.publishAsync(FilmListWriteStartEvent())

        try {
            logger.info("Filme schreiben ({} Filme) :", listeFilme.size)
            logger.info("   --> Start Schreiben nach: {}", datei)

            sender = ""
            thema = ""

            // Check if Cache directory exists on OSX
            if (SystemUtils.IS_OS_MAC_OSX) {
                checkOsxCacheDirectory()
            }

            writeFilmListAtomically(Path.of(datei), listeFilme, progressListener)
        } catch (ex: IOException) {
            logger.error("nach: {}", datei, ex)
            throw ex
        } finally {
            MessageBus.messageBus.publishAsync(FilmListWriteStopEvent())
        }
    }

    private fun writeFilmListAtomically(
        filePath: Path,
        listeFilme: ListeFilme,
        progressListener: ((Double) -> Unit)?,
    ) {
        val targetFile = filePath.toAbsolutePath()
        val tempFile = createTempFileFor(targetFile)
        var moveCompleted = false
        try {
            writeFilmListToFile(tempFile, listeFilme, progressListener)
            FileUtils.moveAtomicallyWithFallback(tempFile, targetFile)
            moveCompleted = true
        } finally {
            if (!moveCompleted) {
                deleteIncompleteTempFile(tempFile)
            }
        }
    }

    private fun deleteIncompleteTempFile(tempFile: Path) {
        try {
            Files.deleteIfExists(tempFile)
        } catch (ex: IOException) {
            logger.warn("Could not delete incomplete filmlist temp file: {}", tempFile, ex)
        }
    }

    private fun createTempFileFor(filePath: Path): Path {
        val directory = filePath.parent
        val fileName = filePath.fileName.toString()
        return Files.createTempFile(directory, "$fileName.", ".tmp")
    }

    private fun writeFilmListToFile(
        filePath: Path,
        listeFilme: ListeFilme,
        progressListener: ((Double) -> Unit)?,
    ) {
        val start = System.nanoTime()

        Files.newOutputStream(filePath).use { fos ->
            BufferedOutputStream(fos, BUFFER_SIZE).use { bos ->
                getJsonGenerator(bos).use { jg ->
                    jg.writeStartObject()

                    writeFormatHeader(jg, listeFilme)
                    writeFormatDescription(jg)

                    val filmEntries = createFilmEntriesSnapshot(listeFilme)
                    val entryCount = filmEntries.size.toLong()
                    var curEntry = 0L
                    val progressStep = maxOf(1L, entryCount / PROGRESS_UPDATES)

                    for (datenFilm in filmEntries) {
                        writeEntry(datenFilm, jg)
                        curEntry++
                        if (progressListener != null && (curEntry % progressStep == 0L || curEntry == entryCount)) {
                            progressListener(curEntry / entryCount.toDouble())
                        }
                    }
                    jg.writeEndObject()

                    progressListener?.invoke(1.0)

                    val end = System.nanoTime()

                    logger.info("   --> geschrieben!")
                    logger.trace("Write duration: {} ms", (end - start).nanoseconds.inWholeMilliseconds)
                }
            }
        }
    }

    private fun createFilmEntriesSnapshot(listeFilme: ListeFilme): List<DatenFilm> {
        val filmEntries = listeFilme.snapshot()
        if (!compressSenderTag) {
            return filmEntries
        }
        return prepareFilmEntriesForCompressedWrite(filmEntries)
    }

    private fun writeDatumLong(datenFilm: DatenFilm, jg: JsonGenerator) {
        if (datenFilm.isDatumFilmUndefined) {
            jg.writeString("")
        } else {
            val timeSeconds = datenFilm.datumFilmTimeMillis.milliseconds.inWholeSeconds
            jg.writeString(timeSeconds.toString())
        }
    }

    private fun writeFilmLength(datenFilm: DatenFilm, jg: JsonGenerator) {
        jg.writeString(datenFilm.filmLengthAsString)
    }

    private fun writeEntry(film: DatenFilm, jg: JsonGenerator) {
        jg.writeArrayPropertyStart(TAG_JSON_LIST)

        writeSender(jg, film)
        writeThema(jg, film)
        writeTitel(jg, film)
        jg.writeString(film.sendeDatum)
        writeZeit(jg, film)
        writeFilmLength(film, jg)
        jg.writeString(film.fileSizeAsString)
        jg.writeString(film.description)
        jg.writeString(film.urlNormalQuality)
        jg.writeString(film.websiteUrl)
        jg.writeString(film.subtitleUrl)
        skipEntry(jg) // legacy RTMP URL field
        writeLowQualityUrl(jg, film)
        skipEntry(jg) // legacy low-quality RTMP URL field
        writeHighQualityUrl(jg, film)
        skipEntry(jg) // legacy HD RTMP URL field
        writeDatumLong(film, jg)
        skipEntry(jg) // legacy history URL field
        if (!film.hasCountries()) {
            jg.writeString("")
        } else {
            jg.writeString(film.countriesAsString)
        }
        jg.writeString(film.isNew.toString())

        jg.writeEndArray()
    }

    private fun writeLowQualityUrl(jg: JsonGenerator, datenFilm: DatenFilm) {
        var url = datenFilm.lowQualityUrl
        if (decompressUrls && DatenFilm.isCompressedUrl(url)) {
            url = datenFilm.decompressUrl(url)
        }

        jg.writeString(url)
    }

    private fun writeHighQualityUrl(jg: JsonGenerator, datenFilm: DatenFilm) {
        var url = datenFilm.storedHighQualityUrl
        if (decompressUrls && DatenFilm.isCompressedUrl(url)) {
            url = datenFilm.decompressUrl(url)
        }

        jg.writeString(url)
    }

    private fun skipEntry(jg: JsonGenerator) {
        jg.writeString("")
    }

    private fun writeTitel(jg: JsonGenerator, datenFilm: DatenFilm) {
        jg.writeString(datenFilm.title)
    }

    private fun writeSender(jg: JsonGenerator, datenFilm: DatenFilm) {
        val tempSender = datenFilm.sender

        if (compressSenderTag) {
            if (tempSender == sender) {
                jg.writeString("")
            } else {
                sender = tempSender
                jg.writeString(tempSender)
            }
        } else {
            jg.writeString(tempSender)
        }
    }

    private fun writeThema(jg: JsonGenerator, datenFilm: DatenFilm) {
        val filmThema = datenFilm.thema
        if (compressThemaTag) {
            if (filmThema == thema) {
                jg.writeString("")
            } else {
                thema = filmThema
                jg.writeString(filmThema)
            }
        } else {
            jg.writeString(filmThema)
        }
    }

    private fun writeZeit(jg: JsonGenerator, datenFilm: DatenFilm) {
        jg.writeString(datenFilm.sendeZeitForFilmList)
    }

    /**
     * Write a dummy field description array.
     * Is not used anywhere but necessary for compatibility
     */
    private fun writeFormatDescription(jg: JsonGenerator) {
        jg.writeArrayPropertyStart(FILMLISTE)
        jg.writeString("")
        jg.writeEndArray()
    }

    private class PrettyObjectWriteContext : ObjectWriteContext.Base() {
        override fun hasPrettyPrinter(): Boolean = true

        override fun getPrettyPrinter(): DefaultPrettyPrinter = DefaultPrettyPrinter()
    }

    companion object {
        internal fun prepareFilmEntriesForCompressedWrite(filmEntries: List<DatenFilm>): List<DatenFilm> =
            if (isSortedForCompressedWrite(filmEntries)) {
                filmEntries
            } else {
                filmEntries.sortedWith(COMPRESSED_WRITE_ORDER)
            }

        private fun isSortedForCompressedWrite(filmEntries: List<DatenFilm>): Boolean {
            var index = 1
            while (index < filmEntries.size) {
                if (COMPRESSED_WRITE_ORDER.compare(filmEntries[index - 1], filmEntries[index]) > 0) {
                    return false
                }
                index++
            }
            return true
        }

        private const val FILMLISTE = "Filmliste"
        private const val TAG_JSON_LIST = "X"
        private const val BUFFER_SIZE = 64 * 1024
        private const val PROGRESS_UPDATES = 500L
        private val COMPRESSED_WRITE_ORDER = compareBy(DatenFilm::sender).thenBy(DatenFilm::thema)
        private val logger = LogManager.getLogger()
    }
}
