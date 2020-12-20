package mediathek.tool

import mediathek.daten.DatenDownload
import mediathek.daten.DatenFilm
import okhttp3.HttpUrl
import okhttp3.HttpUrl.Companion.toHttpUrl
import org.apache.commons.io.FileUtils
import org.apache.commons.text.WordUtils
import org.apache.logging.log4j.LogManager
import java.io.*
import java.nio.file.Files
import java.nio.file.Path
import java.nio.file.Paths
import java.util.*

open class MVInfoFile {
    private fun formatFilmAsString(film: DatenFilm?, url: HttpUrl?): String {
        if (null == film || url == null)
            return ""

        //calculate file size based on actual used URL
        val fileSize = FileSize.getFileSizeFromUrl(url)
        val fileSizeStr = FileUtils.byteCountToDisplaySize(fileSize)

        val formatString = String.format("%%-%ds %%s", MAX_HEADER_LENGTH)
        var sb = StringBuilder()
        sb = appendFormattedTableLine(sb, formatString, FILM_SENDER, film.sender)
        sb = appendFormattedTableLine(sb, formatString, FILM_THEMA, film.thema).append(System.lineSeparator())
        sb = appendFormattedTableLine(sb, formatString, FILM_TITEL, film.title).append(System.lineSeparator())
        sb = appendFormattedTableLine(sb, formatString, FILM_DATUM, film.sendeDatum)
        sb = appendFormattedTableLine(sb, formatString, FILM_ZEIT, film.sendeZeit)
        sb = appendFormattedTableLine(sb, formatString, FILM_DAUER, film.dauer)
        sb = appendFormattedTableLine(sb, formatString, FILM_GROESSE, fileSizeStr).append(System.lineSeparator())
        sb.append("Website")
        sb.append(System.lineSeparator())
        sb.append(film.websiteLink)
        sb.append(System.lineSeparator())
        sb.append(System.lineSeparator())
        sb.append(FILM_URL)
        sb.append(System.lineSeparator())
        sb.append(url)
        sb.append(System.lineSeparator())
        sb.append(System.lineSeparator())
        sb.append(splitStringIntoMaxFixedLengthLines(film.description, MAX_LINE_LENGTH))
        sb.append(System.lineSeparator())
        sb.append(System.lineSeparator())
        return sb.toString()
    }

    protected fun appendFormattedTableLine(sb: StringBuilder, formatString: String?, keyTitle: String?, value: String?): StringBuilder {
        return sb.append(String.format(formatString!!, String.format("%s:", keyTitle), value))
                .append(System.lineSeparator())
    }

    protected fun splitStringIntoMaxFixedLengthLines(input: String?, lineLength: Int): String {
        return Optional.ofNullable(input)
                .map { s: String? -> WordUtils.wrap(s, lineLength) }
                .orElse("")
    }

    @Throws(IOException::class)
    fun writeInfoFile(film: DatenFilm?, path: Path, url: HttpUrl?) {
        logger.info("Infofile schreiben nach: {}", path.toAbsolutePath().toString())
        path.toFile().parentFile.mkdirs()

        Files.newOutputStream(path).use { os ->
            DataOutputStream(os).use { dos ->
                OutputStreamWriter(dos).use { osw ->
                    BufferedWriter(osw).use { br ->
                        br.write(formatFilmAsString(film, url))
                        br.flush()
                    }
                }
            }
        }
        logger.info("Infodatei geschrieben")
    }

    @Throws(IOException::class)
    fun writeInfoFile(datenDownload: DatenDownload) {
        File(datenDownload.arr[DatenDownload.DOWNLOAD_ZIEL_PFAD]).mkdirs()
        val path = Paths.get(datenDownload.fileNameWithoutSuffix + ".txt")
        val film = datenDownload.film
        // this is the URL that will be used during download.
        // write this into info file and calculate size from it
        val url = datenDownload.arr[DatenDownload.DOWNLOAD_URL].toHttpUrl()
        film?.let { writeInfoFile(it, path, url) }
    }

    private companion object {
        private val logger = LogManager.getLogger(MVInfoFile::class.java)
        private const val FILM_GROESSE = "Größe"
        private const val FILM_SENDER = "Sender"
        private const val FILM_THEMA = "Thema"
        private const val FILM_TITEL = "Titel"
        private const val FILM_DATUM = "Datum"
        private const val FILM_ZEIT = "Zeit"
        private const val FILM_DAUER = "Dauer"
        private const val FILM_URL = "URL"
        private const val MAX_HEADER_LENGTH = 12
        private const val MAX_LINE_LENGTH = 62
    }
}