package mediathek.tool

import mediathek.daten.DatenDownload
import mediathek.daten.DatenFilm
import org.apache.commons.text.WordUtils
import org.apache.logging.log4j.LogManager
import java.io.*
import java.nio.file.Files
import java.nio.file.Path
import java.nio.file.Paths
import java.util.*

open class MVInfoFile {
    protected fun formatFilmAsString(film: DatenFilm?, maxLengthHeaders: Int): String {
        if (null == film)
            return ""

        val formatString = String.format("%%-%ds %%s", maxLengthHeaders)
        var sb = StringBuilder()
        sb = appendFormattedTableLine(sb, formatString, FILM_SENDER, film.sender)
        sb = appendFormattedTableLine(sb, formatString, FILM_THEMA, film.thema).append(System.lineSeparator())
        sb = appendFormattedTableLine(sb, formatString, FILM_TITEL, film.title).append(System.lineSeparator())
        sb = appendFormattedTableLine(sb, formatString, FILM_DATUM, film.sendeDatum)
        sb = appendFormattedTableLine(sb, formatString, FILM_ZEIT, film.sendeZeit)
        sb = appendFormattedTableLine(sb, formatString, FILM_DAUER, film.dauer)
        sb = appendFormattedTableLine(sb, formatString, FILM_GROESSE, film.size).append(System.lineSeparator())
        sb.append("Website")
        sb.append(System.lineSeparator())
        sb.append(film.websiteLink)
        sb.append(System.lineSeparator())
        sb.append(System.lineSeparator())
        sb.append(FILM_URL)
        sb.append(System.lineSeparator())
        sb.append(film.url)
        sb.append(System.lineSeparator())
        sb.append(System.lineSeparator())
        sb.append(splitStringIntoMaxFixedLengthLines(film.description, 62))
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
    fun writeInfoFile(film: DatenFilm?, path: Path) {
        logger.info("Infofile schreiben nach: {}", path.toAbsolutePath().toString())
        path.toFile().parentFile.mkdirs()
        Files.newOutputStream(path).use { os ->
            DataOutputStream(os).use { dos ->
                OutputStreamWriter(dos).use { osw ->
                    BufferedWriter(osw).use { br ->
                        br.write(formatFilmAsString(film, FILM_GROESSE.length + 2))
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
        film?.let { writeInfoFile(it, path) }
    }

    private companion object {
        private val logger = LogManager.getLogger(MVInfoFile::class.java)
        private const val FILM_GROESSE = "Größe [MB]"
        private const val FILM_SENDER = "Sender"
        private const val FILM_THEMA = "Thema"
        private const val FILM_TITEL = "Titel"
        private const val FILM_DATUM = "Datum"
        private const val FILM_ZEIT = "Zeit"
        private const val FILM_DAUER = "Dauer"
        private const val FILM_URL = "URL"
    }
}