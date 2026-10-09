package mediathek.tool

import mediathek.daten.DatenDownload
import mediathek.daten.DatenFilm
import okhttp3.HttpUrl
import okhttp3.HttpUrl.Companion.toHttpUrlOrNull
import org.apache.commons.text.WordUtils
import org.apache.logging.log4j.LogManager
import java.io.IOException
import java.nio.file.Files
import java.nio.file.Path
import java.nio.file.Paths

class MVInfoFile(
    private val fileSizeLookup: (HttpUrl) -> Long = FileSize::getFileSizeFromUrl,
) {
    private fun formatFilmAsString(film: DatenFilm, url: HttpUrl, extended: Boolean = false): String {
        val fileSize = fileSizeLookup(url)

        val formatString = String.format("%%-%ds %%s", MAX_HEADER_LENGTH)
        return buildString {
            appendFormattedTableLine(this, formatString, FILM_SENDER, film.sender)
            appendFormattedTableLine(this, formatString, FILM_THEMA, film.thema).append(System.lineSeparator())
            appendFormattedTableLine(this, formatString, FILM_TITEL, film.title).append(System.lineSeparator())
            appendFormattedTableLine(this, formatString, FILM_DATUM, film.sendeDatum)
            appendFormattedTableLine(this, formatString, FILM_ZEIT, film.sendeZeit)
            appendFormattedTableLine(this, formatString, FILM_DAUER, film.filmLengthAsString)
            if (fileSize > FileSize.INVALID_SIZE) {
                appendFormattedTableLine(this, formatString, FILM_GROESSE, FileUtils.humanReadableByteCountBinary(fileSize))
            } else {
                append(System.lineSeparator())
            }

            append(System.lineSeparator())
            append("Website")
            append(System.lineSeparator())
            append(film.websiteUrl)
            append(System.lineSeparator())
            append(System.lineSeparator())
            append("URL")
            append(System.lineSeparator())
            if (extended) {
                appendExtendedUrls(film)
            } else {
                append(url)
            }
            append(System.lineSeparator())
            append(System.lineSeparator())
            appendSubtitleUrl(film)
            append(splitStringIntoMaxFixedLengthLines(film.description, MAX_LINE_LENGTH))
            append(System.lineSeparator())
            append(System.lineSeparator())
        }
    }

    private fun StringBuilder.appendSubtitleUrl(film: DatenFilm) {
        if (!film.hasSubtitle()) {
            return
        }

        append("Subtitle-URL")
        append(System.lineSeparator())
        append(film.subtitleUrl)
        append(System.lineSeparator())
        append(System.lineSeparator())
    }

    private fun StringBuilder.appendExtendedUrls(film: DatenFilm) {
        if (film.isHighQuality) {
            append("HQ: ${film.decompressUrl(film.highQualityUrl)}")
            append(System.lineSeparator())
        }
        append("Normal: ${film.urlNormalQuality}")
        append(System.lineSeparator())
        append("LQ: ${film.decompressUrl(film.lowQualityUrl)}")
        append(System.lineSeparator())
    }

    internal fun appendFormattedTableLine(
        sb: StringBuilder,
        formatString: String,
        keyTitle: String,
        value: String?,
    ): StringBuilder =
        sb.append(String.format(formatString, "$keyTitle:", value))
            .append(System.lineSeparator())

    internal fun splitStringIntoMaxFixedLengthLines(input: String?, lineLength: Int): String =
        input?.let { WordUtils.wrap(it, lineLength) }.orEmpty()

    @Throws(IOException::class)
    fun writeInfoFile(film: DatenFilm?, path: Path, url: HttpUrl?) {
        val currentFilm = film ?: throw IOException("Cannot write info file without film data.")
        val currentUrl = url ?: throw IOException("Cannot write info file without download URL.")

        logger.info("Infofile schreiben nach: {}", path.toAbsolutePath().toString())
        writeString(path, formatFilmAsString(currentFilm, currentUrl))
        logger.info("Infodatei geschrieben")
    }

    @Throws(IOException::class)
    fun writeManualInfoFile(film: DatenFilm, path: Path) {
        val url = film.urlNormalQuality.toHttpUrlOrNull()
            ?: throw IOException("Cannot write info file for invalid download URL: ${film.urlNormalQuality}")
        writeString(path, formatFilmAsString(film, url, extended = true))
    }

    @Throws(IOException::class)
    fun writeInfoFile(datenDownload: DatenDownload) {
        val path = Paths.get(datenDownload.fileNameWithoutSuffix + ".txt")
        val film = datenDownload.film ?: throw IOException("Cannot write info file without film data.")
        val url = datenDownload.downloadUrl.toHttpUrlOrNull()
            ?: throw IOException("Cannot write info file for invalid download URL: ${datenDownload.downloadUrl}")
        writeInfoFile(film, path, url)
    }

    private fun writeString(path: Path, content: String) {
        path.parent?.let { parent -> Files.createDirectories(parent) }
        Files.newBufferedWriter(path, Charsets.UTF_8).use { writer ->
            writer.write(content)
        }
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
        private const val MAX_HEADER_LENGTH = 12
        private const val MAX_LINE_LENGTH = 62
    }
}
