package mediathek.tool

import mediathek.daten.DatenDownload
import mediathek.daten.DatenFilm
import okhttp3.HttpUrl
import okhttp3.HttpUrl.Companion.toHttpUrlOrNull
import org.apache.commons.text.WordUtils
import org.apache.logging.log4j.LogManager
import java.io.IOException
import java.nio.file.AtomicMoveNotSupportedException
import java.nio.file.Files
import java.nio.file.Path
import java.nio.file.Paths
import java.nio.file.StandardCopyOption.ATOMIC_MOVE
import java.nio.file.StandardCopyOption.REPLACE_EXISTING

class MVInfoFile(
    private val fileSizeLookup: (HttpUrl) -> Long = FileSize::getFileSizeFromUrl,
) {
    private fun formatFilmAsString(film: DatenFilm, url: HttpUrl, extended: Boolean = false): String =
        MVInfoFileFormatter.format(film.toInfoFileData(url, extended))

    private fun DatenFilm.toInfoFileData(url: HttpUrl, extended: Boolean): MVInfoFileData =
        MVInfoFileData(
            sender = sender,
            thema = thema,
            title = title,
            sendeDatum = sendeDatum,
            sendeZeit = sendeZeit,
            filmLength = filmLengthAsString,
            fileSize = fileSizeLookup(url),
            websiteUrl = websiteUrl,
            urlLines = if (extended) extendedUrlLines() else listOf(url.toString()),
            subtitleUrl = subtitleUrl.takeIf { hasSubtitle() },
            description = description,
        )

    private fun DatenFilm.extendedUrlLines(): List<String> = buildList {
        if (isHighQuality) {
            add("HQ: $highQualityUrl")
        }
        add("Normal: $urlNormalQuality")
        add("LQ: ${decompressIfNeeded(lowQualityUrl)}")
    }

    private fun DatenFilm.decompressIfNeeded(url: String): String =
        if (DatenFilm.isCompressedUrl(url)) decompressUrl(url) else url

    @Throws(IOException::class)
    fun writeInfoFile(film: DatenFilm, path: Path, url: HttpUrl) {
        logger.info("Infofile schreiben nach: {}", path.toAbsolutePath().toString())
        writeString(path, formatFilmAsString(film, url))
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
        val absolutePath = path.toAbsolutePath()
        val parent = absolutePath.parent
        parent?.let(Files::createDirectories)

        val tempFile = Files.createTempFile(parent, "mvinfo-${path.fileName}.", ".tmp")
        try {
            Files.newBufferedWriter(tempFile, Charsets.UTF_8).use { writer ->
                writer.write(content)
            }
            moveReplacingTarget(tempFile, absolutePath)
        } catch (exception: Exception) {
            Files.deleteIfExists(tempFile)
            throw exception
        }
    }

    private fun moveReplacingTarget(source: Path, target: Path) {
        try {
            Files.move(source, target, ATOMIC_MOVE, REPLACE_EXISTING)
        } catch (_: AtomicMoveNotSupportedException) {
            Files.move(source, target, REPLACE_EXISTING)
        }
    }

    private companion object {
        private val logger = LogManager.getLogger(MVInfoFile::class.java)
    }
}

internal data class MVInfoFileData(
    val sender: String,
    val thema: String,
    val title: String,
    val sendeDatum: String,
    val sendeZeit: String,
    val filmLength: String,
    val fileSize: Long,
    val websiteUrl: String,
    val urlLines: List<String>,
    val subtitleUrl: String?,
    val description: String,
)

internal object MVInfoFileFormatter {
    fun format(data: MVInfoFileData): String {
        val formatString = String.format("%%-%ds %%s", MAX_HEADER_LENGTH)
        return buildString {
            appendFormattedTableLine(this, formatString, FILM_SENDER, data.sender)
            appendFormattedTableLine(this, formatString, FILM_THEMA, data.thema).append(System.lineSeparator())
            appendFormattedTableLine(this, formatString, FILM_TITEL, data.title).append(System.lineSeparator())
            appendFormattedTableLine(this, formatString, FILM_DATUM, data.sendeDatum)
            appendFormattedTableLine(this, formatString, FILM_ZEIT, data.sendeZeit)
            appendFormattedTableLine(this, formatString, FILM_DAUER, data.filmLength)
            if (data.fileSize > FileSize.INVALID_SIZE) {
                appendFormattedTableLine(this, formatString, FILM_GROESSE, FileUtils.humanReadableByteCountBinary(data.fileSize))
            } else {
                append(System.lineSeparator())
            }

            append(System.lineSeparator())
            append("Website")
            append(System.lineSeparator())
            append(data.websiteUrl)
            append(System.lineSeparator())
            append(System.lineSeparator())
            append("URL")
            append(System.lineSeparator())
            append(data.urlLines.joinToString(System.lineSeparator()))
            append(System.lineSeparator())
            append(System.lineSeparator())
            appendSubtitleUrl(data.subtitleUrl)
            append(splitStringIntoMaxFixedLengthLines(data.description))
            append(System.lineSeparator())
            append(System.lineSeparator())
        }
    }

    private fun StringBuilder.appendSubtitleUrl(subtitleUrl: String?) {
        if (subtitleUrl == null) {
            return
        }

        append("Subtitle-URL")
        append(System.lineSeparator())
        append(subtitleUrl)
        append(System.lineSeparator())
        append(System.lineSeparator())
    }

    private fun appendFormattedTableLine(
        sb: StringBuilder,
        formatString: String,
        keyTitle: String,
        value: String?,
    ): StringBuilder =
        sb.append(String.format(formatString, "$keyTitle:", value))
            .append(System.lineSeparator())

    private fun splitStringIntoMaxFixedLengthLines(input: String?): String =
        input?.let { WordUtils.wrap(it, MAX_LINE_LENGTH) }.orEmpty()

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
