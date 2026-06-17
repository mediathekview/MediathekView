package mediathek.gui.dialog.download

import com.github.kokorin.jaffree.StreamType
import com.github.kokorin.jaffree.ffprobe.FFprobe
import com.github.kokorin.jaffree.ffprobe.FFprobeResult
import com.github.kokorin.jaffree.ffprobe.Stream
import com.github.kokorin.jaffree.process.JaffreeAbnormalExitException
import mediathek.daten.DatenFilm
import mediathek.daten.FilmResolution
import mediathek.tool.ArteHlsQualitySelector
import mediathek.tool.FileSize
import mediathek.tool.FileUtils
import mediathek.tool.GuiFunktionenProgramme
import org.apache.logging.log4j.LogManager
import java.nio.file.Files
import java.nio.file.Path
import java.nio.file.Paths

data class DownloadQualityLiveInfoText(
    val video: String = "",
    val audio: String = ""
)

data class DownloadQualityResolutionSizes(
    val high: String = "",
    val normal: String = "",
    val low: String = ""
) {
    fun forResolution(resolution: FilmResolution.Enum): String {
        return when (resolution) {
            FilmResolution.Enum.HIGH_QUALITY -> high
            FilmResolution.Enum.LOW -> low
            else -> normal
        }
    }
}

data class DownloadQualityResolutionSizeLoadResult(
    val sizes: DownloadQualityResolutionSizes = DownloadQualityResolutionSizes(),
    val httpStatusCode: Int? = null,
)

object DownloadQualitySupport {
    private val logger = LogManager.getLogger()
    private const val NO_DATA_AVAILABLE = "Keine Daten verfügbar."

    fun loadResolutionSizes(film: DatenFilm): DownloadQualityResolutionSizes {
        return loadResolutionSizeResult(film).sizes
    }

    fun loadResolutionSizeResult(film: DatenFilm): DownloadQualityResolutionSizeLoadResult {
        return loadResolutionSizeResult(film) { resolution ->
            when (resolution) {
                FilmResolution.Enum.HIGH_QUALITY,
                FilmResolution.Enum.LOW,
                -> fetchFileSizeForQuality(film, resolution)
                else -> fetchFileSizeForNormalQuality(film)
            }
        }
    }

    internal fun loadResolutionSizeResult(
        film: DatenFilm,
        fileSizeLookup: (FilmResolution.Enum) -> FileSize.LookupResult,
    ): DownloadQualityResolutionSizeLoadResult {
        val high = if (film.isHighQuality) {
            fileSizeLookup(FilmResolution.Enum.HIGH_QUALITY)
        } else {
            FileSize.LookupResult(FileSize.INVALID_SIZE.toLong())
        }
        val normal = fileSizeLookup(FilmResolution.Enum.NORMAL)
        val low = if (film.hasLowQuality()) {
            fileSizeLookup(FilmResolution.Enum.LOW)
        } else {
            FileSize.LookupResult(FileSize.INVALID_SIZE.toLong())
        }
        return DownloadQualityResolutionSizeLoadResult(
            sizes = DownloadQualityResolutionSizes(
                high = high.sizeText,
                normal = normal.sizeText,
                low = low.sizeText,
            ),
            httpStatusCode = high.httpStatusCode ?: normal.httpStatusCode ?: low.httpStatusCode,
        )
    }

    fun formatResolutionLabel(baseLabel: String, sizeInMb: String?): String {
        return if (sizeInMb.isNullOrEmpty()) {
            baseLabel
        } else {
            "$baseLabel   [ $sizeInMb MB ]"
        }
    }

    fun qualityPanelTitle(baseTitle: String, usableSpaceBytes: Long): String {
        return if (usableSpaceBytes > 0L) {
            "$baseTitle [ Freier Speicherplatz: ${FileUtils.humanReadableByteCountBinary(usableSpaceBytes)} ]"
        } else {
            baseTitle
        }
    }

    fun getFreeDiskSpace(pathText: String): Long {
        if (pathText.isEmpty()) {
            return 0L
        }

        return try {
            var path = Paths.get(pathText)
            while (path != null && Files.notExists(path)) {
                path = path.parent
            }

            if (path == null) {
                0L
            } else {
                Files.getFileStore(path).usableSpace
            }
        } catch (ex: Exception) {
            logger.error("getFreeDiskSpace failed", ex)
            0L
        }
    }

    fun findFfprobeExecutableDirectory(): Path? {
        return runCatching {
            GuiFunktionenProgramme.findExecutableOnPath("ffprobe").parent
        }.onFailure {
            logger.error("ffprobe not found on system.", it)
        }.getOrNull()
    }

    @Throws(JaffreeAbnormalExitException::class)
    fun fetchLiveInfo(
        ffprobePath: Path,
        film: DatenFilm,
        resolution: FilmResolution.Enum
    ): DownloadQualityLiveInfoText {
        return fetchLiveInfo(ffprobePath, film.getUrlFuerAufloesung(resolution), resolution)
    }

    @Throws(JaffreeAbnormalExitException::class)
    fun fetchLiveInfo(
        ffprobePath: Path,
        url: String
    ): DownloadQualityLiveInfoText = fetchLiveInfo(ffprobePath, url, null)

    @Throws(JaffreeAbnormalExitException::class)
    private fun fetchLiveInfo(
        ffprobePath: Path,
        url: String,
        resolution: FilmResolution.Enum?
    ): DownloadQualityLiveInfoText {
        val result = FFprobe.atPath(ffprobePath).apply {
            ArteHlsQualitySelector.programId(url, resolution)?.let { programId ->
                setSelectStreams("p:$programId")
            }
        }
            .setShowStreams(true)
            .setInput(url)
            .execute()
        return buildLiveInfoText(result)
    }

    fun getLiveInfoErrorString(ex: JaffreeAbnormalExitException): String {
        return ex.processErrorLogMessages
            .firstOrNull()
            ?.message
            ?.substringAfterLast(':')
            ?.trim()
            ?.takeIf { it.startsWith("Server returned ") }
            ?.removePrefix("Server returned ")
            ?.trim()
            ?: "Unbekannter Fehler aufgetreten."
    }

    private fun fetchFileSizeForQuality(film: DatenFilm, resolution: FilmResolution.Enum): FileSize.LookupResult {
        return runCatching {
            film.lookupFileSizeForUrl(film.getUrlFuerAufloesung(resolution), true, resolution.name)
        }.onFailure { logger.error("Failed to retrieve file size for $resolution", it) }
            .getOrDefault(FileSize.LookupResult(FileSize.INVALID_SIZE.toLong()))
    }

    private fun fetchFileSizeForNormalQuality(film: DatenFilm): FileSize.LookupResult {
        return runCatching {
            film.lookupFileSizeForUrl(film.urlNormalQuality, true, FilmResolution.Enum.NORMAL.name)
        }.onFailure { logger.error("Failed to retrieve normal quality size", it) }
            .getOrDefault(FileSize.LookupResult(FileSize.INVALID_SIZE.toLong()))
    }

    private fun buildLiveInfoText(result: FFprobeResult): DownloadQualityLiveInfoText {
        val audioStream = result.streams.find { it.codecType == StreamType.AUDIO }
        val videoStream = result.streams.find { it.codecType == StreamType.VIDEO }

        return DownloadQualityLiveInfoText(
            video = videoStream?.let {
                val frameRate = it.avgFrameRate.toInt()
                val codecName = getVideoCodecName(it)
                getVideoInfoString(it, frameRate, codecName)
            } ?: NO_DATA_AVAILABLE,
            audio = audioStream?.let { getAudioInfo(it, it.sampleRate) } ?: NO_DATA_AVAILABLE
        )
    }

    private fun getVideoCodecName(stream: Stream): String {
        logger.trace("video codec long name: ${stream.codecLongName}")
        return stream.codecLongName.split("/")
            .firstOrNull()
            ?.trim()
            ?: stream.codecLongName
    }

    private fun getAudioInfo(stream: Stream, sampleRate: Int?): String {
        val bitRate = safeProcessBitRate(stream.bitRate)
        return if (bitRate == 0) {
            "Audio: ${sampleRate ?: "?"} Hz, ${stream.codecLongName}"
        } else {
            "Audio: ${sampleRate ?: "?"} Hz, $bitRate kBit/s, ${stream.codecLongName}"
        }
    }

    private fun getVideoInfoString(stream: Stream, frameRate: Int, codecName: String?): String {
        val bitRate = safeProcessBitRate(stream.bitRate)
        return if (bitRate == 0) {
            "Video: ${stream.width}x${stream.height}, $frameRate fps (avg), $codecName"
        } else {
            "Video: ${stream.width}x${stream.height}, $bitRate kBit/s, $frameRate fps (avg), $codecName"
        }
    }

    private fun safeProcessBitRate(bitRate: Int?): Int = (bitRate ?: 0) / 1000
}
