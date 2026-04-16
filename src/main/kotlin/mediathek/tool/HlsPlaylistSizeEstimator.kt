package mediathek.tool

import kotlinx.coroutines.*
import mediathek.tool.http.MVHttpClient
import okhttp3.HttpUrl
import okhttp3.HttpUrl.Companion.toHttpUrlOrNull
import okhttp3.Request
import okhttp3.Response
import okhttp3.ResponseBody
import org.apache.logging.log4j.LogManager
import java.io.IOException
import kotlin.math.ceil

class HlsPlaylistSizeEstimator(
    private val segmentParallelism: Int = DEFAULT_SEGMENT_PARALLELISM,
) {
    data class VariantInfo(
        val bandwidth: Long?,
        val averageBandwidth: Long?,
        val resolution: String?,
        val codecs: String?,
        val playlistUrl: HttpUrl,
        val audioPlaylistUrl: HttpUrl? = null,
    )

    data class EstimateResult(
        val selectedVariant: VariantInfo,
        val availableVariants: List<VariantInfo>,
        val segmentCount: Int,
        val totalBytes: Long,
    )

    suspend fun estimate(url: String): EstimateResult {
        val playlistUrl = requireNotNull(url.toHttpUrlOrNull()) { "Invalid HLS URL: $url" }
        return estimate(
            playlistUrl = playlistUrl,
            textLoader = ::loadText,
            contentLengthLoader = ::loadContentLength,
        )
    }

    internal suspend fun estimate(
        playlistUrl: HttpUrl,
        textLoader: suspend (HttpUrl) -> String,
        contentLengthLoader: suspend (HttpUrl) -> Long,
    ): EstimateResult {
        val playlist = textLoader(playlistUrl)

        return if (playlist.isMasterPlaylist()) {
            estimateMasterPlaylist(playlistUrl, playlist, textLoader, contentLengthLoader)
        } else {
            estimateMediaPlaylist(
                selectedVariant = VariantInfo(
                    bandwidth = null,
                    averageBandwidth = null,
                    resolution = null,
                    codecs = null,
                    playlistUrl = playlistUrl,
                ),
                availableVariants = emptyList(),
                textLoader = textLoader,
                contentLengthLoader = contentLengthLoader,
            )
        }
    }

    private suspend fun estimateMasterPlaylist(
        masterUrl: HttpUrl,
        playlist: String,
        textLoader: suspend (HttpUrl) -> String,
        contentLengthLoader: suspend (HttpUrl) -> Long,
    ): EstimateResult {
        val variants = parseVariants(masterUrl, playlist)
        require(variants.isNotEmpty()) { "No bitrate variants found in playlist: $masterUrl" }

        val selectedVariant = variants.maxWith(
            compareBy<VariantInfo> { it.bandwidth ?: Long.MIN_VALUE }
                .thenBy { it.averageBandwidth ?: Long.MIN_VALUE },
        )

        return estimateMediaPlaylist(selectedVariant, variants, textLoader, contentLengthLoader)
    }

    private suspend fun estimateMediaPlaylist(
        selectedVariant: VariantInfo,
        availableVariants: List<VariantInfo>,
        textLoader: suspend (HttpUrl) -> String,
        contentLengthLoader: suspend (HttpUrl) -> Long,
    ): EstimateResult {
        val videoPlaylist = textLoader(selectedVariant.playlistUrl)
        val videoSegmentUrls = parseSegmentUrls(selectedVariant.playlistUrl, videoPlaylist)
        require(videoSegmentUrls.isNotEmpty()) { "No media segments found in playlist: ${selectedVariant.playlistUrl}" }
        val videoDurationSeconds = parsePlaylistDurationSeconds(videoPlaylist)

        val bitrateFallbackBytes = selectedVariant.estimatedTotalBytes(videoDurationSeconds)
        val exactVideoBytes = runCatching {
            sumSegmentUrls(videoSegmentUrls, contentLengthLoader)
        }.getOrElse { exception ->
            if (bitrateFallbackBytes != null && exception.isMissingContentLengthFailure()) {
                logger.debug("Falling back to bitrate-based HLS size estimate for {}", selectedVariant.playlistUrl, exception)
                null
            } else {
                throw exception
            }
        }

        val totalBytes = if (exactVideoBytes != null) {
            val audioBytes = selectedVariant.audioPlaylistUrl?.let { audioPlaylistUrl ->
                runCatching {
                    val audioPlaylist = textLoader(audioPlaylistUrl)
                    val audioSegmentUrls = parseSegmentUrls(audioPlaylistUrl, audioPlaylist)
                    if (audioSegmentUrls.isEmpty()) {
                        0L
                    } else {
                        sumSegmentUrls(audioSegmentUrls, contentLengthLoader)
                    }
                }.onFailure { exception ->
                    logger.debug("HLS audio size lookup failed for {}", audioPlaylistUrl, exception)
                }.getOrDefault(0L)
            } ?: 0L

            exactVideoBytes + audioBytes
        } else {
            requireNotNull(bitrateFallbackBytes) {
                "Could not determine HLS size for ${selectedVariant.playlistUrl} because all segment lengths were unavailable"
            }
        }

        return EstimateResult(
            selectedVariant = selectedVariant,
            availableVariants = availableVariants.sortedWith(
                compareByDescending<VariantInfo> { it.bandwidth ?: Long.MIN_VALUE }
                    .thenByDescending { it.averageBandwidth ?: Long.MIN_VALUE },
            ),
            segmentCount = videoSegmentUrls.size,
            totalBytes = totalBytes,
        )
    }

    private fun VariantInfo.estimatedTotalBytes(durationSeconds: Double): Long? {
        val bitsPerSecond = averageBandwidth ?: bandwidth ?: return null
        if (durationSeconds <= 0.0) {
            return null
        }
        return ceil(durationSeconds * bitsPerSecond / BITS_PER_BYTE).toLong()
    }

    private fun parsePlaylistDurationSeconds(playlist: String): Double =
        playlist.lineSequence()
            .map(String::trim)
            .filter { it.startsWith(EXTINF_TAG) }
            .mapNotNull(::parseExtinfDuration)
            .sum()

    private fun parseExtinfDuration(line: String): Double? =
        line.removePrefix(EXTINF_TAG)
            .substringBefore(',')
            .toDoubleOrNull()

    private fun Throwable.isMissingContentLengthFailure(): Boolean =
        this is MissingContentLengthException ||
            (this is IllegalStateException && message?.startsWith("Missing Content-Length for segment: ") == true) ||
            cause?.isMissingContentLengthFailure() == true

    private fun parseVariants(baseUrl: HttpUrl, playlist: String): List<VariantInfo> {
        val lines = playlist.lines().map(String::trim)
        val variants = mutableListOf<VariantInfo>()
        val audioGroups = parseAudioGroups(baseUrl, lines)

        for (index in 0 until lines.lastIndex) {
            val line = lines[index]
            if (!line.startsWith(STREAM_INF_TAG)) {
                continue
            }

            val attributes = parseAttributes(line.removePrefix(STREAM_INF_TAG))
            val bandwidth = attributes["BANDWIDTH"]?.toLongOrNull() ?: continue
            val nextLine = lines.subList(index + 1, lines.size).firstOrNull { it.isNotBlank() && !it.startsWith("#") } ?: continue

            variants += VariantInfo(
                bandwidth = bandwidth,
                averageBandwidth = attributes["AVERAGE-BANDWIDTH"]?.toLongOrNull(),
                resolution = attributes["RESOLUTION"],
                codecs = attributes["CODECS"],
                playlistUrl = resolveUrl(baseUrl, nextLine),
                audioPlaylistUrl = attributes["AUDIO"]?.let(audioGroups::get),
            )
        }

        return variants
    }

    private fun parseAudioGroups(baseUrl: HttpUrl, lines: List<String>): Map<String, HttpUrl> =
        buildMap {
            for (line in lines) {
                if (!line.startsWith(MEDIA_TAG)) {
                    continue
                }

                val attributes = parseAttributes(line.removePrefix(MEDIA_TAG))
                if (attributes["TYPE"] != "AUDIO") {
                    continue
                }

                val groupId = attributes["GROUP-ID"] ?: continue
                val uri = attributes["URI"] ?: continue
                put(groupId, resolveUrl(baseUrl, uri))
            }
        }

    private fun parseSegmentUrls(baseUrl: HttpUrl, playlist: String): List<HttpUrl> =
        playlist.lineSequence()
            .map(String::trim)
            .filter { it.isNotBlank() && !it.startsWith("#") }
            .map { resolveUrl(baseUrl, it) }
            .toList()

    private fun parseAttributes(line: String): Map<String, String> {
        val attributes = linkedMapOf<String, String>()
        val current = StringBuilder()
        val parts = mutableListOf<String>()
        var insideQuotes = false

        for (char in line) {
            when {
                char == '"' -> {
                    insideQuotes = !insideQuotes
                    current.append(char)
                }

                char == ',' && !insideQuotes -> {
                    parts += current.toString()
                    current.setLength(0)
                }

                else -> current.append(char)
            }
        }

        if (current.isNotEmpty()) {
            parts += current.toString()
        }

        for (part in parts) {
            val separatorIndex = part.indexOf('=')
            if (separatorIndex <= 0) {
                continue
            }

            val key = part.substring(0, separatorIndex)
            val value = part.substring(separatorIndex + 1).removeSurrounding("\"")
            attributes[key] = value
        }

        return attributes
    }

    private fun String.isMasterPlaylist(): Boolean =
        lineSequence().map(String::trim).any { it.startsWith(STREAM_INF_TAG) }

    private fun resolveUrl(baseUrl: HttpUrl, reference: String): HttpUrl =
        requireNotNull(baseUrl.resolve(reference)) { "Could not resolve '$reference' against '$baseUrl'" }

    private suspend fun sumSegmentUrls(
        segmentUrls: List<HttpUrl>,
        contentLengthLoader: suspend (HttpUrl) -> Long,
    ): Long = coroutineScope {
        segmentUrls
            .chunked(segmentParallelism.coerceAtLeast(1))
            .map { batch ->
                async {
                    var batchTotal = 0L
                    for (segmentUrl in batch) {
                        batchTotal += contentLengthLoader(segmentUrl)
                    }
                    batchTotal
                }
            }
            .awaitAll()
            .sum()
    }

    private suspend fun loadText(url: HttpUrl): String = withContext(Dispatchers.IO) {
        execute(Request.Builder().url(url).get().build()) { response ->
            response.body?.string().orEmpty()
        }
    }

    private suspend fun loadContentLength(url: HttpUrl): Long = withContext(Dispatchers.IO) {
        val headRequest = Request.Builder().url(url).head().build()
        val headLength = runCatching {
            execute(headRequest, ::contentLength)
        }.getOrNull()

        if (headLength != null && headLength > 0) {
            return@withContext headLength
        }

        val getRequest = Request.Builder().url(url).get().build()
        execute(getRequest) { response ->
            val length = contentLengthOrBodyLength(response)
            if (length <= 0) {
                throw MissingContentLengthException(url)
            }
            length
        }
    }

    private fun <T> execute(request: Request, mapper: (Response) -> T): T =
        try {
            MVHttpClient.getInstance().httpClient.newCall(request).execute().use { response ->
                if (!response.isSuccessful) {
                    throw FileSize.HttpStatusException(response.code, request.url)
                }
                mapper(response)
            }
        } catch (exception: IOException) {
            if (exception is FileSize.HttpStatusException) {
                logger.debug("HLS request failed for {} with HTTP {}", request.url, exception.statusCode)
            } else {
                logger.debug("HLS request failed for {}", request.url, exception)
            }
            throw exception
        }

    private fun contentLength(response: Response): Long =
        response.header("Content-Length")?.toLongOrNull()
            ?: response.body?.contentLength()
            ?: -1L

    internal fun contentLengthOrBodyLength(response: Response): Long {
        val length = contentLength(response)
        if (length > 0) {
            return length
        }
        return response.body.bodyByteLength()
    }

    private fun ResponseBody?.bodyByteLength(): Long =
        this?.bytes()?.size?.toLong() ?: -1L

    companion object {
        private const val BITS_PER_BYTE = 8.0
        private const val DEFAULT_SEGMENT_PARALLELISM = 12
        private const val EXTINF_TAG = "#EXTINF:"
        private const val MEDIA_TAG = "#EXT-X-MEDIA:"
        private const val STREAM_INF_TAG = "#EXT-X-STREAM-INF:"
        private val logger = LogManager.getLogger(HlsPlaylistSizeEstimator::class.java)
    }

    private class MissingContentLengthException(
        url: HttpUrl,
    ) : IllegalStateException("Missing Content-Length for segment: $url")
}
