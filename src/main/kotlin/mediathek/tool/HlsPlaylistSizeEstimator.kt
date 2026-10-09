package mediathek.tool

import kotlinx.coroutines.*
import mediathek.tool.http.MVHttpClient
import okhttp3.HttpUrl
import okhttp3.HttpUrl.Companion.toHttpUrlOrNull
import okhttp3.OkHttpClient
import okhttp3.Request
import okhttp3.Response
import org.apache.logging.log4j.LogManager
import java.io.IOException
import java.util.concurrent.atomic.AtomicInteger
import kotlin.math.ceil

class HlsPlaylistSizeEstimator(
    private val segmentParallelism: Int = DEFAULT_SEGMENT_PARALLELISM,
    private val httpClient: OkHttpClient = HlsEgressPolicy.clientFor(MVHttpClient.httpClient),
) {
    data class VariantInfo(
        val bandwidth: Long?,
        val averageBandwidth: Long?,
        val resolution: String?,
        val codecs: String?,
        val playlistUrl: HttpUrl,
        val audioPlaylistUrl: HttpUrl? = null,
    ) {
        val height: Int?
            get() = resolution?.substringAfter('x')?.toIntOrNull()

        val isHevc: Boolean
            get() = codecs?.contains("hev", ignoreCase = true) == true
    }

    data class EstimateResult(
        val selectedVariant: VariantInfo,
        val availableVariants: List<VariantInfo>,
        val segmentCount: Int,
        val totalBytes: Long,
    )

    private data class SegmentReference(
        val url: HttpUrl,
        val byteRangeLength: Long? = null,
    )

    private data class MediaPlaylistReferences(
        val initializationSegments: List<SegmentReference>,
        val mediaSegments: List<SegmentReference>,
    ) {
        val allSegments: List<SegmentReference>
            get() = initializationSegments + mediaSegments
    }

    suspend fun estimate(url: String, probeSegments: Boolean = true, quality: String? = null): EstimateResult {
        val playlistUrl = HlsEgressPolicy.requirePublicHttpUrl(
            requireNotNull(url.toHttpUrlOrNull()) { "Invalid HLS URL: $url" },
        )
        return estimate(
            playlistUrl = playlistUrl,
            probeSegments = probeSegments,
            quality = quality,
            textLoader = ::loadText,
            contentLengthLoader = ::loadContentLength,
        )
    }

    internal suspend fun estimate(
        playlistUrl: HttpUrl,
        probeSegments: Boolean = true,
        quality: String? = null,
        textLoader: suspend (HttpUrl) -> String,
        contentLengthLoader: suspend (HttpUrl) -> Long,
    ): EstimateResult {
        HlsEgressPolicy.requirePublicHttpUrl(playlistUrl)
        val playlist = textLoader(playlistUrl)

        return if (playlist.isMasterPlaylist()) {
            estimateMasterPlaylist(playlistUrl, playlist, probeSegments, quality, textLoader, contentLengthLoader)
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
                probeSegments = probeSegments,
                textLoader = textLoader,
                contentLengthLoader = contentLengthLoader,
            )
        }
    }

    private suspend fun estimateMasterPlaylist(
        masterUrl: HttpUrl,
        playlist: String,
        probeSegments: Boolean,
        quality: String?,
        textLoader: suspend (HttpUrl) -> String,
        contentLengthLoader: suspend (HttpUrl) -> Long,
    ): EstimateResult {
        val variants = parseVariants(masterUrl, playlist)
        require(variants.isNotEmpty()) { "No bitrate variants found in playlist: $masterUrl" }

        val selectedVariant = selectVariant(variants, quality)

        return estimateMediaPlaylist(selectedVariant, variants, probeSegments, textLoader, contentLengthLoader)
    }

    private fun selectVariant(variants: List<VariantInfo>, quality: String?): VariantInfo {
        val compatibleVariants = variants.filterNot { it.isHevc }.ifEmpty { variants }
        return when (quality) {
            "LOW" -> compatibleVariants
                .filter { (it.height ?: Int.MAX_VALUE) <= LOW_MAX_HEIGHT }
                .maxWithOrNull(VARIANT_QUALITY_COMPARATOR)
                ?: compatibleVariants.minWith(VARIANT_QUALITY_COMPARATOR)

            "NORMAL" -> compatibleVariants
                .filter { (it.height ?: Int.MAX_VALUE) <= NORMAL_MAX_HEIGHT }
                .maxWithOrNull(VARIANT_QUALITY_COMPARATOR)
                ?: compatibleVariants.maxWith(VARIANT_QUALITY_COMPARATOR)

            else -> compatibleVariants.maxWith(VARIANT_QUALITY_COMPARATOR)
        }
    }

    private suspend fun estimateMediaPlaylist(
        selectedVariant: VariantInfo,
        availableVariants: List<VariantInfo>,
        probeSegments: Boolean,
        textLoader: suspend (HttpUrl) -> String,
        contentLengthLoader: suspend (HttpUrl) -> Long,
    ): EstimateResult {
        val videoPlaylist = textLoader(selectedVariant.playlistUrl)
        val videoReferences = parseMediaPlaylistReferences(selectedVariant.playlistUrl, videoPlaylist)
        require(videoReferences.mediaSegments.isNotEmpty()) { "No media segments found in playlist: ${selectedVariant.playlistUrl}" }
        val videoDurationSeconds = parsePlaylistDurationSeconds(videoPlaylist)

        val bitrateFallbackBytes = selectedVariant.estimatedTotalBytes(videoDurationSeconds)
        if (!probeSegments) {
            return buildEstimateResult(
                selectedVariant = selectedVariant,
                availableVariants = availableVariants,
                segmentCount = videoReferences.mediaSegments.size,
                totalBytes = requireNotNull(bitrateFallbackBytes) {
                    "Could not determine HLS size for ${selectedVariant.playlistUrl} without segment probing because bitrate metadata is unavailable"
                },
            )
        }

        val exactVideoBytes = runCatching {
            sumSegments(videoReferences.allSegments, contentLengthLoader)
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
                    val audioReferences = parseMediaPlaylistReferences(audioPlaylistUrl, audioPlaylist)
                    if (audioReferences.mediaSegments.isEmpty()) {
                        0L
                    } else {
                        sumSegments(audioReferences.allSegments, contentLengthLoader)
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

        return buildEstimateResult(
            selectedVariant = selectedVariant,
            availableVariants = availableVariants,
            segmentCount = videoReferences.mediaSegments.size,
            totalBytes = totalBytes,
        )
    }

    private fun buildEstimateResult(
        selectedVariant: VariantInfo,
        availableVariants: List<VariantInfo>,
        segmentCount: Int,
        totalBytes: Long,
    ): EstimateResult =
        EstimateResult(
            selectedVariant = selectedVariant,
            availableVariants = availableVariants.sortedWith(
                compareByDescending<VariantInfo> { it.bandwidth ?: Long.MIN_VALUE }
                    .thenByDescending { it.averageBandwidth ?: Long.MIN_VALUE },
            ),
            segmentCount = segmentCount,
            totalBytes = totalBytes,
        )

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

    private fun parseMediaPlaylistReferences(baseUrl: HttpUrl, playlist: String): MediaPlaylistReferences {
        val initializationSegments = mutableListOf<SegmentReference>()
        val mediaSegments = mutableListOf<SegmentReference>()
        var pendingByteRangeLength: Long? = null
        playlist.lineSequence()
            .map(String::trim)
            .filter(String::isNotBlank)
            .forEach { line ->
                when {
                    line.startsWith(MAP_TAG) -> {
                        val attributes = parseAttributes(line.removePrefix(MAP_TAG))
                        val uri = attributes["URI"] ?: return@forEach
                        initializationSegments += SegmentReference(
                            url = resolveUrl(baseUrl, uri),
                            byteRangeLength = attributes["BYTERANGE"]?.parseByteRangeLength(),
                        )
                    }

                    line.startsWith(BYTERANGE_TAG) -> {
                        pendingByteRangeLength = line.removePrefix(BYTERANGE_TAG)
                            .parseByteRangeLength()
                    }

                    !line.startsWith("#") -> {
                        mediaSegments += SegmentReference(
                            url = resolveUrl(baseUrl, line),
                            byteRangeLength = pendingByteRangeLength,
                        )
                        pendingByteRangeLength = null
                    }
                }
            }
        return MediaPlaylistReferences(initializationSegments, mediaSegments)
    }

    private fun String.parseByteRangeLength(): Long? =
        substringBefore('@').toLongOrNull()

    private fun parseAttributes(line: String): Map<String, String> {
        val attributes = linkedMapOf<String, String>()
        val current = StringBuilder()
        val parts = mutableListOf<String>()
        var insideQuotes = false

        for (char in line) {
            when (char) {
                '"' -> {
                    insideQuotes = !insideQuotes
                    current.append(char)
                }

                ',' -> if (!insideQuotes) {
                    parts += current.toString()
                    current.setLength(0)
                } else {
                    current.append(char)
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
        HlsEgressPolicy.requirePublicHttpUrl(
            requireNotNull(baseUrl.resolve(reference)) { "Could not resolve '$reference' against '$baseUrl'" },
        )

    private suspend fun sumSegments(
        segments: List<SegmentReference>,
        contentLengthLoader: suspend (HttpUrl) -> Long,
    ): Long = coroutineScope {
        if (segments.isEmpty()) {
            return@coroutineScope 0L
        }

        val workerCount = minOf(segmentParallelism.coerceAtLeast(1), segments.size)
        val nextSegmentIndex = AtomicInteger(0)
        val workerTotals = LongArray(workerCount)

        List(workerCount) { workerIndex ->
            launch {
                var workerTotal = 0L
                while (true) {
                    val segmentIndex = nextSegmentIndex.getAndIncrement()
                    if (segmentIndex >= segments.size) {
                        break
                    }
                    val segment = segments[segmentIndex]
                    workerTotal += segment.byteRangeLength ?: contentLengthLoader(segment.url)
                }
                workerTotals[workerIndex] = workerTotal
            }
        }.joinAll()

        workerTotals.sum()
    }

    private suspend fun loadText(url: HttpUrl): String = withContext(Dispatchers.IO) {
        HlsEgressPolicy.requirePublicHttpUrl(url)
        execute(Request.Builder().url(url).get().build()) { response ->
            response.body.string()
        }
    }

    private suspend fun loadContentLength(url: HttpUrl): Long = withContext(Dispatchers.IO) {
        HlsEgressPolicy.requirePublicHttpUrl(url)
        val headRequest = Request.Builder().url(url).head().build()
        val headLength = runCatching {
            execute(headRequest, ::contentLength)
        }.getOrNull()

        if (headLength != null && headLength > 0) {
            return@withContext headLength
        }

        val rangeRequest = Request.Builder()
            .url(url)
            .header("Range", "bytes=0-0")
            .get()
            .build()
        execute(rangeRequest) { response ->
            val length = contentLengthOrRangeLength(response)
            if (length <= 0) {
                throw MissingContentLengthException(url)
            }
            length
        }
    }

    private fun <T> execute(request: Request, mapper: (Response) -> T): T =
        try {
            httpClient.newCall(request).execute().use { response ->
                if (!response.isSuccessful) {
                    throw FileSize.HttpStatusException(response.code, request.url)
                }
                mapper(response)
            }
        } catch (exception: IOException) {
            if (exception is FileSize.HttpStatusException) {
                logger.debug("HLS request failed for {} with HTTP {}", request.url, exception.statusCode)
            } else {
                logger.debug("HLS request failed for {}: {}", request.url, exception.conciseLogMessage())
                logger.trace("HLS request failure details for {}", request.url, exception)
            }
            throw exception
        }

    private fun contentLength(response: Response): Long =
        response.header("Content-Length")?.toLongOrNull()
            ?: response.body.contentLength()

    internal fun contentLengthOrRangeLength(response: Response): Long =
        parseContentRangeLength(response.header("Content-Range"))
            ?: contentLength(response)

    private fun parseContentRangeLength(contentRange: String?): Long? =
        contentRange
            ?.substringAfterLast('/', "")
            ?.toLongOrNull()

    companion object {
        private const val BITS_PER_BYTE = 8.0
        private const val DEFAULT_SEGMENT_PARALLELISM = 12
        private const val EXTINF_TAG = "#EXTINF:"
        private const val MAP_TAG = "#EXT-X-MAP:"
        private const val BYTERANGE_TAG = "#EXT-X-BYTERANGE:"
        private const val MEDIA_TAG = "#EXT-X-MEDIA:"
        private const val STREAM_INF_TAG = "#EXT-X-STREAM-INF:"
        private const val LOW_MAX_HEIGHT = 360
        private const val NORMAL_MAX_HEIGHT = 720
        private val VARIANT_QUALITY_COMPARATOR = compareBy<VariantInfo> { it.height ?: Int.MIN_VALUE }
            .thenBy { it.bandwidth ?: Long.MIN_VALUE }
            .thenBy { it.averageBandwidth ?: Long.MIN_VALUE }
        private val logger = LogManager.getLogger(HlsPlaylistSizeEstimator::class.java)
    }

    private class MissingContentLengthException(
        url: HttpUrl,
    ) : IllegalStateException("Missing Content-Length for segment: $url")
}
