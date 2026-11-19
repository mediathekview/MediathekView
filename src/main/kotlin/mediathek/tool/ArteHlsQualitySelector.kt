package mediathek.tool

import mediathek.daten.FilmResolution
import mediathek.tool.http.MVHttpClient
import okhttp3.HttpUrl
import okhttp3.HttpUrl.Companion.toHttpUrlOrNull
import okhttp3.Request
import kotlin.time.Duration.Companion.seconds
import kotlin.time.toJavaDuration

/** One ARTE quality policy for FFmpeg, FFprobe and file-size lookup. */
object ArteHlsQualitySelector {
    internal data class Playlist(val url: HttpUrl, val text: String)
    private data class Variant(val programId: Int, val height: Int, val isAvc: Boolean, val isHevc: Boolean)

    private const val STREAM_INF_TAG = "#EXT-X-STREAM-INF:"
    private const val MAX_PLAYLIST_BYTES = 1_048_576
    private val manifestHosts = setOf("manifest-arte.akamaized.net", "secure-manifest-arte.akamaized.net")
    private val attributePattern = Regex("""([A-Z0-9-]+)=("[^"]*"|[^,"]+)(?:,|$)""")

    fun isArteManifest(url: String): Boolean {
        val httpUrl = url.toHttpUrlOrNull() ?: return false
        return httpUrl.host in manifestHosts &&
            httpUrl.encodedPath.endsWith(".m3u8") &&
            httpUrl.encodedPath.contains("/api/manifest/", ignoreCase = true)
    }

    /** Safe AVC mapping for stored commands and unavailable manifests; no network access here. */
    fun fallbackProgramId(url: String, resolution: FilmResolution.Enum?): Int? {
        if (!isArteManifest(url)) return null
        return resolution?.avcProgramId
    }

    private val FilmResolution.Enum.avcProgramId: Int
        get() = when (this) {
            FilmResolution.Enum.HIGH_QUALITY -> 1
            FilmResolution.Enum.NORMAL -> 2
            FilmResolution.Enum.LOW -> 0
        }

    /** FFmpeg program IDs are the zero-based EXT-X-STREAM-INF indexes, excluding I-frames. */
    internal fun selectProgram(playlist: String, resolution: FilmResolution.Enum): Int? {
        require(playlist.lineSequence().firstOrNull()?.trim() == "#EXTM3U") { "Not an HLS playlist" }
        val variantLines = playlist.lineSequence().map(String::trim)
            .filter { it.startsWith(STREAM_INF_TAG) }.toList()
        val variants = variantLines.mapIndexedNotNull(::parseVariant)
        if (variants.isEmpty()) {
            return resolution.avcProgramId.takeIf { it in variantLines.indices }
                ?: variantLines.indices.singleOrNull()
        }

        val avc = variants.filter { it.isAvc }
        val eligible = if (resolution == FilmResolution.Enum.HIGH_QUALITY) variants else avc.ifEmpty { variants }
        val maxHeight = when (resolution) {
            FilmResolution.Enum.HIGH_QUALITY -> Int.MAX_VALUE
            FilmResolution.Enum.NORMAL -> 720
            FilmResolution.Enum.LOW -> 480
        }
        val targetHeight = eligible.filter { it.height <= maxHeight }.maxOfOrNull { it.height }
            ?: eligible.minOf { it.height }
        val candidates = eligible.filter { it.height == targetHeight }
        val preferred = if (resolution == FilmResolution.Enum.HIGH_QUALITY) {
            candidates.firstOrNull { it.isHevc }
        } else candidates.firstOrNull { it.isAvc }
        return (preferred ?: candidates.firstOrNull { it.isAvc } ?: candidates.first()).programId
    }

    private fun parseVariant(programId: Int, line: String): Variant? {
        val attributes = attributePattern.findAll(line.removePrefix(STREAM_INF_TAG)).associate {
            it.groupValues[1] to it.groupValues[2].removeSurrounding("\"")
        }
        val height = attributes["RESOLUTION"]?.substringAfter('x')?.toIntOrNull() ?: return null
        val codecs = attributes["CODECS"].orEmpty().split(',').map(String::trim)
        fun hasCodec(vararg prefixes: String) = codecs.any { codec ->
            prefixes.any { codec.startsWith(it, ignoreCase = true) }
        }
        return Variant(programId, height, isAvc = hasCodec("avc1", "avc3"), isHevc = hasCodec("hev1", "hvc1"))
    }

    internal fun loadPlaylist(url: HttpUrl): Playlist {
        val client = HlsEgressPolicy.clientFor(MVHttpClient.httpClient).newBuilder()
            .callTimeout(10.seconds.toJavaDuration()).build()
        val request = Request.Builder().url(HlsEgressPolicy.requirePublicHttpUrl(url)).build()
        return client.newCall(request).execute().use { response ->
            check(response.isSuccessful) { "HLS HTTP ${response.code}" }
            val bytes = response.body.byteStream().readNBytes(MAX_PLAYLIST_BYTES + 1)
            require(bytes.size <= MAX_PLAYLIST_BYTES) { "HLS playlist too large" }
            Playlist(response.request.url, bytes.toString(Charsets.UTF_8))
        }
    }
}
