package mediathek.gui.tabs.tab_online_search

import kotlinx.coroutines.CancellationException
import kotlinx.coroutines.delay
import kotlinx.coroutines.sync.Mutex
import kotlinx.coroutines.sync.withLock
import kotlinx.serialization.json.*
import mediathek.config.application.ApplicationConfiguration
import java.net.URLEncoder
import java.nio.charset.StandardCharsets
import java.time.Duration
import java.time.LocalDateTime
import java.time.ZoneId
import java.time.ZonedDateTime
import kotlin.time.Duration.Companion.milliseconds

class ArteOnlineSearchService(
    private val httpClient: OnlineSearchHttpClient = MvOnlineSearchHttpClient,
    private val rateLimitState: ArteOnlineSearchRateLimitState = ApplicationConfigurationArteOnlineSearchRateLimitState,
) : OnlineSearchService {
    private val json = Json { ignoreUnknownKeys = true }
    private val rateLimitMutex = Mutex()

    override suspend fun search(request: OnlineSearchRequest): OnlineSearchPage {
        require(request.provider == OnlineSearchProvider.ARTE)
        val pageNumber = request.nextToken?.toIntOrNull() ?: 1
        val root = get(searchUrl(request.query, pageNumber)).parseJsonObject(json)
        val items = root["data"]?.jsonArrayOrNull().orEmpty()
            .mapNotNull { it.jsonObjectOrNull() }
            .filter { it.hasVideoStreams() }
        val results = items.flatMap { loadFromSearchItem(it) }
            .distinctBy { it.websiteUrl.ifBlank { it.normalQualityUrl } }
        val pagination = root["pagination"]?.jsonObjectOrNull()
        val currentPage = pagination?.get("page")?.jsonPrimitive?.intOrNull ?: pageNumber
        val pages = pagination?.get("pages")?.jsonPrimitive?.intOrNull
        val total = pagination?.get("totalCount")?.jsonPrimitive?.longOrNull
        val nextToken = if (pages != null && currentPage < pages) {
            (currentPage + 1).toString()
        } else {
            null
        }
        return OnlineSearchPage(results, nextToken, total)
    }

    override suspend fun loadByUrl(request: OnlineUrlRequest): OnlineSearchResult? {
        require(request.provider == OnlineSearchProvider.ARTE)
        val id = request.url.toArteProgramId()
        return loadByConfigUrl(
            configUrl = configUrl(id),
            fallbackTopic = "ARTE",
            fallbackWebsiteUrl = request.url,
        )
    }

    private suspend fun loadFromSearchItem(item: JsonObject): List<OnlineSearchResult> {
        if (item.isCollection()) {
            val collectionResults = loadFromCollectionItem(item)
            if (collectionResults.isNotEmpty()) {
                return collectionResults
            }
        }
        val configUrl = item["player"]?.jsonObjectOrNull()?.string("config")
            ?: item.string("id")?.let(::configUrl)
            ?: return emptyList()
        val fallbackTopic = item["genre"]?.jsonObjectOrNull()?.string("label") ?: "ARTE"
        val streamRequest = ArteStreamRequest(
            programId = item.string("programId") ?: item.string("id")?.substringBefore('_'),
            kind = item["kind"]?.jsonObjectOrNull()?.string("code") ?: DEFAULT_STREAM_KIND,
            language = DEFAULT_STREAM_LANGUAGE,
        )
        return listOfNotNull(loadByConfigUrl(
            configUrl = configUrl,
            fallbackTopic = fallbackTopic,
            fallbackWebsiteUrl = item.string("url").orEmpty(),
            fallbackStreamRequest = streamRequest,
        ))
    }

    private suspend fun loadFromCollectionItem(item: JsonObject): List<OnlineSearchResult> {
        val collectionId = item.string("programId") ?: item.string("id") ?: return emptyList()
        val collectionUrl = item.string("url") ?: return emptyList()
        val topic = item.string("title") ?: item["genre"]?.jsonObjectOrNull()?.string("label") ?: "ARTE"
        return runCatching {
            loadCollectionItems(collectionUrl, collectionId)
                .flatMap { collectionItem -> loadFromSearchItem(collectionItem.withFallbackGenre(topic)) }
        }.getOrElse { emptyList() }
    }

    private suspend fun loadCollectionItems(collectionUrl: String, collectionId: String): List<JsonObject> {
        val html = get(collectionUrl)
        val subCollectionIds = html.extractSubCollectionIds(collectionId)
        return subCollectionIds.flatMap { subCollectionId ->
            loadSubCollectionItems(collectionId, subCollectionId)
        }.distinctBy { it.string("programId") ?: it.string("id").orEmpty().substringBefore('_') }
    }

    private suspend fun loadSubCollectionItems(collectionId: String, subCollectionId: String): List<JsonObject> {
        val items = ArrayList<JsonObject>()
        var pageNumber = 1
        do {
            val root = get(subCollectionContentUrl(collectionId, subCollectionId, pageNumber))
                .parseJsonObject(json)
            items += root["data"]?.jsonArrayOrNull().orEmpty()
                .mapNotNull { it.jsonObjectOrNull() }
                .filter { it.hasVideoStreams() }
            val pagination = root["pagination"]?.jsonObjectOrNull()
            val currentPage = pagination?.get("page")?.jsonPrimitive?.intOrNull ?: pageNumber
            val pages = pagination?.get("pages")?.jsonPrimitive?.intOrNull ?: currentPage
            pageNumber = currentPage + 1
        } while (currentPage < pages)
        return items
    }

    private suspend fun loadByConfigUrl(
        configUrl: String,
        fallbackTopic: String,
        fallbackWebsiteUrl: String,
        fallbackStreamRequest: ArteStreamRequest = ArteStreamRequest(
            programId = configUrl.substringAfterLast('/'),
            kind = DEFAULT_STREAM_KIND,
            language = DEFAULT_STREAM_LANGUAGE,
        ),
    ): OnlineSearchResult? {
        val attributes = get(configUrl).parseJsonObject(json)
            .get("data")?.jsonObjectOrNull()
            ?.get("attributes")?.jsonObjectOrNull()
            ?: return null
        val metadata = attributes["metadata"]?.jsonObjectOrNull() ?: return null
        val title = listOfNotNull(metadata.string("title"), metadata.string("subtitle"))
            .joinToString(" - ")
            .ifBlank { return null }
        val hlsStreamUrl = attributes["streams"]?.jsonArrayOrNull().orEmpty()
            .firstNotNullOfOrNull { stream -> stream.jsonObjectOrNull()?.string("url") }
        val mp4Urls = loadMp4Urls(
            fallbackStreamRequest.copy(
                programId = metadata.string("providerId") ?: fallbackStreamRequest.programId,
            ),
        )
        val normalQualityUrl = mp4Urls.normalQualityUrl ?: hlsStreamUrl ?: return null
        val lowQualityUrl = if (mp4Urls.hasAnyUrl) mp4Urls.lowQualityUrl.orEmpty() else hlsStreamUrl.orEmpty()
        val highQualityUrl = if (mp4Urls.hasAnyUrl) mp4Urls.highQualityUrl.orEmpty() else hlsStreamUrl.orEmpty()
        val websiteUrl = metadata["link"]?.jsonObjectOrNull()?.string("url") ?: fallbackWebsiteUrl
        return OnlineSearchResult(
            provider = OnlineSearchProvider.ARTE,
            sender = "ARTE",
            topic = fallbackTopic,
            title = title,
            description = metadata.string("description").orEmpty(),
            websiteUrl = websiteUrl,
            normalQualityUrl = normalQualityUrl,
            lowQualityUrl = lowQualityUrl,
            highQualityUrl = highQualityUrl,
            broadcastTime = parseArteDate(attributes["rights"]?.jsonObjectOrNull()?.string("begin")),
            duration = metadata["duration"]?.jsonObjectOrNull()
                ?.get("seconds")?.jsonPrimitive?.longOrNull?.let(Duration::ofSeconds),
        )
    }

    private suspend fun loadMp4Urls(request: ArteStreamRequest): ArteQualityUrls {
        val programId = request.programId?.takeIf { ARTE_PROGRAM_ID_PATTERN.matches(it) } ?: return ArteQualityUrls.EMPTY
        return runCatching {
            val root = get(streamUrl(programId, request.kind, request.language), ARTE_STREAM_HEADERS).parseJsonObject(json)
            root["videoStreams"]?.jsonArrayOrNull().orEmpty()
                .mapNotNull { it.jsonObjectOrNull() }
                .toArteQualityUrls()
        }.getOrElse {
            if (it is CancellationException) {
                throw it
            }
            ArteQualityUrls.EMPTY
        }
    }

    private fun List<JsonObject>.toArteQualityUrls(): ArteQualityUrls {
        val defaultStreams = filter { it.string("audioCode") in ARTE_DEFAULT_AUDIO_CODES }
        val byQuality = defaultStreams.mapNotNull { stream ->
            val url = stream.string("url")?.fixMissingHttpsProtocol()?.takeIf { it.contains(".mp4", ignoreCase = true) }
            val quality = stream.string("quality")
            if (url == null || quality == null) {
                null
            } else {
                quality to url
            }
        }.toMap()
        val highQualityUrl = byQuality["SQ"]
        val normalQualityUrl = byQuality["EQ"] ?: highQualityUrl
        val lowQualityUrl = byQuality["HQ"] ?: byQuality["MQ"]
        return ArteQualityUrls(
            normalQualityUrl = normalQualityUrl,
            lowQualityUrl = lowQualityUrl,
            highQualityUrl = highQualityUrl,
        )
    }

    private suspend fun get(url: String, headers: Map<String, String> = emptyMap()): String = rateLimitMutex.withLock {
        var retryDelayMillis = rateLimitState.requestDelayMillis.coerceIn(MIN_REQUEST_DELAY_MILLIS, MAX_REQUEST_DELAY_MILLIS)
        repeat(MAX_RATE_LIMIT_ATTEMPTS) { attempt ->
            if (attempt > 0) {
                delay(retryDelayMillis.milliseconds)
            }
            try {
                return httpClient.get(url, headers)
            } catch (ex: OnlineSearchHttpException) {
                if (!ex.isRateLimit || attempt == MAX_RATE_LIMIT_ATTEMPTS - 1) {
                    throw ex
                }
                retryDelayMillis = nextRequestDelayMillis(retryDelayMillis, ex)
                rateLimitState.requestDelayMillis = retryDelayMillis
            }
        }
        error("Unreachable ARTE request retry state")
    }

    private fun nextRequestDelayMillis(currentDelayMillis: Long, exception: OnlineSearchHttpException): Long {
        val retryAfterMillis = exception.retryAfter?.toMillis()
        val nextDelayMillis = retryAfterMillis ?: if (currentDelayMillis > 0) {
            currentDelayMillis * 2
        } else {
            MIN_REQUEST_DELAY_MILLIS
        }
        return nextDelayMillis.coerceIn(MIN_REQUEST_DELAY_MILLIS, MAX_REQUEST_DELAY_MILLIS)
    }

    private fun searchUrl(query: String, pageNumber: Int): String {
        val encodedQuery = URLEncoder.encode(query, StandardCharsets.UTF_8)
        return "$SEARCH_CONTENT_URL?page=$pageNumber&query=$encodedQuery"
    }

    private fun configUrl(id: String): String = "$PLAYER_CONFIG_URL/$id"

    private fun streamUrl(programId: String, kind: String, language: String): String =
        "$OPA_STREAM_URL/$programId/$kind/$language"

    private fun subCollectionContentUrl(collectionId: String, subCollectionId: String, pageNumber: Int): String =
        "$COLLECTION_SUBCOLLECTION_CONTENT_URL?collectionId=$collectionId" +
            "&page=$pageNumber&pageId=collection&subCollectionId=$subCollectionId&type=collection"

    private fun String.toArteProgramId(): String {
        val segments = trim().substringBefore('?').trimEnd('/').split('/')
        return segments.firstOrNull { ARTE_PROGRAM_ID_PATTERN.matches(it) }
            ?: toOnlineSearchUrlLastSegment()
    }

    private fun parseArteDate(value: String?): LocalDateTime? = value?.let {
        ZonedDateTime.parse(it).withZoneSameInstant(BERLIN).toLocalDateTime()
    }

    private fun JsonObject.hasVideoStreams(): Boolean =
        this["availability"]?.jsonObjectOrNull()
            ?.get("hasVideoStreams")?.jsonPrimitive?.booleanOrNull != false

    private fun JsonObject.isCollection(): Boolean =
        this["kind"]?.jsonObjectOrNull()?.get("isCollection")?.jsonPrimitive?.booleanOrNull == true ||
            string("deeplink")?.startsWith("arte://collection/") == true

    private fun JsonObject.withFallbackGenre(label: String): JsonObject =
        if (this["genre"] != null) {
            this
        } else {
            JsonObject(this + ("genre" to JsonObject(mapOf("label" to JsonPrimitive(label)))))
        }

    private fun String.extractSubCollectionIds(collectionId: String): List<String> =
        SUBCOLLECTION_ID_PATTERN.findAll(this)
            .map { it.groupValues[1] }
            .filter { it != collectionId }
            .distinct()
            .toList()

    private fun String.fixMissingHttpsProtocol(): String =
        if (startsWith("http:")) {
            replaceFirst("http:", "https:")
        } else {
            this
        }

    private companion object {
        private const val SEARCH_CONTENT_URL =
            "https://api.arte.tv/api/emac/v4/de/web/zones/f75e7b54-a37b-4f8f-b01b-dbefa877c041/content"
        private const val PLAYER_CONFIG_URL = "https://api.arte.tv/api/player/v2/config/de"
        private const val OPA_STREAM_URL = "https://www.arte.tv/hbbtvv2/services/web/index.php/OPA/v3/streams"
        private const val COLLECTION_SUBCOLLECTION_CONTENT_URL =
            "https://api-cdn.arte.tv/api/emac/v4/de/web/zones/8fbf35e4-324d-4d3e-a194-28f7d288789b/content"
        private const val DEFAULT_STREAM_KIND = "SHOW"
        private const val DEFAULT_STREAM_LANGUAGE = "de"
        private const val ARTE_STREAM_API_TOKEN =
            "Bearer Nzc1Yjc1ZjJkYjk1NWFhN2I2MWEwMmRlMzAzNjI5NmU3NWU3ODg4ODJjOWMxNTMxYzEzZGRjYjg2ZGE4MmIwOA"
        private const val MAX_RATE_LIMIT_ATTEMPTS = 3
        private const val MIN_REQUEST_DELAY_MILLIS = 250L
        private const val MAX_REQUEST_DELAY_MILLIS = 10_000L
        private val ARTE_PROGRAM_ID_PATTERN = Regex("""(?:[0-9]{6}-[0-9A-Z]{3}-[A-Z]|RC-[0-9]{6})""")
        private val ARTE_DEFAULT_AUDIO_CODES = setOf("VA", "VA-STA", "VOA", "VOA-STA")
        private val ARTE_STREAM_HEADERS = mapOf("Authorization" to ARTE_STREAM_API_TOKEN)
        private val SUBCOLLECTION_ID_PATTERN = Regex("""subCollectionId(?:=|\\u003d)(RC-[0-9]{6})""")
        private val BERLIN: ZoneId = ZoneId.of("Europe/Berlin")
    }
}

private data class ArteStreamRequest(
    val programId: String?,
    val kind: String,
    val language: String,
)

private data class ArteQualityUrls(
    val normalQualityUrl: String?,
    val lowQualityUrl: String?,
    val highQualityUrl: String?,
) {
    val hasAnyUrl: Boolean
        get() = normalQualityUrl != null || lowQualityUrl != null || highQualityUrl != null

    companion object {
        val EMPTY = ArteQualityUrls(null, null, null)
    }
}

interface ArteOnlineSearchRateLimitState {
    var requestDelayMillis: Long
}

private object ApplicationConfigurationArteOnlineSearchRateLimitState : ArteOnlineSearchRateLimitState {
    override var requestDelayMillis: Long
        get() = ApplicationConfiguration.getInstance().onlineSearchArteRequestDelayMillis
        set(value) {
            ApplicationConfiguration.getInstance().onlineSearchArteRequestDelayMillis = value
        }
}
