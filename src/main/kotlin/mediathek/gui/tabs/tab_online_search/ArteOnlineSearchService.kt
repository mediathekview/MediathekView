package mediathek.gui.tabs.tab_online_search

import kotlinx.coroutines.CancellationException
import kotlinx.coroutines.delay
import kotlinx.coroutines.sync.Mutex
import kotlinx.coroutines.sync.withLock
import kotlinx.serialization.json.*
import mediathek.config.application.ApplicationConfiguration
import java.net.URI
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
        return listOfNotNull(loadByConfigUrl(
            configUrl = configUrl,
            fallbackTopic = fallbackTopic,
            fallbackWebsiteUrl = item.string("url").orEmpty(),
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
    ): OnlineSearchResult? {
        val attributes = get(configUrl).parseJsonObject(json)["data"]?.jsonObjectOrNull()
            ?.get("attributes")?.jsonObjectOrNull()
            ?: return null
        val metadata = attributes["metadata"]?.jsonObjectOrNull() ?: return null
        val title = listOfNotNull(metadata.string("title"), metadata.string("subtitle"))
            .joinToString(" - ")
            .ifBlank { return null }
        val hlsStream = attributes["streams"]?.jsonArrayOrNull().orEmpty()
            .firstNotNullOfOrNull { stream -> stream.jsonObjectOrNull()?.takeIf { it.string("url") != null } }
        val hlsStreamUrl = hlsStream?.string("url") ?: return null
        val subtitleUrl = loadSubtitleUrl(hlsStream)
        val websiteUrl = metadata["link"]?.jsonObjectOrNull()?.string("url") ?: fallbackWebsiteUrl
        return OnlineSearchResult(
            provider = OnlineSearchProvider.ARTE,
            sender = "ARTE",
            topic = fallbackTopic,
            title = title,
            description = metadata.string("description").orEmpty(),
            websiteUrl = websiteUrl,
            normalQualityUrl = hlsStreamUrl,
            lowQualityUrl = hlsStreamUrl,
            highQualityUrl = hlsStreamUrl,
            subtitleUrl = subtitleUrl,
            broadcastTime = parseArteDate(attributes["rights"]?.jsonObjectOrNull()?.string("begin")),
            duration = metadata["duration"]?.jsonObjectOrNull()
                ?.get("seconds")?.jsonPrimitive?.longOrNull?.let(Duration::ofSeconds),
        )
    }

    private suspend fun loadSubtitleUrl(hlsStream: JsonObject): String {
        val hasGermanSubtitles = hlsStream["versions"]?.jsonArrayOrNull().orEmpty()
            .mapNotNull { it.jsonObjectOrNull() }
            .any { version ->
                version["closedCaptioning"]?.jsonPrimitive?.booleanOrNull == true &&
                    version.string("subtitleLanguage") in GERMAN_LANGUAGE_CODES
            }
        val masterUrl = hlsStream.string("url")
        if (!hasGermanSubtitles || masterUrl == null) {
            return ""
        }

        return try {
            val subtitlePlaylistUrl = get(masterUrl).hlsMediaAttributes()
                .firstOrNull { attributes ->
                    attributes["TYPE"] == "SUBTITLES" && attributes["LANGUAGE"] in GERMAN_LANGUAGE_CODES
                }
                ?.get("URI")
                ?.let { masterUrl.resolveUrl(it) }
                ?: return ""
            if (!subtitlePlaylistUrl.endsWith(".m3u8", ignoreCase = true)) {
                return subtitlePlaylistUrl
            }
            val subtitleFiles = get(subtitlePlaylistUrl).lineSequence()
                .map(String::trim)
                .filter { it.isNotEmpty() && !it.startsWith('#') }
                .toList()
            subtitleFiles.singleOrNull()?.let { subtitlePlaylistUrl.resolveUrl(it) }.orEmpty()
        } catch (ex: CancellationException) {
            throw ex
        } catch (_: Exception) {
            ""
        }
    }

    private fun String.hlsMediaAttributes(): Sequence<Map<String, String>> = lineSequence()
        .map(String::trim)
        .filter { it.startsWith("#EXT-X-MEDIA:") }
        .map { line ->
            HLS_ATTRIBUTE_PATTERN.findAll(line.substringAfter(':'))
                .associate { match ->
                    match.groupValues[1] to (match.groupValues[2].ifEmpty { match.groupValues[3] })
                }
        }

    private fun String.resolveUrl(reference: String): String = URI(this).resolve(reference).toString()

    private suspend fun get(url: String): String = rateLimitMutex.withLock {
        var retryDelayMillis = rateLimitState.requestDelayMillis.coerceIn(MIN_REQUEST_DELAY_MILLIS, MAX_REQUEST_DELAY_MILLIS)
        repeat(MAX_RATE_LIMIT_ATTEMPTS) { attempt ->
            if (attempt > 0) {
                delay(retryDelayMillis.milliseconds)
            }
            try {
                return httpClient.get(url)
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

    private companion object {
        private const val SEARCH_CONTENT_URL =
            "https://api.arte.tv/api/emac/v4/de/web/zones/f75e7b54-a37b-4f8f-b01b-dbefa877c041/content"
        private const val PLAYER_CONFIG_URL = "https://api.arte.tv/api/player/v2/config/de"
        private const val COLLECTION_SUBCOLLECTION_CONTENT_URL =
            "https://api-cdn.arte.tv/api/emac/v4/de/web/zones/8fbf35e4-324d-4d3e-a194-28f7d288789b/content"
        private const val MAX_RATE_LIMIT_ATTEMPTS = 3
        private const val MIN_REQUEST_DELAY_MILLIS = 250L
        private const val MAX_REQUEST_DELAY_MILLIS = 10_000L
        private val ARTE_PROGRAM_ID_PATTERN = Regex("""[0-9]{6}-[0-9A-Z]{3}-[A-Z]|RC-[0-9]{6}""")
        private val GERMAN_LANGUAGE_CODES = setOf("de", "deu")
        private val SUBCOLLECTION_ID_PATTERN = Regex("""subCollectionId(?:=|\\u003d)(RC-[0-9]{6})""")
        private val HLS_ATTRIBUTE_PATTERN = Regex("""([A-Z0-9-]+)=(?:"([^"]*)"|([^,]*))""")
        private val BERLIN: ZoneId = ZoneId.of("Europe/Berlin")
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
