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
import java.util.Locale
import kotlin.time.Duration.Companion.milliseconds

class ArteOnlineSearchService(
    private val httpClient: OnlineSearchHttpClient = MvOnlineSearchHttpClient,
    private val rateLimitState: ArteOnlineSearchRateLimitState = ApplicationConfigurationArteOnlineSearchRateLimitState,
) : OnlineSearchService {
    private val json = Json { ignoreUnknownKeys = true }
    private val rateLimitMutex = Mutex()

    override suspend fun search(request: OnlineSearchRequest): OnlineSearchPage {
        require(request.provider == OnlineSearchProvider.ARTE)
        val cursor = request.nextToken?.parseJsonObject(json)
        val pages = cursor?.get("pages")?.jsonObjectOrNull()?.mapValues { (_, page) -> page.jsonPrimitive.int }
            ?: CATALOGUE_LANGUAGES.associateWith { 1 }
        val seen = cursor?.get("seen")?.jsonArrayOrNull().orEmpty()
            .map { it.jsonPrimitive.content }.toMutableSet()
        val nextPages = linkedMapOf<String, Int>()
        val configCache = mutableMapOf<String, List<ArteLanguageResult>>()
        val results = ArrayList<OnlineSearchResult>()
        for ((language, pageNumber) in pages) {
            val root = get(searchUrl(request.query, pageNumber, language)).parseJsonObject(json)
            val items = root["data"]?.jsonArrayOrNull().orEmpty()
                .mapNotNull { it.jsonObjectOrNull() }
                .filter { it.hasVideoStreams() }
            for (item in items) {
                for (variant in loadFromSearchItem(item, language, configCache)) {
                    if (seen.add(variant.identity)) {
                        results.add(variant.result)
                    }
                }
            }
            val pagination = root["pagination"]?.jsonObjectOrNull()
            val currentPage = pagination?.get("page")?.jsonPrimitive?.intOrNull ?: pageNumber
            val pageCount = pagination?.get("pages")?.jsonPrimitive?.intOrNull
            if (pageCount != null && currentPage < pageCount) {
                nextPages[language] = currentPage + 1
            }
        }
        val nextToken = if (nextPages.isEmpty()) null else buildJsonObject {
            put("pages", buildJsonObject { nextPages.forEach { (language, page) -> put(language, page) } })
            put("seen", JsonArray(seen.map(::JsonPrimitive)))
        }.toString()
        // Catalogue counts overlap, and one programme may produce several audio-language results.
        val total = if (nextToken == null) seen.size.toLong() else null
        return OnlineSearchPage(results, nextToken, total)
    }

    override suspend fun loadByUrl(request: OnlineUrlRequest): OnlineSearchResult? {
        require(request.provider == OnlineSearchProvider.ARTE)
        val id = request.url.toArteProgramId()
        return loadByConfigUrl(
            configUrl = configUrl(id),
            fallbackTopic = "ARTE",
            fallbackWebsiteUrl = request.url,
        ).firstOrNull()?.result
    }

    private suspend fun loadFromSearchItem(
        item: JsonObject,
        language: String,
        configCache: MutableMap<String, List<ArteLanguageResult>>,
    ): List<ArteLanguageResult> {
        if (item.isCollection()) {
            val collectionResults = loadFromCollectionItem(item, language, configCache)
            if (collectionResults.isNotEmpty()) {
                return collectionResults
            }
        }
        val configUrl = item["player"]?.jsonObjectOrNull()?.string("config")
            ?: item.arteProgramId()?.let { configUrl(it, language) }
            ?: return emptyList()
        val fallbackTopic = item["genre"]?.jsonObjectOrNull()?.string("label") ?: "ARTE"
        return configCache.getOrPut(configUrl) {
            loadByConfigUrl(
                configUrl = configUrl,
                fallbackTopic = fallbackTopic,
                fallbackWebsiteUrl = item.string("url").orEmpty(),
            )
        }
    }

    private suspend fun loadFromCollectionItem(
        item: JsonObject,
        language: String,
        configCache: MutableMap<String, List<ArteLanguageResult>>,
    ): List<ArteLanguageResult> {
        val collectionId = item.arteProgramId() ?: return emptyList()
        val collectionUrl = item.string("url") ?: return emptyList()
        val topic = item.string("title") ?: item["genre"]?.jsonObjectOrNull()?.string("label") ?: "ARTE"
        return runCatching {
            loadCollectionItems(collectionUrl, collectionId, language)
                .flatMap { collectionItem -> loadFromSearchItem(collectionItem.withFallbackGenre(topic), language, configCache) }
        }.getOrElse {
            if (it is CancellationException) throw it
            emptyList()
        }
    }

    private suspend fun loadCollectionItems(collectionUrl: String, collectionId: String, language: String): List<JsonObject> {
        val html = get(collectionUrl)
        val subCollectionIds = html.extractSubCollectionIds(collectionId)
        return subCollectionIds.flatMap { subCollectionId ->
            loadSubCollectionItems(collectionId, subCollectionId, language)
        }.distinctBy { it.arteProgramId() }
    }

    private suspend fun loadSubCollectionItems(collectionId: String, subCollectionId: String, language: String): List<JsonObject> {
        val items = ArrayList<JsonObject>()
        var pageNumber = 1
        do {
            val root = get(subCollectionContentUrl(collectionId, subCollectionId, pageNumber, language))
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
    ): List<ArteLanguageResult> {
        val attributes = get(configUrl).parseJsonObject(json)["data"]?.jsonObjectOrNull()
            ?.get("attributes")?.jsonObjectOrNull()
            ?: return emptyList()
        val metadata = attributes["metadata"]?.jsonObjectOrNull() ?: return emptyList()
        val title = listOfNotNull(metadata.string("title"), metadata.string("subtitle"))
            .joinToString(" - ")
            .ifBlank { return emptyList() }
        val programId = (metadata.string("providerId")?.takeIf { it.isNotBlank() }
            ?: configUrl.substringBefore('?').substringAfterLast('/')).substringBefore('_')
        val websiteUrl = metadata["link"]?.jsonObjectOrNull()?.string("url") ?: fallbackWebsiteUrl
        val variants = attributes["streams"]?.jsonArrayOrNull().orEmpty()
            .mapNotNull { it.jsonObjectOrNull() }
            .flatMap { stream -> stream.audioVariants() }
            .sortedWith(compareBy<ArteAudioVariant> { it.language != "de" }.thenBy { it.audioDescription })
            .distinctBy { it.language }
        return variants.map { variant ->
            val languageLabel = variant.language?.let { Locale.forLanguageTag(it).getDisplayLanguage(Locale.GERMAN) }
            val displayTitle = if (languageLabel.isNullOrBlank()) title else "$title ($languageLabel)"
            val result = OnlineSearchResult(
                provider = OnlineSearchProvider.ARTE,
                sender = "ARTE",
                topic = fallbackTopic,
                title = displayTitle,
                description = metadata.string("description").orEmpty(),
                websiteUrl = websiteUrl,
                normalQualityUrl = variant.url,
                lowQualityUrl = variant.url,
                highQualityUrl = variant.url,
                subtitleUrl = loadSubtitleUrl(variant.stream, variant.language ?: "de"),
                broadcastTime = parseArteDate(attributes["rights"]?.jsonObjectOrNull()?.string("begin")),
                duration = metadata["duration"]?.jsonObjectOrNull()
                    ?.get("seconds")?.jsonPrimitive?.longOrNull?.let(Duration::ofSeconds),
                isAudioDescription = variant.audioDescription,
            )
            ArteLanguageResult("$programId:${variant.language.orEmpty()}", result)
        }
    }

    private fun JsonObject.audioVariants(): List<ArteAudioVariant> {
        val url = string("url")?.takeIf { it.isNotBlank() } ?: return emptyList()
        val versions = this["versions"]?.jsonArrayOrNull().orEmpty().mapNotNull { it.jsonObjectOrNull() }
        val manifest = ARTE_MANIFEST_PATTERN.matchEntire(url)
        // Older player responses expose one audio version per stream URL.
        val selectableVersions = if (manifest == null) versions.take(1) else versions
        return selectableVersions.mapNotNull { version ->
            val language = version.string("audioLanguage")?.let(::normalizeLanguage)
            val code = version.string("code")?.takeIf { it.isNotBlank() }
            // A version code selects a single audio track; the locale segment alone keeps all tracks.
            val versionUrl = if (manifest != null && code != null) {
                manifest.groupValues[1] + URLEncoder.encode(code, StandardCharsets.UTF_8) + manifest.groupValues[2]
            } else if (manifest != null && versions.size > 1) {
                return@mapNotNull null
            } else {
                url
            }
            ArteAudioVariant(language, versionUrl, this, version["audioDescription"]?.jsonPrimitive?.booleanOrNull == true)
        }.ifEmpty { listOf(ArteAudioVariant(null, url, this, false)) }
    }

    private fun normalizeLanguage(language: String): String = when (val code = language.lowercase(Locale.ROOT)) {
        "deu", "ger" -> "de"
        "fra", "fre" -> "fr"
        "eng" -> "en"
        "spa" -> "es"
        "ita" -> "it"
        "pol" -> "pl"
        "ron", "rum" -> "ro"
        else -> code
    }

    private suspend fun loadSubtitleUrl(hlsStream: JsonObject, language: String): String {
        val hasSubtitles = hlsStream["versions"]?.jsonArrayOrNull().orEmpty()
            .mapNotNull { it.jsonObjectOrNull() }
            .any { version ->
                version["closedCaptioning"]?.jsonPrimitive?.booleanOrNull == true &&
                    version.string("subtitleLanguage")?.let(::normalizeLanguage) == language
            }
        val masterUrl = hlsStream.string("url")
        if (!hasSubtitles || masterUrl == null) {
            return ""
        }

        return try {
            val subtitlePlaylistUrl = get(masterUrl).hlsMediaAttributes()
                .firstOrNull { attributes ->
                    attributes["TYPE"] == "SUBTITLES" && attributes["LANGUAGE"]?.let(::normalizeLanguage) == language
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

    private fun searchUrl(query: String, pageNumber: Int, language: String): String {
        val encodedQuery = URLEncoder.encode(query, StandardCharsets.UTF_8)
        return "$EMAC_BASE_URL/$language/web/zones/$SEARCH_ZONE/content?page=$pageNumber&query=$encodedQuery"
    }

    private fun configUrl(id: String, language: String = "de"): String = "$PLAYER_CONFIG_URL/$language/$id"

    private fun subCollectionContentUrl(collectionId: String, subCollectionId: String, pageNumber: Int, language: String): String =
        "$EMAC_CDN_BASE_URL/$language/web/zones/$COLLECTION_ZONE/content?collectionId=$collectionId" +
            "&page=$pageNumber&pageId=collection&subCollectionId=$subCollectionId&type=collection"

    private fun String.toArteProgramId(): String {
        val segments = trim().substringBefore('?').trimEnd('/').split('/')
        return segments.firstOrNull { ARTE_PROGRAM_ID_PATTERN.matches(it) }
            ?: toOnlineSearchUrlLastSegment()
    }

    private fun parseArteDate(value: String?): LocalDateTime? = value?.let {
        ZonedDateTime.parse(it).withZoneSameInstant(BERLIN).toLocalDateTime()
    }

    private fun JsonObject.arteProgramId(): String? =
        (string("programId") ?: string("id"))?.substringBefore('_')

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
        private val CATALOGUE_LANGUAGES = listOf("de", "fr", "en", "es", "pl", "it", "ro")
        private const val EMAC_BASE_URL = "https://api.arte.tv/api/emac/v4"
        private const val EMAC_CDN_BASE_URL = "https://api-cdn.arte.tv/api/emac/v4"
        private const val SEARCH_ZONE = "f75e7b54-a37b-4f8f-b01b-dbefa877c041"
        private const val COLLECTION_ZONE = "8fbf35e4-324d-4d3e-a194-28f7d288789b"
        private const val PLAYER_CONFIG_URL = "https://api.arte.tv/api/player/v2/config"
        private const val MAX_RATE_LIMIT_ATTEMPTS = 3
        private const val MIN_REQUEST_DELAY_MILLIS = 250L
        private const val MAX_REQUEST_DELAY_MILLIS = 10_000L
        private val ARTE_PROGRAM_ID_PATTERN = Regex("""[0-9]{6}-[0-9A-Z]{3}-[A-Z]|RC-[0-9]{6}""")
        private val ARTE_MANIFEST_PATTERN =
            Regex("""(https://manifest-arte\.akamaized\.net/api/manifest/v1/Generate/[^/]+/)[^/]+(/.*)""")
        private val SUBCOLLECTION_ID_PATTERN = Regex("""subCollectionId(?:=|\\u003d)(RC-[0-9]{6})""")
        private val HLS_ATTRIBUTE_PATTERN = Regex("""([A-Z0-9-]+)=(?:"([^"]*)"|([^,]*))""")
        private val BERLIN: ZoneId = ZoneId.of("Europe/Berlin")
    }
}

private data class ArteLanguageResult(val identity: String, val result: OnlineSearchResult)

private data class ArteAudioVariant(
    val language: String?,
    val url: String,
    val stream: JsonObject,
    val audioDescription: Boolean,
)

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
