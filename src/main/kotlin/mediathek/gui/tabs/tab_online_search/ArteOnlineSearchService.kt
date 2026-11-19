package mediathek.gui.tabs.tab_online_search

import kotlinx.coroutines.CancellationException
import kotlinx.coroutines.delay
import kotlinx.coroutines.sync.Mutex
import kotlinx.coroutines.sync.withLock
import kotlinx.serialization.json.*
import mediathek.config.application.ApplicationConfiguration
import okhttp3.HttpUrl.Companion.toHttpUrl
import java.net.ProtocolException
import java.net.URI
import java.net.URLEncoder
import java.nio.charset.StandardCharsets
import java.time.Duration
import java.time.LocalDateTime
import java.time.ZoneId
import java.time.ZonedDateTime
import java.util.Locale
import java.util.concurrent.ConcurrentHashMap
import kotlin.time.ComparableTimeMark
import kotlin.time.Duration as WaitDuration
import kotlin.time.Duration.Companion.milliseconds
import kotlin.time.Duration.Companion.minutes
import kotlin.time.TimeSource
import kotlin.time.toKotlinDuration

class ArteOnlineSearchService internal constructor(
    private val httpClient: OnlineSearchHttpClient,
    private val rateLimitState: ArteOnlineSearchRateLimitState,
    private val timeSource: TimeSource.WithComparableMarks,
    private val waitFor: suspend (WaitDuration) -> Unit,
) : OnlineSearchService {
    constructor(
        httpClient: OnlineSearchHttpClient = MvOnlineSearchHttpClient,
        rateLimitState: ArteOnlineSearchRateLimitState = ApplicationConfigurationArteOnlineSearchRateLimitState,
    ) : this(httpClient, rateLimitState, TimeSource.Monotonic, { delay(it) })

    private val json = Json { ignoreUnknownKeys = true }
    private val hostRateLimits = ConcurrentHashMap<String, HostRateLimit>()

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

    private suspend fun get(url: String): String {
        val originalUrl = url.toHttpUrl()
        var requestUrl = originalUrl
        var originalRequestStartedAt: ComparableTimeMark? = null
        var redirects = 0
        var rateLimitAttempts = 0
        while (true) {
            val rateLimit = hostRateLimits.computeIfAbsent(requestUrl.host) { host ->
                HostRateLimit(rateLimitState.takeIf { host == ARTE_API_HOST })
            }
            val hop = try {
                rateLimit.getHop(requestUrl.toString(), originalRequestStartedAt)
            } catch (ex: OnlineSearchHttpException) {
                if (!ex.isRateLimit || ++rateLimitAttempts >= MAX_RATE_LIMIT_ATTEMPTS) {
                    throw ex
                }
                if (redirects > 0) {
                    rateLimit.awaitRetry()
                }
                null
            }
            when (val response = hop?.response) {
                is OnlineSearchHttpHop.Body -> return response.text
                is OnlineSearchHttpHop.Redirect -> {
                    if (++redirects > MAX_REDIRECTS) {
                        throw ProtocolException("Too many ARTE redirects for $url")
                    }
                    requestUrl = response.url
                    originalRequestStartedAt = originalRequestStartedAt ?: hop.startedAt
                }
                null -> {
                    requestUrl = originalUrl
                    originalRequestStartedAt = null
                    redirects = 0
                }
            }
        }
    }

    private data class TimedHop(val response: OnlineSearchHttpHop, val startedAt: ComparableTimeMark)

    private inner class HostRateLimit(private val persistedState: ArteOnlineSearchRateLimitState?) {
        private val rateLimitMutex = Mutex()
        private var lastRequestFinishedAt: ComparableTimeMark? = null
        private var retryNotBefore: ComparableTimeMark? = null
        private var consecutiveSuccesses = 0
        private var delayBeforeReductionMillis = 0L
        private var learnedDelayFloorMillis = 0L
        private var inMemoryDelayMillis = 0L
        private var requestDelayMillis: Long
            get() = persistedState?.requestDelayMillis ?: inMemoryDelayMillis
            set(value) {
                if (persistedState == null) {
                    inMemoryDelayMillis = value
                } else {
                    persistedState.requestDelayMillis = value
                }
            }
        private val learnedDelayMillis: Long
            get() = requestDelayMillis.coerceIn(0, MAX_REQUEST_DELAY_MILLIS)

        suspend fun awaitRetry() {
            rateLimitMutex.withLock { waitForRequest(learnedDelayMillis) }
        }

        suspend fun getHop(url: String, originalRequestStartedAt: ComparableTimeMark?): TimedHop? = rateLimitMutex.withLock {
            val cooldownDeadline = retryNotBefore
            // Keep the deadline so queued or delayed redirects can be checked against the original request.
            if (originalRequestStartedAt != null && cooldownDeadline != null && originalRequestStartedAt < cooldownDeadline) {
                waitUntil(cooldownDeadline)
                return@withLock null
            }
            lastRequestFinishedAt?.let { finishedAt ->
                val idleSince = maxOf(finishedAt, cooldownDeadline ?: finishedAt)
                if (idleSince.elapsedNow() >= RATE_LIMIT_IDLE_RESET) {
                    consecutiveSuccesses = 0
                    delayBeforeReductionMillis = 0
                    learnedDelayFloorMillis = 0
                }
            }

            val currentDelayMillis = learnedDelayMillis
            waitForRequest(currentDelayMillis)
            val startedAt = timeSource.markNow()
            var retryDelay: WaitDuration? = null
            try {
                val hop = httpClient.getHop(url)
                if (currentDelayMillis > learnedDelayFloorMillis) {
                    consecutiveSuccesses++
                    if (consecutiveSuccesses == SUCCESSES_BEFORE_DELAY_REDUCTION) {
                        delayBeforeReductionMillis = currentDelayMillis
                        requestDelayMillis = (currentDelayMillis * 3 / 4).coerceAtLeast(learnedDelayFloorMillis)
                        consecutiveSuccesses = 0
                    }
                }
                TimedHop(hop, startedAt)
            } catch (ex: Exception) {
                consecutiveSuccesses = 0
                if (ex is OnlineSearchHttpException && ex.isRateLimit) {
                    // A failed reduction pins the previous successful interval until traffic goes idle.
                    learnedDelayFloorMillis = maxOf(learnedDelayFloorMillis, delayBeforeReductionMillis)
                    delayBeforeReductionMillis = 0
                    // Retry-After governs the next request. Only the recurring interval is capped.
                    val serverCooldown = ex.retryAfter?.toKotlinDuration() ?: WaitDuration.ZERO
                    val backoff = currentDelayMillis.coerceAtLeast(MIN_REQUEST_DELAY_MILLIS) * 2
                    requestDelayMillis = maxOf(backoff.milliseconds, serverCooldown)
                        .coerceAtMost(MAX_REQUEST_DELAY_MILLIS.milliseconds)
                        .inWholeMilliseconds
                        .coerceAtLeast(learnedDelayFloorMillis)
                    retryDelay = maxOf(requestDelayMillis.milliseconds, serverCooldown)
                }
                throw ex
            } finally {
                val finishedAt = timeSource.markNow()
                lastRequestFinishedAt = finishedAt
                retryDelay?.let { retryNotBefore = finishedAt + it }
            }
        }

        private suspend fun waitForRequest(learnedDelayMillis: Long) {
            val nextRequestAt = lastRequestFinishedAt?.plus(learnedDelayMillis.milliseconds) ?: return
            waitUntil(maxOf(nextRequestAt, retryNotBefore ?: nextRequestAt))
        }

        private suspend fun waitUntil(deadline: ComparableTimeMark) {
            val remaining = -deadline.elapsedNow()
            if (remaining > WaitDuration.ZERO) {
                waitFor(remaining)
            }
        }
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
        private const val ARTE_API_HOST = "api.arte.tv"
        private const val EMAC_BASE_URL = "https://$ARTE_API_HOST/api/emac/v4"
        private const val EMAC_CDN_BASE_URL = "https://api-cdn.arte.tv/api/emac/v4"
        private const val SEARCH_ZONE = "f75e7b54-a37b-4f8f-b01b-dbefa877c041"
        private const val COLLECTION_ZONE = "8fbf35e4-324d-4d3e-a194-28f7d288789b"
        private const val PLAYER_CONFIG_URL = "https://$ARTE_API_HOST/api/player/v2/config"
        private const val MAX_RATE_LIMIT_ATTEMPTS = 3
        private const val MAX_REDIRECTS = 20
        private val RATE_LIMIT_IDLE_RESET = 1.minutes
        private const val SUCCESSES_BEFORE_DELAY_REDUCTION = 5
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
