package mediathek.gui.tabs.tab_online_search

import kotlinx.serialization.json.*
import java.net.URLEncoder
import java.nio.charset.StandardCharsets
import java.time.Clock
import java.time.Duration
import java.time.Instant
import java.time.LocalDateTime
import java.time.ZoneId
import java.time.ZonedDateTime

private const val ZDF_APP_ID = "ffw-mt-web-036df51e"

fun interface ZdfGraphqlSearchLoader {
    suspend fun load(query: String, cursor: String?, bearer: String): ZdfSearchGraphqlResult
}

data class ZdfSearchGraphqlResult(
    val canonicalPaths: List<String>,
    val nextCursor: String?,
    val totalResults: Long?,
)

class ZdfOnlineSearchService(
    private val httpClient: OnlineSearchHttpClient = MvOnlineSearchHttpClient,
    private val clock: Clock = Clock.systemDefaultZone(),
    private val graphqlLoader: ZdfGraphqlSearchLoader = ZdfDefaultGraphqlSearchLoader(httpClient),
) : OnlineSearchService {
    private val json = Json { ignoreUnknownKeys = true }
    private var cachedToken: ZdfApiToken? = null

    override suspend fun search(request: OnlineSearchRequest): OnlineSearchPage {
        require(request.provider == OnlineSearchProvider.ZDF)
        return withFreshTokenRetry { bearer ->
            val searchResult = graphqlLoader.load(request.query, request.nextToken, bearer)
            val results = searchResult.canonicalPaths.mapConcurrently { loadDocument(it, bearer) }.flatten()
            OnlineSearchPage(results, searchResult.nextCursor, searchResult.totalResults)
        }
    }

    override suspend fun loadByUrl(request: OnlineUrlRequest): OnlineSearchResult? {
        require(request.provider == OnlineSearchProvider.ZDF)
        val canonical = request.url.toOnlineSearchUrlLastSegment()
        return withFreshTokenRetry { bearer -> loadDocument(canonical, bearer).firstOrNull() }
    }

    private suspend fun bearerToken(): String {
        cachedToken?.takeIf { it.isUsable(clock.instant()) }?.let { return it.value }
        return refreshBearerToken()
    }

    private suspend fun refreshBearerToken(): String {
        val html = httpClient.get(ZDF_SEARCH_URL)
        return ZdfTokenExtractor.extract(html)?.also { cachedToken = it }?.value
            ?: error("ZDF API token not found")
    }

    private suspend fun <T> withFreshTokenRetry(block: suspend (String) -> T): T {
        val bearer = bearerToken()
        return try {
            block(bearer)
        } catch (ex: OnlineSearchHttpException) {
            if (!ex.isAuthorizationFailure) {
                throw ex
            }
            cachedToken = null
            block(refreshBearerToken())
        }
    }

    private fun ZdfApiToken.isUsable(now: Instant): Boolean =
        expiresAt == null || expiresAt.isAfter(now.plus(TOKEN_EXPIRY_SAFETY_MARGIN))

    private suspend fun loadDocument(canonical: String, bearer: String): List<OnlineSearchResult> {
        val root = httpClient.get("https://api.zdf.de/content/documents/$canonical.json", zdfHeaders(bearer))
            .parseJsonObject(json)
        val mainVideo = root["mainVideoContent"]?.jsonObjectOrNull() ?: return emptyList()
        val targetVideo = mainVideo["http://zdf.de/rels/target"]?.jsonObjectOrNull() ?: mainVideo
        val ptmdTemplates = (targetVideo.findPtmdTemplates() + mainVideo.findPtmdTemplates())
            .distinctBy { it.template }
            .ifEmpty { return emptyList() }
        val downloadVariants = ptmdTemplates.mapConcurrently { stream ->
            val ptmdUrl = stream.template.replace("{playerId}", "android_native_5").withZdfApiBase()
            loadDownloadVariants(ptmdUrl, bearer, stream.isSignLanguage)
        }.flatten()
        val title = listOfNotNull(root.string("title"), root.string("subtitle"))
            .joinToString(" - ")
            .ifBlank { return emptyList() }
        val topic = root.zdfTopic() ?: "ZDF"
        return downloadVariants.map { variant ->
            OnlineSearchResult(
                provider = OnlineSearchProvider.ZDF,
                sender = "ZDF",
                topic = topic,
                title = variant.displayTitle(title),
                description = root.string("leadParagraph") ?: root.string("teasertext").orEmpty(),
                websiteUrl = root.string("http://zdf.de/rels/sharing-url").orEmpty(),
                normalQualityUrl = variant.normalUrl,
                lowQualityUrl = variant.lowUrl,
                highQualityUrl = variant.highUrl,
                subtitleUrl = variant.subtitleUrl,
                broadcastTime = parseZdfDate(root.string("editorialDate")),
                duration = mainVideo["http://zdf.de/rels/target"]?.jsonObjectOrNull()
                    ?.get("duration")?.jsonPrimitive?.longOrNull?.let(Duration::ofSeconds),
                isSignLanguage = variant.isSignLanguage,
                isAudioDescription = variant.isAudioDescription,
            )
        }
    }

    private suspend fun loadDownloadVariants(
        ptmdUrl: String,
        bearer: String,
        isSignLanguage: Boolean,
    ): List<ZdfDownloadVariant> =
        httpClient.get(ptmdUrl, zdfHeaders(bearer)).parseJsonObject(json).downloadVariants(isSignLanguage)

    private fun parseZdfDate(value: String?): LocalDateTime? = value?.let {
        ZonedDateTime.parse(it).withZoneSameInstant(BERLIN).toLocalDateTime()
    }

    private fun String.withZdfApiBase(): String = if (startsWith("http://") || startsWith("https://")) {
        this
    } else if (startsWith("/")) {
        "https://api.zdf.de$this"
    } else {
        "https://api.zdf.de/$this"
    }

    private fun zdfHeaders(bearer: String): Map<String, String> = mapOf(
        "Referer" to "https://www.zdf.de/",
        "content-type" to "application/json",
        "zdf-app-id" to ZDF_APP_ID,
        "api-auth" to "Bearer $bearer",
        "Origin" to "https://www.zdf.de",
    )

    private fun JsonObject.zdfTopic(): String? =
        this["http://zdf.de/rels/brand"]?.jsonObjectOrNull()?.string("title")
            ?: this["programmeItem"]?.jsonArrayOrNull()?.firstOrNull()?.jsonObjectOrNull()
                ?.get("http://zdf.de/rels/target")?.jsonObjectOrNull()?.string("primaryBrand")
            ?: string("title")

    private fun JsonObject.findPtmdTemplates(): List<ZdfPtmdStreamTemplate> {
        val templates = mutableListOf<ZdfPtmdStreamTemplate>()
        string("http://zdf.de/rels/streams/ptmd-template")?.let { template ->
            templates += ZdfPtmdStreamTemplate("default", template)
        }
        this["streams"]?.jsonObjectOrNull()?.forEach { (kind, stream) ->
            stream.jsonObjectOrNull()?.string("http://zdf.de/rels/streams/ptmd-template")?.let { template ->
                templates += ZdfPtmdStreamTemplate(kind, template)
            }
        }
        return templates
    }

    private fun JsonObject.downloadVariants(isSignLanguage: Boolean): List<ZdfDownloadVariant> {
        val variants = linkedMapOf<String, MutableZdfDownloadVariant>()
        this["priorityList"]?.jsonArrayOrNull().orEmpty().forEach { priority ->
            priority.jsonObjectOrNull()?.get("formitaeten")?.jsonArrayOrNull().orEmpty().forEach { format ->
                val formatObject = format.jsonObjectOrNull() ?: return@forEach
                if (!formatObject.string("mimeType").equals("video/mp4", ignoreCase = true)) {
                    return@forEach
                }
                val downloads = mutableListOf<ZdfDownloadInfo>()
                formatObject["qualities"]?.jsonArrayOrNull().orEmpty().forEach { quality ->
                    val qualityObject = quality.jsonObjectOrNull() ?: return@forEach
                    val qualityValue = qualityObject.string("quality").toZdfQuality()
                    val verticalResolution = qualityObject["highestVerticalResolution"]?.jsonPrimitive?.intOrNull ?: 0
                    qualityObject["audio"]?.jsonObjectOrNull()?.get("tracks")?.jsonArrayOrNull().orEmpty().forEach { track ->
                        val info = track.jsonObjectOrNull()
                            ?.toDownloadInfo(qualityValue, verticalResolution, isSignLanguage)
                            ?: return@forEach
                        downloads.add(info)
                        if (downloads.size == 1 && qualityValue != ZdfVideoQuality.NORMAL) {
                            downloads.add(info.copy(quality = ZdfVideoQuality.NORMAL))
                        }
                    }
                }
                downloads.sortedBy { it.verticalResolution }.forEach { info ->
                    variants.getOrPut(info.language) { MutableZdfDownloadVariant(info.language) }
                        .setUrl(info.quality, info.uri)
                }
            }
        }
        val subtitles = subtitlesByLanguage()
        return variants.values
            .mapNotNull { it.toVariant(subtitles[it.baseLanguage].orEmpty()) }
    }

    private fun JsonObject.subtitlesByLanguage(): Map<String, String> {
        val subtitles = linkedMapOf<String, String>()
        this["captions"]?.jsonArrayOrNull().orEmpty().forEach { caption ->
            val captionObject = caption.jsonObjectOrNull() ?: return@forEach
            val uri = captionObject.string("uri") ?: return@forEach
            val language = captionObject.string("language").orEmpty()
            if (uri.endsWith(".xml") || subtitles[language].isNullOrEmpty()) {
                subtitles[language] = uri
            }
        }
        return subtitles
    }

    private fun JsonObject.toDownloadInfo(
        quality: ZdfVideoQuality,
        verticalResolution: Int,
        isSignLanguage: Boolean,
    ): ZdfDownloadInfo? {
        val uri = string("uri") ?: return null
        val baseLanguage = string("language").orEmpty()
        val language = when {
            isSignLanguage -> "$baseLanguage-dgs"
            string("class").equals("ad", ignoreCase = true) -> "$baseLanguage-ad"
            else -> baseLanguage
        }
        return ZdfDownloadInfo(language, quality, verticalResolution, uri)
    }

    private fun String?.toZdfQuality(): ZdfVideoQuality = when (this) {
        "veryhigh" -> ZdfVideoQuality.NORMAL
        "hd", "fhd" -> ZdfVideoQuality.HIGH
        else -> ZdfVideoQuality.LOW
    }

    private companion object {
        private const val ZDF_SEARCH_URL = "https://www.zdf.de/suche"
        private val TOKEN_EXPIRY_SAFETY_MARGIN: Duration = Duration.ofMinutes(1)
        private val BERLIN: ZoneId = ZoneId.of("Europe/Berlin")
    }
}

private enum class ZdfVideoQuality {
    LOW,
    NORMAL,
    HIGH,
}

private data class ZdfDownloadInfo(
    val language: String,
    val quality: ZdfVideoQuality,
    val verticalResolution: Int,
    val uri: String,
)

private data class ZdfPtmdStreamTemplate(
    val kind: String,
    val template: String,
) {
    val isSignLanguage: Boolean
        get() = kind.equals("dgs", ignoreCase = true)
}

private data class ZdfDownloadVariant(
    val language: String,
    val normalUrl: String,
    val lowUrl: String = "",
    val highUrl: String = "",
    val subtitleUrl: String = "",
) {
    val isAudioDescription: Boolean
        get() = language.endsWith("-ad")

    val isSignLanguage: Boolean
        get() = language.endsWith("-dgs")

    fun displayTitle(baseTitle: String): String = when {
        isAudioDescription -> "$baseTitle (Audiodeskription)"
        isSignLanguage -> "$baseTitle (Gebärdensprache)"
        language == "eng" -> "$baseTitle (Englisch)"
        language == "fra" -> "$baseTitle (Französisch)"
        language == "deu" -> baseTitle
        else -> "$baseTitle ($language)"
    }
}

private class MutableZdfDownloadVariant(
    val language: String,
) {
    val baseLanguage: String = language.substringBefore('-')
    private var lowUrl: String = ""
    private var normalUrl: String = ""
    private var highUrl: String = ""

    fun setUrl(quality: ZdfVideoQuality, uri: String) {
        when (quality) {
            ZdfVideoQuality.LOW -> lowUrl = uri
            ZdfVideoQuality.NORMAL -> normalUrl = uri
            ZdfVideoQuality.HIGH -> highUrl = uri
        }
    }

    fun toVariant(subtitleUrl: String): ZdfDownloadVariant? {
        val normal = normalUrl.ifEmpty { return null }
        return ZdfDownloadVariant(
            language = language,
            normalUrl = normal,
            lowUrl = lowUrl,
            highUrl = highUrl,
            subtitleUrl = subtitleUrl,
        )
    }
}

private class ZdfDefaultGraphqlSearchLoader(
    private val httpClient: OnlineSearchHttpClient,
) : ZdfGraphqlSearchLoader {
    private val json = Json { ignoreUnknownKeys = true }

    override suspend fun load(query: String, cursor: String?, bearer: String): ZdfSearchGraphqlResult {
        val body = httpClient.get(ZdfGraphqlUrlFactory.build(query, cursor), zdfHeaders(bearer))
        val root = body.parseJsonObject(json)
        val searchDocuments = root["data"]?.jsonObjectOrNull()?.get("searchDocuments")?.jsonObjectOrNull()
            ?: return ZdfSearchGraphqlResult(emptyList(), null, null)
        val pageInfo = searchDocuments["pageInfo"]?.jsonObjectOrNull()
        val nextCursor = if (pageInfo?.get("hasNextPage")?.jsonPrimitive?.booleanOrNull == true) {
            pageInfo["endCursor"]?.jsonPrimitive?.contentOrNull
        } else {
            null
        }
        val paths = searchDocuments["results"]?.jsonArrayOrNull().orEmpty()
            .mapNotNull { result ->
                result.jsonObjectOrNull()?.get("item")?.jsonObjectOrNull()?.get("canonical")?.jsonPrimitive?.contentOrNull
            }
        return ZdfSearchGraphqlResult(paths, nextCursor, null)
    }

    private fun zdfHeaders(bearer: String): Map<String, String> = mapOf(
        "Referer" to "https://www.zdf.de/",
        "content-type" to "application/json",
        "zdf-app-id" to ZDF_APP_ID,
        "api-auth" to "Bearer $bearer",
        "Origin" to "https://www.zdf.de",
    )
}

internal object ZdfGraphqlUrlFactory {
    fun build(query: String, cursor: String?): String {
        val variables = buildJsonObject {
            put("query", query)
            put("mode", "ALL_RESULTS_EXCLUDING_TOP_RESULTS")
            put(
                "filters",
                buildJsonObject {
                    put("contentOwner", JsonArray(emptyList()))
                    put("fsk", JsonArray(emptyList()))
                    put("language", JsonArray(emptyList()))
                },
            )
            put("first", 24)
            if (cursor == null) {
                put("after", JsonNull)
            } else {
                put("after", cursor)
            }
        }.toString()
        return "https://api.zdf.de/graphql?operationName=getSearchResults" +
            "&variables=${URLEncoder.encode(variables, StandardCharsets.UTF_8)}" +
            "&query=${URLEncoder.encode(SEARCH_QUERY, StandardCharsets.UTF_8)}"
    }

    private const val SEARCH_QUERY =
        "query getSearchResults(\$query: String!, \$mode: SearchMode, \$first: Int, \$after: Cursor, " +
            "\$filters: SearchFilters, \$group: String) { " +
            "searchDocuments(query: \$query, mode: \$mode, first: \$first, after: \$after, filters: \$filters, " +
            "group: \$group) { " +
            "pageInfo { hasNextPage endCursor } " +
            "results { item { __typename ... on IBaseDocument { canonical } ... on ISmartCollection { canonical } " +
            "... on CuratedCollection { canonical } ... on MetaCollection { canonical } } } " +
            "} " +
            "}"
}
