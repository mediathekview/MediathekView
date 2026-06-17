package mediathek.gui.tabs.tab_online_search

import kotlinx.serialization.json.*
import java.net.URLEncoder
import java.nio.charset.StandardCharsets
import java.time.Duration
import java.time.ZoneId
import java.time.ZonedDateTime

class ArdOnlineSearchService(
    private val httpClient: OnlineSearchHttpClient = MvOnlineSearchHttpClient,
) : OnlineSearchService {
    private val json = Json { ignoreUnknownKeys = true }

    override suspend fun search(request: OnlineSearchRequest): OnlineSearchPage {
        require(request.provider == OnlineSearchProvider.ARD)
        val pageNumber = request.nextToken?.toIntOrNull() ?: 0
        val root = httpClient.get(searchUrl(request.query, pageNumber)).parseJsonObject(json)
        val total = root["pagination"]?.jsonObject?.get("totalElements")?.jsonPrimitive?.longOrNull
        val ids = root["teasers"]?.jsonArray.orEmpty()
            .mapNotNull { element -> element.jsonObject["id"]?.jsonPrimitive?.contentOrNull }
        val results = ids.mapConcurrently { loadById(it) }.filterNotNull()
        val nextToken = if (total != null && (pageNumber + 1L) * PAGE_SIZE < total) {
            (pageNumber + 1).toString()
        } else {
            null
        }
        return OnlineSearchPage(results, nextToken, total)
    }

    override suspend fun loadByUrl(request: OnlineUrlRequest): OnlineSearchResult? {
        require(request.provider == OnlineSearchProvider.ARD)
        val id = request.url.toOnlineSearchUrlLastSegment()
        return loadById(id)
    }

    private suspend fun loadById(id: String): OnlineSearchResult? {
        val root = httpClient.get("$ITEM_URL$id").parseJsonObject(json)
        val item = root["widgets"]?.jsonArray?.firstOrNull()?.jsonObject ?: return null
        val title = item.string("title") ?: return null
        val topic = item["show"]?.jsonObjectOrNull()?.string("title") ?: title
        val media = item["mediaCollection"]?.jsonObjectOrNull()?.get("embedded")?.jsonObjectOrNull()
        val streamUrl = media?.streamUrl() ?: return null
        val duration = media.durationSeconds()?.let(Duration::ofSeconds)
        val broadcast = item.string("broadcastedOn")?.let {
            ZonedDateTime.parse(it).withZoneSameInstant(BERLIN).toLocalDateTime()
        }
        return OnlineSearchResult(
            provider = OnlineSearchProvider.ARD,
            sender = "ARD",
            topic = topic,
            title = title.replace("Hörfassung", "Audiodeskription"),
            description = item.string("synopsis").orEmpty(),
            websiteUrl = "https://www.ardmediathek.de/video/$id",
            normalQualityUrl = streamUrl,
            broadcastTime = broadcast,
            duration = duration,
        )
    }

    private fun searchUrl(query: String, pageNumber: Int): String {
        val encodedQuery = URLEncoder.encode(query, StandardCharsets.UTF_8)
        return "https://api.ardmediathek.de/search-system/search/vods/ard" +
            "?query=$encodedQuery&pageNumber=$pageNumber&pageSize=$PAGE_SIZE" +
            "&audioDes=false&signLang=false&subtitle=false&childCont=false" +
            "&sortingCriteria=SCORE_DESC&platform=MEDIA_THEK"
    }

    private fun JsonObject.streamUrl(): String? = this["streams"]?.jsonArray.orEmpty()
        .asSequence()
        .flatMap { stream -> stream.jsonObjectOrNull()?.get("media")?.jsonArray.orEmpty().asSequence() }
        .firstNotNullOfOrNull { media -> media.jsonObjectOrNull()?.string("url") }
        ?: findFirstValue("_stream")

    private fun JsonObject.durationSeconds(): Long? =
        this["meta"]?.jsonObjectOrNull()?.get("durationSeconds")?.jsonPrimitive?.longOrNull
            ?: this["_duration"]?.jsonPrimitive?.longOrNull

    private fun JsonElement.findFirstValue(name: String): String? = when (this) {
        is JsonObject -> this[name]?.jsonPrimitive?.contentOrNull
            ?: values.firstNotNullOfOrNull { it.findFirstValue(name) }

        is JsonArray -> asSequence().firstNotNullOfOrNull { it.findFirstValue(name) }
        else -> null
    }

    private companion object {
        private const val PAGE_SIZE = 20
        private const val ITEM_URL = "https://api.ardmediathek.de/page-gateway/pages/ard/item/"
        private val BERLIN: ZoneId = ZoneId.of("Europe/Berlin")
    }
}

internal fun String.toOnlineSearchUrlLastSegment(): String =
    trim().substringBefore('?').trimEnd('/').substringAfterLast('/')
