package mediathek.gui.tabs.tab_online_search

import kotlinx.coroutines.runBlocking
import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Assertions.assertTrue
import org.junit.jupiter.api.Test
import java.time.Duration
import java.time.LocalDateTime

class ArteOnlineSearchServiceTest {
    @Test
    fun `search loads ARTE results and resolves playable config`() = runBlocking {
        val httpClient = FakeOnlineSearchHttpClient(
            mapOf(
                "$SEARCH_CONTENT_URL?page=1&query=tatort" to arteSearchJson(),
                "$PLAYER_CONFIG_URL/118267-006-A" to arteConfigJson(),
                "$OPA_STREAM_URL/118267-006-A/SHOW/de" to arteOpaStreamsJson(),
            ),
        )
        val service = testArteService(httpClient)

        val page = service.search(OnlineSearchRequest(OnlineSearchProvider.ARTE, "tatort"))

        assertEquals(1, page.results.size)
        assertEquals(null, page.nextToken)
        assertEquals(1, page.totalResults)
        val result = page.results.single()
        assertEquals(OnlineSearchProvider.ARTE, result.provider)
        assertEquals("ARTE", result.sender)
        assertEquals("Dokus und Reportagen", result.topic)
        assertEquals("Re: Tatort Kirche - Betroffene klagen an", result.title)
        assertEquals("https://www.arte.tv/de/videos/118267-006-A/re-tatort-kirche-betroffene-klagen-an/", result.websiteUrl)
        assertEquals("https://arte-cdn.example/118267-006-A_EQ.mp4", result.normalQualityUrl)
        assertEquals("https://arte-cdn.example/118267-006-A_SQ.mp4", result.highQualityUrl)
        assertEquals("https://arte-cdn.example/118267-006-A_HQ.mp4", result.lowQualityUrl)
        assertEquals(Duration.ofSeconds(1815), result.duration)
        assertEquals(LocalDateTime.of(2025, 2, 25, 5, 0), result.broadcastTime)
        val streamRequestIndex = httpClient.requestedUrls.indexOf("$OPA_STREAM_URL/118267-006-A/SHOW/de")
        assertEquals(
            mapOf("Authorization" to ARTE_STREAM_API_TOKEN),
            httpClient.requestedHeaders[streamRequestIndex],
        )
    }

    @Test
    fun `search follows pagination with numeric next token`() = runBlocking {
        val httpClient = FakeOnlineSearchHttpClient(
            mapOf(
                "$SEARCH_CONTENT_URL?page=2&query=klima" to arteSearchJson(page = 2, pages = 3, totalCount = 42),
                "$PLAYER_CONFIG_URL/118267-006-A" to arteConfigJson(),
            ),
        )
        val service = testArteService(httpClient)

        val page = service.search(OnlineSearchRequest(OnlineSearchProvider.ARTE, "klima", nextToken = "2"))

        assertEquals("3", page.nextToken)
        assertEquals(42, page.totalResults)
    }

    @Test
    fun `url lookup extracts ARTE program id and loads config`() = runBlocking {
        val httpClient = FakeOnlineSearchHttpClient(
            mapOf(
                "$PLAYER_CONFIG_URL/118267-006-A" to arteConfigJson(),
                "$OPA_STREAM_URL/118267-006-A/SHOW/de" to arteOpaStreamsJson(),
            ),
        )
        val service = testArteService(httpClient)

        val result = service.loadByUrl(
            OnlineUrlRequest(
                OnlineSearchProvider.ARTE,
                "https://www.arte.tv/de/videos/118267-006-A/re-tatort-kirche-betroffene-klagen-an/",
            ),
        )

        assertEquals("Re: Tatort Kirche - Betroffene klagen an", result?.title)
        assertEquals("https://arte-cdn.example/118267-006-A_EQ.mp4", result?.normalQualityUrl)
        assertEquals(
            listOf("$PLAYER_CONFIG_URL/118267-006-A", "$OPA_STREAM_URL/118267-006-A/SHOW/de"),
            httpClient.requestedUrls,
        )
    }

    @Test
    fun `search skips results without playable streams`() = runBlocking {
        val httpClient = FakeOnlineSearchHttpClient(
            mapOf(
                "$SEARCH_CONTENT_URL?page=1&query=tatort" to arteSearchJson(),
                "$PLAYER_CONFIG_URL/118267-006-A" to arteConfigJson(streamUrl = null),
            ),
        )
        val service = testArteService(httpClient)

        val page = service.search(OnlineSearchRequest(OnlineSearchProvider.ARTE, "tatort"))

        assertEquals(emptyList<OnlineSearchResult>(), page.results)
    }

    @Test
    fun `search uses OPA mp4 streams when player config has no HLS stream`() = runBlocking {
        val httpClient = FakeOnlineSearchHttpClient(
            mapOf(
                "$SEARCH_CONTENT_URL?page=1&query=tatort" to arteSearchJson(),
                "$PLAYER_CONFIG_URL/118267-006-A" to arteConfigJson(streamUrl = null),
                "$OPA_STREAM_URL/118267-006-A/SHOW/de" to arteOpaStreamsJson(),
            ),
        )
        val service = testArteService(httpClient)

        val page = service.search(OnlineSearchRequest(OnlineSearchProvider.ARTE, "tatort"))

        assertEquals("https://arte-cdn.example/118267-006-A_EQ.mp4", page.results.single().normalQualityUrl)
    }

    @Test
    fun `search falls back to HLS stream when OPA has no usable mp4 stream`() = runBlocking {
        val httpClient = FakeOnlineSearchHttpClient(
            mapOf(
                "$SEARCH_CONTENT_URL?page=1&query=tatort" to arteSearchJson(),
                "$PLAYER_CONFIG_URL/118267-006-A" to arteConfigJson(),
                "$OPA_STREAM_URL/118267-006-A/SHOW/de" to arteOpaStreamsJson(includeDefaultAudio = false),
            ),
        )
        val service = testArteService(httpClient)

        val page = service.search(OnlineSearchRequest(OnlineSearchProvider.ARTE, "tatort"))

        val result = page.results.single()
        assertEquals("https://manifest-arte.akamaized.net/118267-006-A.m3u8", result.normalQualityUrl)
        assertEquals("https://manifest-arte.akamaized.net/118267-006-A.m3u8", result.highQualityUrl)
        assertEquals("https://manifest-arte.akamaized.net/118267-006-A.m3u8", result.lowQualityUrl)
    }

    @Test
    fun `search builds config url from id when player config is absent`() = runBlocking {
        val httpClient = FakeOnlineSearchHttpClient(
            mapOf(
                "$SEARCH_CONTENT_URL?page=1&query=tatort" to arteSearchJson(playerConfig = null),
                "$PLAYER_CONFIG_URL/118267-006-A" to arteConfigJson(),
            ),
        )
        val service = testArteService(httpClient)

        val page = service.search(OnlineSearchRequest(OnlineSearchProvider.ARTE, "tatort"))

        assertEquals(1, page.results.size)
        assertTrue(httpClient.requestedUrls.contains("$PLAYER_CONFIG_URL/118267-006-A"))
    }

    @Test
    fun `search accepts collection-like ARTE ids when config returns streams`() = runBlocking {
        val httpClient = FakeOnlineSearchHttpClient(
            mapOf(
                "$SEARCH_CONTENT_URL?page=1&query=klima" to arteSearchJson(
                    id = "RC-020428",
                    title = "Flaechenbraende: Die Klima-Katastrophe",
                    playerConfig = "$PLAYER_CONFIG_URL/RC-020428",
                ),
                "$PLAYER_CONFIG_URL/RC-020428" to arteConfigJson(
                    id = "RC-020428",
                    title = "Marseille lernt, mit dem Feuer zu leben",
                ),
            ),
        )
        val service = testArteService(httpClient)

        val page = service.search(OnlineSearchRequest(OnlineSearchProvider.ARTE, "klima"))

        assertEquals(1, page.results.size)
        assertEquals("Marseille lernt, mit dem Feuer zu leben", page.results.single().title)
    }

    @Test
    fun `search expands ARTE collection result to playable episodes`() = runBlocking {
        val httpClient = FakeOnlineSearchHttpClient(
            mapOf(
                "$SEARCH_CONTENT_URL?page=1&query=mittsommer" to arteSearchJson(
                    id = "RC-022391",
                    title = "Mord im Mittsommer",
                    url = "https://www.arte.tv/de/videos/RC-022391/mord-im-mittsommer/",
                    playerConfig = "$PLAYER_CONFIG_URL/RC-022391",
                    isCollection = true,
                    totalCount = 2,
                ),
                "https://www.arte.tv/de/videos/RC-022391/mord-im-mittsommer/" to arteCollectionPageHtml(),
                "$COLLECTION_SUBCOLLECTION_CONTENT_URL?collectionId=RC-022391&page=1&pageId=collection&subCollectionId=RC-022392&type=collection" to
                    arteCollectionContentJson(
                        "050709-001-A" to "Mord im Mittsommer - Staffel 1 (1/3)",
                        "050709-002-A" to "Mord im Mittsommer - Staffel 1 (2/3)",
                    ),
                "$COLLECTION_SUBCOLLECTION_CONTENT_URL?collectionId=RC-022391&page=1&pageId=collection&subCollectionId=RC-022397&type=collection" to
                    arteCollectionContentJson(
                        "050710-001-A" to "Mord im Mittsommer - Staffel 2 (1/3)",
                    ),
                "$PLAYER_CONFIG_URL/050709-001-A" to arteConfigJson(
                    id = "050709-001-A",
                    title = "Mord im Mittsommer - Staffel 1 (1/3)",
                ),
                "$PLAYER_CONFIG_URL/050709-002-A" to arteConfigJson(
                    id = "050709-002-A",
                    title = "Mord im Mittsommer - Staffel 1 (2/3)",
                    streamUrl = "https://manifest-arte.akamaized.net/050709-002-A.m3u8",
                ),
                "$PLAYER_CONFIG_URL/050710-001-A" to arteConfigJson(
                    id = "050710-001-A",
                    title = "Mord im Mittsommer - Staffel 2 (1/3)",
                    streamUrl = "https://manifest-arte.akamaized.net/050710-001-A.m3u8",
                ),
            ),
        )
        val service = testArteService(httpClient)

        val page = service.search(OnlineSearchRequest(OnlineSearchProvider.ARTE, "mittsommer"))

        assertEquals(
            listOf(
                "Mord im Mittsommer - Staffel 1 (1/3)",
                "Mord im Mittsommer - Staffel 1 (2/3)",
                "Mord im Mittsommer - Staffel 2 (1/3)",
            ),
            page.results.map { it.title },
        )
        assertEquals(listOf("Mord im Mittsommer"), page.results.map { it.topic }.distinct())
        assertTrue(httpClient.requestedUrls.none { it == "$PLAYER_CONFIG_URL/RC-022391" })
    }

    @Test
    fun `search retries rate limited ARTE request and stores learned delay`() = runBlocking {
        val searchUrl = "$SEARCH_CONTENT_URL?page=1&query=tatort"
        val httpClient = RateLimitedOnlineSearchHttpClient(
            responses = mapOf(
                searchUrl to arteSearchJson(),
                "$PLAYER_CONFIG_URL/118267-006-A" to arteConfigJson(),
            ),
            rateLimitedUrl = searchUrl,
        )
        val rateLimitState = TestArteRateLimitState()
        val service = ArteOnlineSearchService(httpClient, rateLimitState)

        val page = service.search(OnlineSearchRequest(OnlineSearchProvider.ARTE, "tatort"))

        assertEquals(1, page.results.size)
        assertEquals(2, httpClient.requestedUrls.count { it == searchUrl })
        assertEquals(250, rateLimitState.requestDelayMillis)
    }

    private companion object {
        private const val SEARCH_CONTENT_URL =
            "https://api.arte.tv/api/emac/v4/de/web/zones/f75e7b54-a37b-4f8f-b01b-dbefa877c041/content"
        private const val PLAYER_CONFIG_URL = "https://api.arte.tv/api/player/v2/config/de"
        private const val OPA_STREAM_URL = "https://www.arte.tv/hbbtvv2/services/web/index.php/OPA/v3/streams"
        private const val COLLECTION_SUBCOLLECTION_CONTENT_URL =
            "https://api-cdn.arte.tv/api/emac/v4/de/web/zones/8fbf35e4-324d-4d3e-a194-28f7d288789b/content"
        private const val ARTE_STREAM_API_TOKEN =
            "Bearer Nzc1Yjc1ZjJkYjk1NWFhN2I2MWEwMmRlMzAzNjI5NmU3NWU3ODg4ODJjOWMxNTMxYzEzZGRjYjg2ZGE4MmIwOA"
    }
}

private class TestArteRateLimitState(
    override var requestDelayMillis: Long = 0,
) : ArteOnlineSearchRateLimitState

private fun testArteService(httpClient: OnlineSearchHttpClient): ArteOnlineSearchService =
    ArteOnlineSearchService(httpClient, TestArteRateLimitState())

private class RateLimitedOnlineSearchHttpClient(
    private val responses: Map<String, String>,
    private val rateLimitedUrl: String,
) : OnlineSearchHttpClient {
    val requestedUrls = ArrayList<String>()
    private var rateLimitThrown = false

    override suspend fun get(url: String, headers: Map<String, String>): String {
        requestedUrls += url
        if (url == rateLimitedUrl && !rateLimitThrown) {
            rateLimitThrown = true
            throw OnlineSearchHttpException(
                statusCode = 429,
                message = "Too Many Requests",
                retryAfter = Duration.ofMillis(1),
            )
        }
        return responses[url] ?: error("No fake response registered for $url")
    }
}

private fun arteSearchJson(
    id: String = "118267-006-A",
    title: String = "Re: Tatort Kirche - Betroffene klagen an",
    url: String = "https://www.arte.tv/de/videos/$id/re-tatort-kirche-betroffene-klagen-an/",
    page: Int = 1,
    pages: Int = 1,
    totalCount: Long = 1,
    playerConfig: String? = "https://api.arte.tv/api/player/v2/config/de/118267-006-A",
    isCollection: Boolean = false,
): String {
    val player = playerConfig?.let { """, "player": { "config": "$it" }""" }.orEmpty()
    val kindCode = if (isCollection) "TV_SERIES" else "SHOW"
    val deeplinkKind = if (isCollection) "collection" else "program"
    return """
        {
          "data": [
            {
              "deeplink": "arte://$deeplinkKind/$id",
              "id": "$id",
              "kind": { "code": "$kindCode", "isCollection": $isCollection, "label": "Programm" },
              "title": "$title",
              "shortDescription": "Lange haben Betroffene geschwiegen.",
              "duration": 1815,
              "url": "$url",
              "genre": { "label": "Dokus und Reportagen" },
              "availability": { "hasVideoStreams": true, "start": "2025-02-25T04:00:00+00:00" }
              $player
            }
          ],
          "pagination": { "page": $page, "pages": $pages, "totalCount": $totalCount }
        }
    """.trimIndent()
}

private fun arteCollectionPageHtml(): String = """
    <script>
      self.__next_f.push([1, "content?collectionId=RC-022391\u0026page=1\u0026pageId=collection\u0026subCollectionId=RC-022392\u0026type=collection"]);
      self.__next_f.push([1, "content?collectionId=RC-022391\u0026page=1\u0026pageId=collection\u0026subCollectionId=RC-022397\u0026type=collection"]);
    </script>
""".trimIndent()

private fun arteCollectionContentJson(vararg episodes: Pair<String, String>): String {
    val data = episodes.joinToString(",") { (id, title) ->
        """
            {
              "deeplink": "arte://program/$id",
              "id": "${id}_de",
              "kind": { "code": "SHOW", "isCollection": false, "label": "Programm" },
              "title": "$title",
              "shortDescription": "$title description",
              "url": "https://www.arte.tv/de/videos/$id/mord-im-mittsommer/",
              "availability": { "hasVideoStreams": true },
              "player": { "config": "https://api.arte.tv/api/player/v2/config/de/$id" }
            }
        """.trimIndent()
    }
    return """
        {
          "data": [$data],
          "pagination": { "page": 1, "pages": 1, "totalCount": ${episodes.size} }
        }
    """.trimIndent()
}

private fun arteConfigJson(
    id: String = "118267-006-A",
    title: String = "Re: Tatort Kirche - Betroffene klagen an",
    streamUrl: String? = "https://manifest-arte.akamaized.net/118267-006-A.m3u8",
): String {
    val streams = if (streamUrl == null) {
        "[]"
    } else {
        """[{ "url": "$streamUrl", "versions": [{ "code": "VA", "audioDescription": false }], "mainQuality": { "code": "XQ" } }]"""
    }
    return """
        {
          "data": {
            "attributes": {
              "metadata": {
                "providerId": "$id",
                "title": "$title",
                "subtitle": null,
                "description": "Lange haben Betroffene von sexueller Gewalt geschwiegen.",
                "link": { "url": "https://www.arte.tv/de/videos/$id/re-tatort-kirche-betroffene-klagen-an/" },
                "duration": { "seconds": 1815 }
              },
              "rights": { "begin": "2025-02-25T04:00:00+00:00", "end": "2030-02-24T04:00:00+00:00" },
              "streams": $streams
            }
          }
        }
    """.trimIndent()
}

private fun arteOpaStreamsJson(
    id: String = "118267-006-A",
    includeDefaultAudio: Boolean = true,
): String {
    val defaultAudio = if (includeDefaultAudio) {
        """
            { "programId": "$id", "url": "http://arte-cdn.example/${id}_SQ.mp4", "quality": "SQ", "audioCode": "VA", "width": "1920", "height": "1080" },
            { "programId": "$id", "url": "https://arte-cdn.example/${id}_EQ.mp4", "quality": "EQ", "audioCode": "VA", "width": "1280", "height": "720" },
            { "programId": "$id", "url": "https://arte-cdn.example/${id}_HQ.mp4", "quality": "HQ", "audioCode": "VA", "width": "768", "height": "432" },
        """.trimIndent()
    } else {
        ""
    }
    return """
        {
          "videoStreams": [
            $defaultAudio
            { "programId": "$id", "url": "https://arte-cdn.example/${id}_VO.mp4", "quality": "EQ", "audioCode": "VO", "width": "1280", "height": "720" }
          ]
        }
    """.trimIndent()
}
