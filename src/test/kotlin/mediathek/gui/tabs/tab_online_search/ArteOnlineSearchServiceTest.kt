package mediathek.gui.tabs.tab_online_search

import kotlinx.coroutines.runBlocking
import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Assertions.assertTrue
import org.junit.jupiter.api.Test
import java.time.Duration
import java.time.LocalDateTime

class ArteOnlineSearchServiceTest {
    @Test
    fun `search loads ARTE results with HLS for all qualities`() = runBlocking {
        val httpClient = FakeOnlineSearchHttpClient(
            mapOf(
                "$SEARCH_CONTENT_URL?page=1&query=tatort" to arteSearchJson(),
                "$PLAYER_CONFIG_URL/118267-006-A" to arteConfigJson(),
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
        assertEquals("Re: Tatort Kirche - Betroffene klagen an (Deutsch)", result.title)
        assertEquals("https://www.arte.tv/de/videos/118267-006-A/re-tatort-kirche-betroffene-klagen-an/", result.websiteUrl)
        assertEquals("https://manifest-arte.akamaized.net/118267-006-A.m3u8", result.normalQualityUrl)
        assertEquals("https://manifest-arte.akamaized.net/118267-006-A.m3u8", result.highQualityUrl)
        assertEquals("https://manifest-arte.akamaized.net/118267-006-A.m3u8", result.lowQualityUrl)
        assertEquals(Duration.ofSeconds(1815), result.duration)
        assertEquals(LocalDateTime.of(2025, 2, 25, 5, 0), result.broadcastTime)
        assertEquals(
            listOf("$SEARCH_CONTENT_URL?page=1&query=tatort", "$PLAYER_CONFIG_URL/118267-006-A"),
            httpClient.requestedUrls,
        )
        assertTrue(httpClient.requestedHeaders.all { it.isEmpty() })
    }

    @Test
    fun `search returns one labelled result per audio language with selectable HLS versions`() = runBlocking {
        val masterUrl = "https://manifest-arte.akamaized.net/api/manifest/v1/Generate/test-id/de/XQ+KS+CHEV1/118267-006-A.m3u8"
        val httpClient = FakeOnlineSearchHttpClient(
            mapOf(
                "$SEARCH_CONTENT_URL?page=1&query=tatort" to arteSearchJson(),
                "$PLAYER_CONFIG_URL/118267-006-A" to arteConfigJson(streamsJson = """
                    [{ "url": "$masterUrl", "versions": [
                      { "code": "VF", "audioLanguage": "fr" },
                      { "code": "VA", "audioLanguage": "de" },
                      { "code": "VA-STMA", "audioLanguage": "deu" },
                      { "code": "VO-STA", "audioLanguage": "eng" },
                      { "code": "VAUD", "audioLanguage": "de", "audioDescription": true }
                    ] }]
                """.trimIndent()),
            ),
        )

        val page = testArteService(httpClient).search(OnlineSearchRequest(OnlineSearchProvider.ARTE, "tatort"))

        assertEquals(
            listOf("Deutsch", "Französisch", "Englisch").map { "Re: Tatort Kirche - Betroffene klagen an ($it)" },
            page.results.map { it.title },
        )
        assertEquals(
            listOf("VA", "VF", "VO-STA").map { masterUrl.replace("/de/", "/$it/") },
            page.results.map { it.normalQualityUrl },
        )
        assertEquals(1, page.results.map { it.websiteUrl }.distinct().size)
        assertTrue(page.results.all { it.normalQualityUrl == it.lowQualityUrl && it.normalQualityUrl == it.highQualityUrl })
        assertTrue(page.results.none { it.isAudioDescription })
        assertEquals(2, httpClient.requestedUrls.size)
    }

    @Test
    fun `search preserves separate legacy language streams and removes duplicate languages`() = runBlocking {
        val httpClient = FakeOnlineSearchHttpClient(
            mapOf(
                "$SEARCH_CONTENT_URL?page=1&query=tatort" to arteSearchJson(),
                "$PLAYER_CONFIG_URL/118267-006-A" to arteConfigJson(streamsJson = """
                    [
                      { "url": "https://cdn.example/fr.m3u8", "versions": [{ "audioLanguage": "fra", "code": "VF" }] },
                      { "url": "https://cdn.example/de.m3u8", "versions": [{ "audioLanguage": "deu", "code": "VA" }] },
                      { "url": "https://cdn.example/de-sub.m3u8", "versions": [{ "audioLanguage": "de", "code": "VA-STMA" }] },
                      { "url": "", "versions": [{ "audioLanguage": "en" }] }
                    ]
                """.trimIndent()),
            ),
        )

        val results = testArteService(httpClient).search(OnlineSearchRequest(OnlineSearchProvider.ARTE, "tatort")).results

        assertEquals(listOf("https://cdn.example/de.m3u8", "https://cdn.example/fr.m3u8"), results.map { it.normalQualityUrl })
        assertTrue(results[0].title.endsWith("(Deutsch)"))
        assertTrue(results[1].title.endsWith("(Französisch)"))
    }

    @Test
    fun `search keeps stream without language metadata playable without inventing a label`() = runBlocking {
        val httpClient = FakeOnlineSearchHttpClient(
            mapOf(
                "$SEARCH_CONTENT_URL?page=1&query=tatort" to arteSearchJson(),
                "$PLAYER_CONFIG_URL/118267-006-A" to arteConfigJson(streamsJson = """
                    [{ "url": "https://cdn.example/unknown.m3u8" }]
                """.trimIndent()),
            ),
        )

        val result = testArteService(httpClient).search(OnlineSearchRequest(OnlineSearchProvider.ARTE, "tatort")).results.single()

        assertEquals("Re: Tatort Kirche - Betroffene klagen an", result.title)
        assertEquals("https://cdn.example/unknown.m3u8", result.normalQualityUrl)
    }

    @Test
    fun `url lookup prefers German audio when French is listed first`() = runBlocking {
        val masterUrl = "https://manifest-arte.akamaized.net/api/manifest/v1/Generate/test-id/de/XQ/118267-006-A.m3u8"
        val httpClient = FakeOnlineSearchHttpClient(
            mapOf(
                "$PLAYER_CONFIG_URL/118267-006-A" to arteConfigJson(streamsJson = """
                    [{ "url": "$masterUrl", "versions": [
                      { "code": "VF", "audioLanguage": "fr" },
                      { "code": "VA", "audioLanguage": "de" }
                    ] }]
                """.trimIndent()),
            ),
        )

        val result = testArteService(httpClient).loadByUrl(
            OnlineUrlRequest(OnlineSearchProvider.ARTE, "https://www.arte.tv/de/videos/118267-006-A/example/"),
        )

        assertEquals("Re: Tatort Kirche - Betroffene klagen an (Deutsch)", result?.title)
        assertEquals(masterUrl.replace("/de/", "/VA/"), result?.normalQualityUrl)
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
            ),
        )
        val service = testArteService(httpClient)

        val result = service.loadByUrl(
            OnlineUrlRequest(
                OnlineSearchProvider.ARTE,
                "https://www.arte.tv/de/videos/118267-006-A/re-tatort-kirche-betroffene-klagen-an/",
            ),
        )

        assertEquals("Re: Tatort Kirche - Betroffene klagen an (Deutsch)", result?.title)
        assertEquals("https://manifest-arte.akamaized.net/118267-006-A.m3u8", result?.normalQualityUrl)
        assertEquals(
            listOf("$PLAYER_CONFIG_URL/118267-006-A"),
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
        assertEquals(
            listOf("$SEARCH_CONTENT_URL?page=1&query=tatort", "$PLAYER_CONFIG_URL/118267-006-A"),
            httpClient.requestedUrls,
        )
    }

    @Test
    fun `search resolves German WebVTT subtitles from ARTE HLS manifests`() = runBlocking {
        val masterUrl = "https://manifest-arte.akamaized.net/118267-006-A.m3u8"
        val subtitlePlaylistUrl = "https://manifest-arte.akamaized.net/subtitles/de.m3u8"
        val httpClient = FakeOnlineSearchHttpClient(
            mapOf(
                "$SEARCH_CONTENT_URL?page=1&query=tatort" to arteSearchJson(),
                "$PLAYER_CONFIG_URL/118267-006-A" to arteConfigJson(hasGermanSubtitles = true),
                masterUrl to arteMasterPlaylist(subtitlePlaylistUrl),
                subtitlePlaylistUrl to arteSubtitlePlaylist("de/118267-006-A.vtt"),
            ),
        )
        val service = testArteService(httpClient)

        val result = service.search(OnlineSearchRequest(OnlineSearchProvider.ARTE, "tatort")).results.single()

        assertEquals("https://manifest-arte.akamaized.net/subtitles/de/118267-006-A.vtt", result.subtitleUrl)
    }

    @Test
    fun `search attaches subtitles matching each audio language`() = runBlocking {
        val masterUrl = "https://manifest-arte.akamaized.net/api/manifest/v1/Generate/test-id/de/XQ/118267-006-A.m3u8"
        val httpClient = FakeOnlineSearchHttpClient(
            mapOf(
                "$SEARCH_CONTENT_URL?page=1&query=tatort" to arteSearchJson(),
                "$PLAYER_CONFIG_URL/118267-006-A" to arteConfigJson(streamsJson = """
                    [{ "url": "$masterUrl", "versions": [
                      { "code": "VA-STMA", "audioLanguage": "de", "subtitleLanguage": "de", "closedCaptioning": true },
                      { "code": "VF-STMF", "audioLanguage": "fr", "subtitleLanguage": "fr", "closedCaptioning": true }
                    ] }]
                """.trimIndent()),
                masterUrl to """
                    #EXTM3U
                    #EXT-X-MEDIA:TYPE=SUBTITLES,GROUP-ID="subs",LANGUAGE="de",URI="https://cdn.example/de.vtt"
                    #EXT-X-MEDIA:TYPE=SUBTITLES,GROUP-ID="subs",LANGUAGE="fr",URI="https://cdn.example/fr.vtt"
                """.trimIndent(),
            ),
        )

        val results = testArteService(httpClient).search(OnlineSearchRequest(OnlineSearchProvider.ARTE, "tatort")).results

        assertEquals(listOf("https://cdn.example/de.vtt", "https://cdn.example/fr.vtt"), results.map { it.subtitleUrl })
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
        assertEquals("Marseille lernt, mit dem Feuer zu leben (Deutsch)", page.results.single().title)
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
                    streamsJson = """
                        [
                          { "url": "https://cdn.example/episode1-de.m3u8", "versions": [{ "audioLanguage": "de" }] },
                          { "url": "https://cdn.example/episode1-fr.m3u8", "versions": [{ "audioLanguage": "fr" }] }
                        ]
                    """.trimIndent(),
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
                "Mord im Mittsommer - Staffel 1 (1/3) (Deutsch)",
                "Mord im Mittsommer - Staffel 1 (1/3) (Französisch)",
                "Mord im Mittsommer - Staffel 1 (2/3) (Deutsch)",
                "Mord im Mittsommer - Staffel 2 (1/3) (Deutsch)",
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
        private const val COLLECTION_SUBCOLLECTION_CONTENT_URL =
            "https://api-cdn.arte.tv/api/emac/v4/de/web/zones/8fbf35e4-324d-4d3e-a194-28f7d288789b/content"
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
    hasGermanSubtitles: Boolean = false,
    streamsJson: String? = null,
): String {
    val streams = streamsJson ?: if (streamUrl == null) {
        "[]"
    } else {
        val subtitleVersion = if (hasGermanSubtitles) {
            """, { "code": "VA-STMA", "subtitleLanguage": "de", "closedCaptioning": true }"""
        } else {
            ""
        }
        """[{ "url": "$streamUrl", "versions": [{ "code": "VA", "audioLanguage": "de", "audioDescription": false }$subtitleVersion], "mainQuality": { "code": "XQ" } }]"""
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

private fun arteMasterPlaylist(subtitlePlaylistUrl: String): String = """
    #EXTM3U
    #EXT-X-MEDIA:TYPE=SUBTITLES,GROUP-ID="subtitle_0",LANGUAGE="de",NAME="Deutsch",URI="$subtitlePlaylistUrl"
""".trimIndent()

private fun arteSubtitlePlaylist(subtitleFile: String): String = """
    #EXTM3U
    #EXT-X-PLAYLIST-TYPE:VOD
    #EXTINF:1815,
    $subtitleFile
    #EXT-X-ENDLIST
""".trimIndent()
