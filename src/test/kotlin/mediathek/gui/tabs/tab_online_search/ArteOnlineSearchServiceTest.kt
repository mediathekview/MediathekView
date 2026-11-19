package mediathek.gui.tabs.tab_online_search

import kotlinx.coroutines.*
import kotlinx.coroutines.channels.Channel
import okhttp3.HttpUrl.Companion.toHttpUrl
import okhttp3.OkHttpClient
import okhttp3.Protocol
import okhttp3.Response
import okhttp3.ResponseBody.Companion.toResponseBody
import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Assertions.assertTrue
import org.junit.jupiter.api.Test
import org.junit.jupiter.api.assertThrows
import org.junit.jupiter.params.ParameterizedTest
import org.junit.jupiter.params.provider.ValueSource
import java.time.Duration
import java.time.LocalDateTime
import java.util.concurrent.CopyOnWriteArrayList
import java.util.concurrent.atomic.AtomicInteger
import kotlin.time.Duration.Companion.milliseconds
import kotlin.time.Duration.Companion.minutes
import kotlin.time.Duration.Companion.seconds
import kotlin.time.TestTimeSource

class ArteOnlineSearchServiceTest {
    @Test
    fun `search loads ARTE results with HLS for all qualities`() = runBlocking {
        val httpClient = arteHttpClient(
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
            listOf("$SEARCH_CONTENT_URL?page=1&query=tatort", "$PLAYER_CONFIG_URL/118267-006-A") +
                otherCatalogueUrls("tatort"),
            httpClient.requestedUrls,
        )
        assertTrue(httpClient.requestedHeaders.all { it.isEmpty() })
    }

    @Test
    fun `search returns one labelled result per audio language with selectable HLS versions`() = runBlocking {
        val masterUrl = "https://manifest-arte.akamaized.net/api/manifest/v1/Generate/test-id/de/XQ+KS+CHEV1/118267-006-A.m3u8"
        val httpClient = arteHttpClient(
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
        assertEquals(8, httpClient.requestedUrls.size)
    }

    @Test
    fun `search preserves separate legacy language streams and removes duplicate languages`() = runBlocking {
        val httpClient = arteHttpClient(
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
        val httpClient = arteHttpClient(
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
        val httpClient = arteHttpClient(
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
    fun `search merges all seven catalogues by programme and preserves additional audio languages`() = runBlocking {
        val ids = mapOf(
            "de" to "118267-006-A", "fr" to "118267-006-A", "en" to "118267-006-A",
            "es" to "118267-007-A", "pl" to "118267-008-A", "it" to "118267-009-A", "ro" to "118267-010-A",
        )
        val responses = buildMap {
            for ((language, id) in ids) {
                put(
                    "${SEARCH_CONTENT_URL.replace("/de/", "/$language/")}?page=1&query=union",
                    arteSearchJson(id = "${id}_$language", language = language, playerConfig = null),
                )
                val audioLanguages = when (language) {
                    "fr" -> listOf("deu", "pol")
                    "en" -> listOf("spa")
                    "ro" -> listOf("ron")
                    else -> listOf(language)
                }
                val streams = audioLanguages.joinToString(",") { audio ->
                    """{ "url": "https://cdn.example/$language/$audio/$id.m3u8", "versions": [{ "audioLanguage": "$audio" }] }"""
                }
                put(
                    "${PLAYER_CONFIG_URL.replace("/de", "/$language")}/$id",
                    arteConfigJson(id = id, title = "$language programme", streamsJson = "[$streams]")
                        .replace("https://www.arte.tv/de/", "https://www.arte.tv/$language/"),
                )
            }
        }
        val httpClient = FakeOnlineSearchHttpClient(responses)

        val page = testArteService(httpClient).search(OnlineSearchRequest(OnlineSearchProvider.ARTE, "union"))

        assertEquals(
            listOf(
                "de programme (Deutsch)", "fr programme (Polnisch)", "en programme (Spanisch)",
                "es programme (Spanisch)", "pl programme (Polnisch)", "it programme (Italienisch)", "ro programme (Rumänisch)",
            ),
            page.results.map { it.title },
        )
        assertEquals(7, page.totalResults)
        assertEquals(null, page.nextToken)
        assertEquals(7, httpClient.requestedUrls.count { "/content?page=" in it })
        assertEquals(7, httpClient.requestedUrls.count { "/config/" in it })
        assertTrue(httpClient.requestedUrls.none { "_" in it.substringAfterLast('/') })
        assertTrue(page.results.none { it.normalQualityUrl == "https://cdn.example/fr/deu/118267-006-A.m3u8" })
    }

    @Test
    fun `search expands Polish collections using the Polish catalogue and merges episode overlaps`() = runBlocking {
        val collectionUrl = "https://www.arte.tv/pl/videos/RC-022391/example/"
        val plConfigUrl = "${PLAYER_CONFIG_URL.replace("/de", "/pl")}/118267-006-A"
        val httpClient = arteHttpClient(
            mapOf(
                "$SEARCH_CONTENT_URL?page=1&query=collection" to arteSearchJson(),
                "${SEARCH_CONTENT_URL.replace("/de/", "/pl/")}?page=1&query=collection" to arteSearchJson(
                    id = "RC-022391_pl", language = "pl", url = collectionUrl, isCollection = true,
                ),
                "$PLAYER_CONFIG_URL/118267-006-A" to arteConfigJson(),
                collectionUrl to """<script>subCollectionId=RC-022392</script>""",
                "${COLLECTION_SUBCOLLECTION_CONTENT_URL.replace("/de/", "/pl/")}?collectionId=RC-022391&page=1&pageId=collection&subCollectionId=RC-022392&type=collection" to
                    arteCollectionContentJson("118267-006-A" to "Polish episode").replace("/de/", "/pl/"),
                plConfigUrl to arteConfigJson(
                    title = "Polish episode",
                    streamsJson = """[
                        { "url": "https://cdn.example/de.m3u8", "versions": [{ "audioLanguage": "de" }] },
                        { "url": "https://cdn.example/pl.m3u8", "versions": [{ "audioLanguage": "pl" }] }
                    ]""".trimIndent(),
                ),
            ),
        )

        val page = testArteService(httpClient).search(OnlineSearchRequest(OnlineSearchProvider.ARTE, "collection"))

        assertEquals(
            listOf("Re: Tatort Kirche - Betroffene klagen an (Deutsch)", "Polish episode (Polnisch)"),
            page.results.map { it.title },
        )
        assertEquals(2, page.totalResults)
        assertEquals(1, httpClient.requestedUrls.count { it == plConfigUrl })
        assertTrue(httpClient.requestedUrls.none { "/config/pl/RC-022391" in it })
    }

    @Test
    fun `search returns zero results when every catalogue is empty`() = runBlocking {
        val httpClient = arteHttpClient(
            mapOf("$SEARCH_CONTENT_URL?page=1&query=empty" to """{ "data": [], "pagination": null }"""),
        )

        val page = testArteService(httpClient).search(OnlineSearchRequest(OnlineSearchProvider.ARTE, "empty"))

        assertTrue(page.results.isEmpty())
        assertEquals(null, page.nextToken)
        assertEquals(0, page.totalResults)
        assertEquals(7, httpClient.requestedUrls.size)
    }

    @Test
    fun `search follows independent catalogue pages and deduplicates across pages`() = runBlocking {
        val deUrl = "$SEARCH_CONTENT_URL?page=1&query=klima"
        val plUrl = deUrl.replace("/de/", "/pl/")
        val httpClient = arteHttpClient(
            mapOf(
                deUrl to arteSearchJson(pages = 2, totalCount = 42),
                plUrl to arteSearchJson(pages = 3, totalCount = 50),
                deUrl.replace("page=1", "page=2") to arteSearchJson(page = 2, pages = 2),
                plUrl.replace("page=1", "page=2") to arteSearchJson(page = 2, pages = 3),
                plUrl.replace("page=1", "page=3") to arteSearchJson(
                    page = 3, pages = 3, id = "118267-006-A_pl", playerConfig = null,
                ),
                "$PLAYER_CONFIG_URL/118267-006-A" to arteConfigJson(),
                "${PLAYER_CONFIG_URL.replace("/de", "/pl")}/118267-006-A" to arteConfigJson(
                    title = "Polish-only result",
                    streamsJson = """[{ "url": "https://cdn.example/pl.m3u8", "versions": [{ "audioLanguage": "pl" }] }]""",
                ),
            ),
        )
        val service = testArteService(httpClient)

        val first = service.search(OnlineSearchRequest(OnlineSearchProvider.ARTE, "klima"))
        val second = service.search(OnlineSearchRequest(OnlineSearchProvider.ARTE, "klima", first.nextToken))
        val third = service.search(OnlineSearchRequest(OnlineSearchProvider.ARTE, "klima", second.nextToken))

        assertEquals(1, first.results.size)
        assertEquals(null, first.totalResults)
        assertTrue(first.hasNextPage)
        assertTrue(second.results.isEmpty())
        assertTrue(second.hasNextPage)
        assertEquals(null, second.totalResults)
        assertEquals("Polish-only result (Polnisch)", third.results.single().title)
        assertEquals(null, third.nextToken)
        assertEquals(2, third.totalResults)
        assertEquals(2, httpClient.requestedUrls.count { it == "$PLAYER_CONFIG_URL/118267-006-A" })
        assertEquals(1, httpClient.requestedUrls.count { it == deUrl })
        assertEquals(1, httpClient.requestedUrls.count { it == plUrl })
        assertTrue(httpClient.requestedUrls.none { it == deUrl.replace("page=1", "page=3") })
        assertEquals(1, httpClient.requestedUrls.count { it == plUrl.replace("page=1", "page=3") })
        assertTrue(otherCatalogueUrls("klima").all { url -> httpClient.requestedUrls.count { it == url } == 1 })
    }

    @Test
    fun `url lookup extracts ARTE program id and loads config`() = runBlocking {
        val httpClient = arteHttpClient(
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
        val httpClient = arteHttpClient(
            mapOf(
                "$SEARCH_CONTENT_URL?page=1&query=tatort" to arteSearchJson(),
                "$PLAYER_CONFIG_URL/118267-006-A" to arteConfigJson(streamUrl = null),
            ),
        )
        val service = testArteService(httpClient)

        val page = service.search(OnlineSearchRequest(OnlineSearchProvider.ARTE, "tatort"))

        assertEquals(emptyList<OnlineSearchResult>(), page.results)
        assertEquals(
            listOf("$SEARCH_CONTENT_URL?page=1&query=tatort", "$PLAYER_CONFIG_URL/118267-006-A") +
                otherCatalogueUrls("tatort"),
            httpClient.requestedUrls,
        )
    }

    @Test
    fun `search resolves German WebVTT subtitles from ARTE HLS manifests`() = runBlocking {
        val masterUrl = "https://manifest-arte.akamaized.net/118267-006-A.m3u8"
        val subtitlePlaylistUrl = "https://manifest-arte.akamaized.net/subtitles/de.m3u8"
        val httpClient = arteHttpClient(
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
        val httpClient = arteHttpClient(
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
        val httpClient = arteHttpClient(
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
        val httpClient = arteHttpClient(
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
        val httpClient = arteHttpClient(
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
    fun `search paces requests and reduces the learned delay after five consecutive successes`() = runBlocking {
        val searchUrl = "$SEARCH_CONTENT_URL?page=1&query=tatort"
        val httpClient = RateLimitedOnlineSearchHttpClient(
            responses = mapOf(
                searchUrl to arteSearchJson(),
                "$PLAYER_CONFIG_URL/118267-006-A" to arteConfigJson(),
            ),
            rateLimitedUrl = searchUrl,
            retryAfterValues = listOf(Duration.ofMillis(1)),
        )
        val rateLimitState = TestArteRateLimitState()
        val clock = TestTimeSource()
        val waits = mutableListOf<Long>()
        val service = ArteOnlineSearchService(httpClient, rateLimitState, clock) {
            waits += it.inWholeMilliseconds
            clock += it
        }

        val page = service.search(OnlineSearchRequest(OnlineSearchProvider.ARTE, "tatort"))

        assertEquals(1, page.results.size)
        assertEquals(2, httpClient.requestedUrls.count { it == searchUrl })
        assertEquals(List(5) { 500L } + List(3) { 375L }, waits)
        assertEquals(375L, rateLimitState.requestDelayMillis)
    }

    @ParameterizedTest
    @ValueSource(booleans = [false, true])
    fun `sustained traffic stops testing a delay that caused a 429`(withRetryAfter: Boolean) = runBlocking {
        val clock = TestTimeSource()
        val retryAfter = Duration.ofSeconds(1).takeIf { withRetryAfter }
        val rejectedLookups = mutableListOf<Int>()
        var minimumSpacing = 1.seconds
        var lookup = 0
        val client = object : OnlineSearchHttpClient {
            private var lastAcceptedRequest = clock.markNow() - 1.seconds

            override suspend fun get(url: String, headers: Map<String, String>): String {
                if (lastAcceptedRequest.elapsedNow() < minimumSpacing) {
                    rejectedLookups += lookup
                    throw OnlineSearchHttpException(429, "Too Many Requests", url, retryAfter)
                }
                lastAcceptedRequest = clock.markNow()
                return arteConfigJson()
            }
        }
        val rateLimitState = TestArteRateLimitState(requestDelayMillis = 1_000L)
        val service = ArteOnlineSearchService(client, rateLimitState, clock) { clock += it }
        val request = OnlineUrlRequest(OnlineSearchProvider.ARTE, "$PLAYER_CONFIG_URL/118267-006-A")

        for (index in 1..150) {
            lookup = index
            assertTrue(service.loadByUrl(request) != null)
            if (index >= 6) {
                assertTrue(rateLimitState.requestDelayMillis >= 1_000L, "Delay fell below the floor at lookup $index")
            }
        }

        assertEquals(listOf(6), rejectedLookups)
        assertEquals(1_000L, rateLimitState.requestDelayMillis)

        clock += 1.minutes
        minimumSpacing = 500.milliseconds
        for (index in 151..156) {
            lookup = index
            assertTrue(service.loadByUrl(request) != null)
        }
        assertEquals(750L, rateLimitState.requestDelayMillis)
        assertEquals(listOf(6), rejectedLookups)
    }

    @Test
    fun `a 429 resets the success streak and backs off even with a short Retry-After`() = runBlocking {
        val ordinaryUrl = "$PLAYER_CONFIG_URL/118267-006-A"
        val limitedUrl = "$PLAYER_CONFIG_URL/119947-000-A"
        val httpClient = RateLimitedOnlineSearchHttpClient(
            responses = mapOf(ordinaryUrl to "{}", limitedUrl to "{}"),
            rateLimitedUrl = limitedUrl,
            retryAfterValues = listOf(Duration.ofSeconds(1)),
        )
        val rateLimitState = TestArteRateLimitState(requestDelayMillis = 2_000L)
        val clock = TestTimeSource()
        val service = ArteOnlineSearchService(httpClient, rateLimitState, clock) { clock += it }
        val ordinaryRequest = OnlineUrlRequest(OnlineSearchProvider.ARTE, ordinaryUrl)

        repeat(4) { service.loadByUrl(ordinaryRequest) }
        service.loadByUrl(OnlineUrlRequest(OnlineSearchProvider.ARTE, limitedUrl))
        assertEquals(4_000L, rateLimitState.requestDelayMillis)

        repeat(3) { service.loadByUrl(ordinaryRequest) }
        assertEquals(4_000L, rateLimitState.requestDelayMillis)
        service.loadByUrl(ordinaryRequest)
        assertEquals(3_000L, rateLimitState.requestDelayMillis)
    }

    @Test
    fun `short Retry-After still learns the sustained two-second request interval`() = runBlocking {
        val clock = TestTimeSource()
        val rejectedLookups = mutableListOf<Int>()
        var lookup = 0
        val httpClient = object : OnlineSearchHttpClient {
            private var lastAcceptedRequest = clock.markNow() - 2.seconds

            override suspend fun get(url: String, headers: Map<String, String>): String {
                if (lastAcceptedRequest.elapsedNow() < 2.seconds) {
                    rejectedLookups += lookup
                    throw OnlineSearchHttpException(429, "Too Many Requests", url, Duration.ofSeconds(1))
                }
                lastAcceptedRequest = clock.markNow()
                return arteConfigJson()
            }
        }
        val rateLimitState = TestArteRateLimitState(requestDelayMillis = 1_000L)
        val service = ArteOnlineSearchService(httpClient, rateLimitState, clock) { clock += it }
        val request = OnlineUrlRequest(OnlineSearchProvider.ARTE, "$PLAYER_CONFIG_URL/118267-006-A")

        for (index in 1..150) {
            lookup = index
            assertTrue(service.loadByUrl(request) != null)
        }

        assertEquals(listOf(2, 7), rejectedLookups)
        assertEquals(2_000L, rateLimitState.requestDelayMillis)
    }

    @Test
    fun `player config retries honor a Retry-After above the learned delay cap`() = runBlocking {
        val configUrl = "$PLAYER_CONFIG_URL/118267-006-A"
        val httpClient = RateLimitedOnlineSearchHttpClient(
            responses = mapOf(configUrl to arteConfigJson()),
            rateLimitedUrl = configUrl,
            retryAfterValues = listOf(Duration.ofSeconds(30)),
        )
        val rateLimitState = TestArteRateLimitState()
        val clock = TestTimeSource()
        val startedAt = clock.markNow()
        val service = ArteOnlineSearchService(httpClient, rateLimitState, clock) { clock += it }
        val request = OnlineUrlRequest(OnlineSearchProvider.ARTE, configUrl)

        assertTrue(service.loadByUrl(request) != null)
        assertEquals(listOf(configUrl, configUrl), httpClient.requestedUrls)
        assertEquals(30.seconds, startedAt.elapsedNow())
        assertEquals(10_000L, rateLimitState.requestDelayMillis)

        clock += 4.seconds
        assertTrue(service.loadByUrl(request) != null)
        assertEquals(40.seconds, startedAt.elapsedNow())
        assertEquals(10_000L, rateLimitState.requestDelayMillis)
    }

    @Test
    fun `a final 429 keeps its cooldown for the next request`() = runBlocking {
        val configUrl = "$PLAYER_CONFIG_URL/118267-006-A"
        val httpClient = RateLimitedOnlineSearchHttpClient(
            responses = mapOf(configUrl to arteConfigJson()),
            rateLimitedUrl = configUrl,
            retryAfterValues = listOf(null, null, Duration.ofSeconds(90)),
        )
        val clock = TestTimeSource()
        val startedAt = clock.markNow()
        val service = ArteOnlineSearchService(httpClient, TestArteRateLimitState(), clock) { clock += it }
        val request = OnlineUrlRequest(OnlineSearchProvider.ARTE, configUrl)

        assertEquals(429, assertThrows<OnlineSearchHttpException> { service.loadByUrl(request) }.statusCode)
        assertEquals(3, httpClient.requestedUrls.size)
        assertEquals(1500.milliseconds, startedAt.elapsedNow())

        clock += 65.seconds
        assertTrue(service.loadByUrl(request) != null)
        assertEquals(91_500.milliseconds, startedAt.elapsedNow())
    }

    @Test
    fun `cancelling during Retry-After preserves the remaining cooldown for the next search`() = runBlocking {
        withTimeout(5.seconds) {
            val firstUrl = "$PLAYER_CONFIG_URL/119947-000-A"
            val nextUrl = "$PLAYER_CONFIG_URL/118267-006-A"
            val httpClient = RateLimitedOnlineSearchHttpClient(
                responses = mapOf(nextUrl to arteConfigJson()),
                rateLimitedUrl = firstUrl,
                retryAfterValues = listOf(Duration.ofSeconds(30)),
            )
            val clock = TestTimeSource()
            val waits = Channel<kotlin.time.Duration>(Channel.UNLIMITED)
            val resumeWait = CompletableDeferred<Unit>()
            val rateLimitState = TestArteRateLimitState()
            val service = ArteOnlineSearchService(httpClient, rateLimitState, clock) { wait ->
                waits.send(wait)
                resumeWait.await()
                clock += wait
            }

            val firstSearch = launch { service.loadByUrl(OnlineUrlRequest(OnlineSearchProvider.ARTE, firstUrl)) }
            assertEquals(30.seconds, waits.receive())
            clock += 12.seconds
            firstSearch.cancelAndJoin()

            val nextSearch = async { service.loadByUrl(OnlineUrlRequest(OnlineSearchProvider.ARTE, nextUrl)) }
            assertEquals(18.seconds, waits.receive())
            assertEquals(listOf(firstUrl), httpClient.requestedUrls)
            assertEquals(10_000L, rateLimitState.requestDelayMillis)
            resumeWait.complete(Unit)

            assertTrue(nextSearch.await() != null)
            assertEquals(listOf(firstUrl, nextUrl), httpClient.requestedUrls)
            assertEquals(10_000L, rateLimitState.requestDelayMillis)
        }
    }

    @Test
    fun `cancelling after the idle threshold preserves the learned floor and pending Retry-After`() = runBlocking {
        withTimeout(5.seconds) {
            val clock = TestTimeSource()
            val rateLimitState = TestArteRateLimitState(requestDelayMillis = 1_000L)
            var calls = 0
            var rejections = 0
            val httpClient = object : OnlineSearchHttpClient {
                private var lastAcceptedRequest = clock.markNow() - 1.seconds

                override suspend fun get(url: String, headers: Map<String, String>): String {
                    calls++
                    if (lastAcceptedRequest.elapsedNow() < 1.seconds) {
                        rejections++
                        throw OnlineSearchHttpException(429, "Too Many Requests", url, Duration.ofSeconds(90))
                    }
                    lastAcceptedRequest = clock.markNow()
                    return arteConfigJson()
                }
            }
            val cooldownWaits = Channel<kotlin.time.Duration>(Channel.UNLIMITED)
            val resumeWait = CompletableDeferred<Unit>()
            val service = ArteOnlineSearchService(httpClient, rateLimitState, clock) { wait ->
                if (wait > 10.seconds) {
                    cooldownWaits.send(wait)
                    resumeWait.await()
                }
                clock += wait
            }
            val firstRequest = OnlineUrlRequest(OnlineSearchProvider.ARTE, "$PLAYER_CONFIG_URL/119947-000-A")
            val nextRequest = OnlineUrlRequest(OnlineSearchProvider.ARTE, "$PLAYER_CONFIG_URL/118267-006-A")

            repeat(5) { service.loadByUrl(firstRequest) }
            assertEquals(750L, rateLimitState.requestDelayMillis)
            val firstSearch = launch { service.loadByUrl(firstRequest) }
            assertEquals(90.seconds, cooldownWaits.receive())
            clock += 65.seconds
            firstSearch.cancelAndJoin()

            val nextSearch = async { service.loadByUrl(nextRequest) }
            assertEquals(25.seconds, cooldownWaits.receive())
            assertEquals(6, calls)
            resumeWait.complete(Unit)
            assertTrue(nextSearch.await() != null)

            repeat(60) { assertTrue(service.loadByUrl(nextRequest) != null) }
            assertEquals(1_000L, rateLimitState.requestDelayMillis)
            assertEquals(1, rejections)
            assertEquals(67, calls)
        }
    }

    @Test
    fun `a cancelled subtitle cooldown stays on its CDN and leaves the ARTE API available`() = runBlocking {
        withTimeout(5.seconds) {
            val firstUrl = "$PLAYER_CONFIG_URL/118267-006-A"
            val nextUrl = "$PLAYER_CONFIG_URL/119947-000-A"
            val masterUrl = "https://manifest-arte.akamaized.net/118267-006-A.m3u8"
            val subtitleUrl = "https://manifest-arte.akamaized.net/subtitles/de.vtt"
            val clock = TestTimeSource()
            val apiRateLimitState = TestArteRateLimitState()
            val requestedUrls = mutableListOf<String>()
            var masterRequests = 0
            val httpClient = object : OnlineSearchHttpClient {
                override suspend fun get(url: String, headers: Map<String, String>): String {
                    requestedUrls += url
                    return when (url) {
                        firstUrl -> arteConfigJson(hasGermanSubtitles = true)
                        nextUrl -> arteConfigJson()
                        masterUrl -> {
                            if (++masterRequests == 1) {
                                throw OnlineSearchHttpException(429, "Too Many Requests", url, Duration.ofSeconds(90))
                            }
                            arteMasterPlaylist(subtitleUrl)
                        }
                        else -> error("Unexpected ARTE request: $url")
                    }
                }
            }
            val waits = Channel<kotlin.time.Duration>(Channel.UNLIMITED)
            val resumeWait = CompletableDeferred<Unit>()
            val service = ArteOnlineSearchService(httpClient, apiRateLimitState, clock) { wait ->
                waits.send(wait)
                resumeWait.await()
                clock += wait
            }
            val firstRequest = OnlineUrlRequest(OnlineSearchProvider.ARTE, firstUrl)
            val firstSearch = launch { service.loadByUrl(firstRequest) }
            assertEquals(90.seconds, waits.receive())
            clock += 65.seconds
            firstSearch.cancelAndJoin()

            val retry = async { service.loadByUrl(firstRequest) }
            assertEquals(25.seconds, waits.receive())
            assertEquals(listOf(firstUrl, masterUrl, firstUrl), requestedUrls)

            val cooldownStartedAt = clock.markNow()
            assertTrue(service.loadByUrl(OnlineUrlRequest(OnlineSearchProvider.ARTE, nextUrl)) != null)
            assertEquals(kotlin.time.Duration.ZERO, cooldownStartedAt.elapsedNow())
            assertEquals(listOf(firstUrl, masterUrl, firstUrl, nextUrl), requestedUrls)
            assertEquals(0L, apiRateLimitState.requestDelayMillis)

            resumeWait.complete(Unit)
            assertEquals(subtitleUrl, retry.await()?.subtitleUrl)
            assertEquals(25.seconds, cooldownStartedAt.elapsedNow())
            assertEquals(listOf(firstUrl, masterUrl, firstUrl, nextUrl, masterUrl), requestedUrls)
        }
    }

    @Test
    fun `a redirected CDN retains its cooldown and learned floor across cancellation`() = runBlocking {
        withTimeout(5.seconds) {
            val clock = TestTimeSource()
            val redirectingUrl = "$PLAYER_CONFIG_URL/119947-000-A"
            val healthyUrl = "$PLAYER_CONFIG_URL/118267-006-A"
            val redirectLocation = "//manifest-arte.akamaized.net/config/119947-000-A"
            val cdnUrl = "https:$redirectLocation"
            val cdnRequests = AtomicInteger()
            val cdnRejections = AtomicInteger()
            var lastAcceptedCdnRequest = clock.markNow() - 1.seconds
            val client = OkHttpClient.Builder().addInterceptor { chain ->
                val request = chain.request()
                val response = Response.Builder().request(request).protocol(Protocol.HTTP_1_1)
                when (request.url.toString()) {
                    redirectingUrl -> response.code(302).message("Found")
                        .header("Location", redirectLocation)
                        .body("".toResponseBody())
                    healthyUrl -> response.code(200).message("OK").body(arteConfigJson().toResponseBody())
                    cdnUrl -> {
                        cdnRequests.incrementAndGet()
                        if (lastAcceptedCdnRequest.elapsedNow() < 1.seconds) {
                            val retryAfter = if (cdnRejections.incrementAndGet() == 1) "1" else "90"
                            response.code(429).message("Too Many Requests").header("Retry-After", retryAfter)
                                .body("".toResponseBody())
                        } else {
                            lastAcceptedCdnRequest = clock.markNow()
                            response.code(200).message("OK").body(arteConfigJson().toResponseBody())
                        }
                    }
                    else -> error("Unexpected ARTE request: ${request.url}")
                }.build()
            }.build()
            val waits = Channel<kotlin.time.Duration>(Channel.UNLIMITED)
            val resumeWait = CompletableDeferred<Unit>()
            val apiRateLimitState = TestArteRateLimitState()
            val service = ArteOnlineSearchService(OkHttpOnlineSearchHttpClient(client), apiRateLimitState, clock) { wait ->
                if (wait > 10.seconds) {
                    waits.send(wait)
                    resumeWait.await()
                }
                clock += wait
            }
            val redirectingRequest = OnlineUrlRequest(OnlineSearchProvider.ARTE, redirectingUrl)
            val healthyRequest = OnlineUrlRequest(OnlineSearchProvider.ARTE, healthyUrl)
            repeat(6) { assertTrue(service.loadByUrl(redirectingRequest) != null) }
            assertEquals(1, cdnRejections.get())

            val first = launch { service.loadByUrl(redirectingRequest) }
            assertEquals(90.seconds, waits.receive())
            clock += 65.seconds
            first.cancelAndJoin()

            val next = async { service.loadByUrl(redirectingRequest) }
            assertEquals(25.seconds, waits.receive())
            assertEquals(8, cdnRequests.get())
            val startedAt = clock.markNow()
            assertTrue(service.loadByUrl(healthyRequest) != null)
            assertEquals(kotlin.time.Duration.ZERO, startedAt.elapsedNow())
            assertEquals(0L, apiRateLimitState.requestDelayMillis)
            resumeWait.complete(Unit)
            assertTrue(next.await() != null)

            repeat(55) { assertTrue(service.loadByUrl(redirectingRequest) != null) }
            val sustainedStart = clock.markNow()
            repeat(5) { assertTrue(service.loadByUrl(redirectingRequest) != null) }
            assertEquals(5.seconds, sustainedStart.elapsedNow())
            assertEquals(2, cdnRejections.get())
        }
    }

    @ParameterizedTest
    @ValueSource(booleans = [false, true])
    fun `a redirected lookup refreshes the target after the full cooldown`(cancelFirstLookup: Boolean) = runBlocking {
        withTimeout(5.seconds) {
            val originalUrl = "$PLAYER_CONFIG_URL/119947-000-A"
            val oldTarget = "https://manifest-arte.akamaized.net/temporary-config/old"
            val newTarget = "https://manifest-arte.akamaized.net/temporary-config/new"
            val clock = TestTimeSource()
            val startedAt = clock.markNow()
            val requests = CopyOnWriteArrayList<String>()
            val client = OkHttpClient.Builder().addInterceptor { chain ->
                val request = chain.request()
                val url = request.url.toString()
                val targetExpired = startedAt.elapsedNow() >= 30.seconds
                requests += url
                val response = Response.Builder().request(request).protocol(Protocol.HTTP_1_1)
                when {
                    url == originalUrl -> response.code(302).message("Found")
                        .header("Location", if (targetExpired) newTarget else oldTarget).body("".toResponseBody())
                    url == oldTarget && !targetExpired -> response.code(429).message("Too Many Requests")
                        .header("Retry-After", "90").body("".toResponseBody())
                    url == oldTarget -> response.code(403).message("Expired").body("".toResponseBody())
                    url == newTarget -> response.code(200).message("OK").body(arteConfigJson().toResponseBody())
                    else -> error("Unexpected ARTE request: $url")
                }.build()
            }.build()
            val waits = Channel<kotlin.time.Duration>(Channel.UNLIMITED)
            val resumeWait = CompletableDeferred<Unit>()
            val apiRateLimitState = TestArteRateLimitState()
            val service = ArteOnlineSearchService(OkHttpOnlineSearchHttpClient(client), apiRateLimitState, clock) { wait ->
                waits.send(wait)
                resumeWait.await()
                clock += wait
            }
            val request = OnlineUrlRequest(OnlineSearchProvider.ARTE, originalUrl)
            val first = async { service.loadByUrl(request) }
            assertEquals(90.seconds, waits.receive())
            assertEquals(listOf(originalUrl, oldTarget), requests)

            val lookup = if (cancelFirstLookup) {
                clock += 15.seconds
                first.cancelAndJoin()
                val replacement = async { service.loadByUrl(request) }
                assertEquals(75.seconds, waits.receive())
                assertEquals(listOf(originalUrl, oldTarget, originalUrl), requests)
                replacement
            } else {
                first
            }
            val requestsBeforeRetry = requests.toList()
            resumeWait.complete(Unit)

            assertTrue(lookup.await() != null)
            assertEquals(90.seconds, startedAt.elapsedNow())
            assertEquals(requestsBeforeRetry + listOf(originalUrl, newTarget), requests)
            assertEquals(0L, apiRateLimitState.requestDelayMillis)
        }
    }

    @Test
    fun `concurrent lookups both refresh redirects queued during a CDN cooldown`() = runBlocking {
        withTimeout(5.seconds) {
            val originalUrl = "$PLAYER_CONFIG_URL/119947-000-A"
            val oldTarget = "https://manifest-arte.akamaized.net/temporary-config/old"
            val newTarget = "https://manifest-arte.akamaized.net/temporary-config/new"
            val clock = TestTimeSource()
            val startedAt = clock.markNow()
            val requests = mutableListOf<String>()
            val waits = Channel<kotlin.time.Duration>(Channel.UNLIMITED)
            val resumeWait = CompletableDeferred<Unit>()
            val client = object : OnlineSearchHttpClient {
                override suspend fun get(url: String, headers: Map<String, String>): String = error("Expected a single hop")

                override suspend fun getHop(url: String): OnlineSearchHttpHop {
                    requests += url
                    val expired = startedAt.elapsedNow() >= 30.seconds
                    return when (url) {
                        originalUrl -> OnlineSearchHttpHop.Redirect((if (expired) newTarget else oldTarget).toHttpUrl())
                        oldTarget -> if (expired) {
                            throw OnlineSearchHttpException(403, "Expired", url)
                        } else {
                            throw OnlineSearchHttpException(429, "Too Many Requests", url, Duration.ofSeconds(90))
                        }
                        newTarget -> OnlineSearchHttpHop.Body(arteConfigJson())
                        else -> error("Unexpected ARTE request: $url")
                    }
                }
            }
            val service = ArteOnlineSearchService(client, TestArteRateLimitState(), clock) { wait ->
                if (!resumeWait.isCompleted) {
                    waits.send(wait)
                    resumeWait.await()
                }
                clock += wait
            }
            val request = OnlineUrlRequest(OnlineSearchProvider.ARTE, originalUrl)
            val cancelled = launch { service.loadByUrl(request) }
            assertEquals(90.seconds, waits.receive())
            clock += 15.seconds
            cancelled.cancelAndJoin()

            val waiting = async { service.loadByUrl(request) }
            assertEquals(75.seconds, waits.receive())
            val queued = async(start = CoroutineStart.UNDISPATCHED) { service.loadByUrl(request) }
            assertEquals(listOf(originalUrl, oldTarget, originalUrl, originalUrl), requests)

            resumeWait.complete(Unit)
            assertTrue(waiting.await() != null)
            assertTrue(queued.await() != null)
            assertEquals(1, requests.count { it == oldTarget })
            assertEquals(2, requests.count { it == newTarget })
            assertEquals(5, requests.count { it == originalUrl })
            assertEquals(100.seconds, startedAt.elapsedNow())
        }
    }

    @Test
    fun `a delayed redirect response still refreshes a target issued during the CDN cooldown`() = runBlocking {
        withTimeout(5.seconds) {
            val originalUrl = "$PLAYER_CONFIG_URL/119947-000-A"
            val oldRoute = "$PLAYER_CONFIG_URL/redirect-old"
            val newRoute = "$PLAYER_CONFIG_URL/redirect-new"
            val oldTarget = "https://manifest-arte.akamaized.net/temporary-config/old"
            val newTarget = "https://manifest-arte.akamaized.net/temporary-config/new"
            val clock = TestTimeSource()
            val startedAt = clock.markNow()
            val waits = Channel<kotlin.time.Duration>(Channel.UNLIMITED)
            val redirectIssued = CompletableDeferred<Unit>()
            val deliverRedirect = CompletableDeferred<Unit>()
            val requests = mutableListOf<String>()
            var originalRequests = 0
            val client = object : OnlineSearchHttpClient {
                override suspend fun get(url: String, headers: Map<String, String>): String = error("Expected a single hop")

                override suspend fun getHop(url: String): OnlineSearchHttpHop {
                    requests += url
                    val expired = startedAt.elapsedNow() >= 88.seconds
                    return when (url) {
                        originalUrl -> {
                            val route = if (expired) newRoute else oldRoute
                            if (++originalRequests == 2) {
                                redirectIssued.complete(Unit)
                                deliverRedirect.await()
                            }
                            OnlineSearchHttpHop.Redirect(route.toHttpUrl())
                        }
                        oldRoute -> OnlineSearchHttpHop.Redirect(oldTarget.toHttpUrl())
                        newRoute -> OnlineSearchHttpHop.Redirect(newTarget.toHttpUrl())
                        oldTarget -> if (expired) {
                            throw OnlineSearchHttpException(403, "Expired", url)
                        } else {
                            throw OnlineSearchHttpException(429, "Too Many Requests", url, Duration.ofSeconds(90))
                        }
                        newTarget -> OnlineSearchHttpHop.Body(arteConfigJson())
                        else -> error("Unexpected ARTE request: $url")
                    }
                }
            }
            val service = ArteOnlineSearchService(client, TestArteRateLimitState(), clock) { wait ->
                waits.send(wait)
                awaitCancellation()
            }
            val request = OnlineUrlRequest(OnlineSearchProvider.ARTE, originalUrl)
            val cancelled = launch { service.loadByUrl(request) }
            assertEquals(90.seconds, waits.receive())
            clock += 85.seconds
            cancelled.cancelAndJoin()

            val replacement = async { service.loadByUrl(request) }
            redirectIssued.await()
            clock += 6.seconds
            deliverRedirect.complete(Unit)

            assertTrue(replacement.await() != null)
            assertEquals(91.seconds, startedAt.elapsedNow())
            assertEquals(
                listOf(originalUrl, oldRoute, oldTarget, originalUrl, oldRoute, originalUrl, newRoute, newTarget),
                requests,
            )
        }
    }

    @Test
    fun `a replacement lookup shares one three-attempt budget across CDN hosts`() = runBlocking {
        withTimeout(5.seconds) {
            val clock = TestTimeSource()
            val startedAt = clock.markNow()
            val originalUrl = "$PLAYER_CONFIG_URL/119947-000-A"
            val firstCdn = "https://one.example.invalid/config"
            val secondCdn = "https://two.example.invalid/config"
            val finalCdn = "https://three.example.invalid/config"
            val cdnCalls = mutableListOf<Pair<String, kotlin.time.Duration>>()
            val waits = Channel<kotlin.time.Duration>(Channel.UNLIMITED)
            val resumeWait = CompletableDeferred<Unit>()
            var cdnRecovered = false
            val client = object : OnlineSearchHttpClient {
                override suspend fun get(url: String, headers: Map<String, String>): String = error("Expected a single hop")

                override suspend fun getHop(url: String): OnlineSearchHttpHop {
                    val elapsed = startedAt.elapsedNow()
                    if (url == originalUrl) {
                        val destination = when {
                            elapsed < 120.seconds -> firstCdn
                            elapsed < 160.seconds -> secondCdn
                            else -> finalCdn
                        }
                        return OnlineSearchHttpHop.Redirect(destination.toHttpUrl())
                    }

                    cdnCalls += url to elapsed
                    if (cdnRecovered) return OnlineSearchHttpHop.Body(arteConfigJson())
                    check(cdnCalls.size <= 4) { "Replacement lookup exceeded its three-attempt budget" }
                    val retryAfter = when (url) {
                        firstCdn -> if (elapsed == kotlin.time.Duration.ZERO) 90L else 30L
                        secondCdn -> 40L
                        finalCdn -> 50L
                        else -> error("Unexpected ARTE request: $url")
                    }
                    throw OnlineSearchHttpException(429, "Too Many Requests", url, Duration.ofSeconds(retryAfter))
                }
            }
            val apiRateLimitState = TestArteRateLimitState()
            val service = ArteOnlineSearchService(client, apiRateLimitState, clock) { wait ->
                if (!resumeWait.isCompleted) {
                    waits.send(wait)
                    resumeWait.await()
                }
                clock += wait
            }
            val request = OnlineUrlRequest(OnlineSearchProvider.ARTE, originalUrl)
            val cancelled = launch { service.loadByUrl(request) }
            assertEquals(90.seconds, waits.receive())
            clock += 15.seconds
            cancelled.cancelAndJoin()

            val replacement = async { assertThrows<OnlineSearchHttpException> { service.loadByUrl(request) } }
            assertEquals(75.seconds, waits.receive())
            resumeWait.complete(Unit)

            val failure = replacement.await()
            assertEquals(429, failure.statusCode)
            assertEquals(finalCdn, failure.url)
            assertEquals(
                listOf(firstCdn to 0.seconds, firstCdn to 90.seconds, secondCdn to 120.seconds, finalCdn to 160.seconds),
                cdnCalls,
            )
            assertEquals(160.seconds, startedAt.elapsedNow())
            assertEquals(0L, apiRateLimitState.requestDelayMillis)

            cdnRecovered = true
            assertTrue(service.loadByUrl(request) != null)
            assertEquals(finalCdn to 210.seconds, cdnCalls.last())
            assertEquals(5, cdnCalls.size)
        }
    }

    private companion object {
        private const val SEARCH_CONTENT_URL =
            "https://api.arte.tv/api/emac/v4/de/web/zones/f75e7b54-a37b-4f8f-b01b-dbefa877c041/content"
        private const val PLAYER_CONFIG_URL = "https://api.arte.tv/api/player/v2/config/de"
        private const val COLLECTION_SUBCOLLECTION_CONTENT_URL =
            "https://api-cdn.arte.tv/api/emac/v4/de/web/zones/8fbf35e4-324d-4d3e-a194-28f7d288789b/content"
    }
}

private val arteCatalogueLanguages = listOf("de", "fr", "en", "es", "pl", "it", "ro")
private const val arteSearchUrl =
    "https://api.arte.tv/api/emac/v4/de/web/zones/f75e7b54-a37b-4f8f-b01b-dbefa877c041/content"

private fun otherCatalogueUrls(query: String): List<String> =
    arteCatalogueLanguages.drop(1).map { language -> "${arteSearchUrl.replace("/de/", "/$language/")}?page=1&query=$query" }

private fun Map<String, String>.withEmptyOtherCatalogues(): Map<String, String> {
    val emptyCatalogues = keys.filter { it.startsWith(arteSearchUrl) }.flatMap { url ->
        val query = url.substringAfter("&query=")
        otherCatalogueUrls(query).map { it to """{ "data": [], "pagination": null }""" }
    }.toMap()
    return emptyCatalogues + this
}

private fun arteHttpClient(responses: Map<String, String>): FakeOnlineSearchHttpClient =
    FakeOnlineSearchHttpClient(responses.withEmptyOtherCatalogues())

private class TestArteRateLimitState(
    override var requestDelayMillis: Long = 0,
) : ArteOnlineSearchRateLimitState

private fun testArteService(httpClient: OnlineSearchHttpClient): ArteOnlineSearchService =
    ArteOnlineSearchService(httpClient, TestArteRateLimitState())

private class RateLimitedOnlineSearchHttpClient(
    responses: Map<String, String>,
    private val rateLimitedUrl: String,
    retryAfterValues: List<Duration?>,
) : OnlineSearchHttpClient {
    private val responses = responses.withEmptyOtherCatalogues()
    val requestedUrls = ArrayList<String>()
    private val retryAfterValues = retryAfterValues.iterator()

    override suspend fun get(url: String, headers: Map<String, String>): String {
        requestedUrls += url
        if (url == rateLimitedUrl && retryAfterValues.hasNext()) {
            throw OnlineSearchHttpException(
                statusCode = 429,
                message = "Too Many Requests",
                retryAfter = retryAfterValues.next(),
            )
        }
        return responses[url] ?: error("No fake response registered for $url")
    }
}

private fun arteSearchJson(
    id: String = "118267-006-A",
    title: String = "Re: Tatort Kirche - Betroffene klagen an",
    language: String = "de",
    url: String = "https://www.arte.tv/$language/videos/${id.substringBefore('_')}/re-tatort-kirche-betroffene-klagen-an/",
    page: Int = 1,
    pages: Int = 1,
    totalCount: Long = 1,
    playerConfig: String? = "https://api.arte.tv/api/player/v2/config/$language/${id.substringBefore('_')}",
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
