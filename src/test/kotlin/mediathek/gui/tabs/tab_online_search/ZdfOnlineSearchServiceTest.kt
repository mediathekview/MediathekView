package mediathek.gui.tabs.tab_online_search

import kotlinx.coroutines.runBlocking
import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Test
import java.time.Clock
import java.time.Instant
import java.time.ZoneId
import java.time.ZoneOffset

class ZdfOnlineSearchServiceTest {
    @Test
    fun `ZDF search loads token graphql results and document details`() = runBlocking {
        val indexUrl = "https://www.zdf.de/suche"
        val detailUrl = "https://api.zdf.de/content/documents/testfilm-100.json"
        val ptmdUrl = "https://api.zdf.de/tmd/ptmd/test"
        val http = FakeOnlineSearchHttpClient(
            mapOf(
                indexUrl to ZdfFixtures.indexHtml("token-123"),
                detailUrl to ZdfFixtures.detailJson("Testfilm", ptmdUrl),
                ptmdUrl to ZdfFixtures.downloadJson("https://cdn.example/zdf.mp4"),
            )
        )
        val service = ZdfOnlineSearchService(http) { _, _, _ ->
            ZdfSearchGraphqlResult(
                canonicalPaths = listOf("testfilm-100"),
                nextCursor = "MjQ=",
                totalResults = null,
            )
        }

        val page = service.search(OnlineSearchRequest(OnlineSearchProvider.ZDF, "test"))

        assertEquals("MjQ=", page.nextToken)
        assertEquals(OnlineSearchProvider.ZDF, page.results.single().provider)
        assertEquals("Testfilm - Folgentitel", page.results.single().title)
        assertEquals("https://cdn.example/zdf.mp4", page.results.single().normalQualityUrl)
        assertEquals("Bearer token-123", http.requestedHeaders.first { it["api-auth"] != null }["api-auth"])
    }

    @Test
    fun `ZDF search reads nested target ptmd template and prefixes relative API URLs`() = runBlocking {
        val indexUrl = "https://www.zdf.de/suche"
        val detailUrl = "https://api.zdf.de/content/documents/miami-ibiza-wismar-100.json"
        val ptmdUrl = "https://api.zdf.de/tmd/2/android_native_5/vod/ptmd/mediathek/260121_1800_sendung_sok7/3"
        val http = FakeOnlineSearchHttpClient(
            mapOf(
                indexUrl to ZdfFixtures.indexHtml("token-123"),
                detailUrl to ZdfFixtures.realisticDetailJson("Miami, Ibiza, Wismar"),
                ptmdUrl to ZdfFixtures.downloadJson("https://cdn.example/soko-wismar.mp4"),
            )
        )
        val service = ZdfOnlineSearchService(http) { _, _, _ ->
            ZdfSearchGraphqlResult(
                canonicalPaths = listOf("miami-ibiza-wismar-100"),
                nextCursor = null,
                totalResults = 1,
            )
        }

        val page = service.search(OnlineSearchRequest(OnlineSearchProvider.ZDF, "SOKO Wismar"))

        assertEquals(1, page.results.size)
        assertEquals("SOKO Wismar", page.results.single().topic)
        assertEquals("Miami, Ibiza, Wismar", page.results.single().title)
        assertEquals("https://cdn.example/soko-wismar.mp4", page.results.single().normalQualityUrl)
        assertEquals(ptmdUrl, http.requestedUrls.last())
    }

    @Test
    fun `ZDF URL search uses brand title as topic`() = runBlocking {
        val indexUrl = "https://www.zdf.de/suche"
        val detailUrl = "https://api.zdf.de/content/documents/miami-ibiza-wismar-100.json"
        val ptmdUrl = "https://api.zdf.de/tmd/2/android_native_5/vod/ptmd/mediathek/260121_1800_sendung_sok7/3"
        val http = FakeOnlineSearchHttpClient(
            mapOf(
                indexUrl to ZdfFixtures.indexHtml("token-123"),
                detailUrl to ZdfFixtures.realisticDetailJson("Miami, Ibiza, Wismar"),
                ptmdUrl to ZdfFixtures.downloadJson("https://cdn.example/soko-wismar.mp4"),
            )
        )
        val service = ZdfOnlineSearchService(http)

        val result = service.loadByUrl(
            OnlineUrlRequest(
                OnlineSearchProvider.ZDF,
                "https://www.zdf.de/video/serien/soko-wismar-104/miami-ibiza-wismar-100",
            ),
        )

        assertEquals("SOKO Wismar", result?.topic)
        assertEquals("Miami, Ibiza, Wismar", result?.title)
    }

    @Test
    fun `ZDF URL search accepts trailing slash`() = runBlocking {
        val indexUrl = "https://www.zdf.de/suche"
        val detailUrl = "https://api.zdf.de/content/documents/miami-ibiza-wismar-100.json"
        val ptmdUrl = "https://api.zdf.de/tmd/2/android_native_5/vod/ptmd/mediathek/260121_1800_sendung_sok7/3"
        val http = FakeOnlineSearchHttpClient(
            mapOf(
                indexUrl to ZdfFixtures.indexHtml("token-123"),
                detailUrl to ZdfFixtures.realisticDetailJson("Miami, Ibiza, Wismar"),
                ptmdUrl to ZdfFixtures.downloadJson("https://cdn.example/soko-wismar.mp4"),
            )
        )
        val service = ZdfOnlineSearchService(http)

        val result = service.loadByUrl(
            OnlineUrlRequest(
                OnlineSearchProvider.ZDF,
                "https://www.zdf.de/video/serien/soko-wismar-104/miami-ibiza-wismar-100/",
            ),
        )

        assertEquals("Miami, Ibiza, Wismar", result?.title)
        assertEquals(detailUrl, http.requestedUrls[1])
    }

    @Test
    fun `ZDF URL search skips item with null main video content`() = runBlocking {
        val indexUrl = "https://www.zdf.de/suche"
        val detailUrl = "https://api.zdf.de/content/documents/null-main-video-100.json"
        val http = FakeOnlineSearchHttpClient(
            mapOf(
                indexUrl to ZdfFixtures.indexHtml("token-123"),
                detailUrl to ZdfFixtures.detailJsonWithNullMainVideoContent("Null Video"),
            )
        )
        val service = ZdfOnlineSearchService(http)

        val result = service.loadByUrl(
            OnlineUrlRequest(OnlineSearchProvider.ZDF, "https://www.zdf.de/video/null-main-video-100"),
        )

        assertEquals(null, result)
    }

    @Test
    fun `ZDF URL search falls back to title when optional topic fields are null`() = runBlocking {
        val indexUrl = "https://www.zdf.de/suche"
        val detailUrl = "https://api.zdf.de/content/documents/null-topic-100.json"
        val ptmdUrl = "https://api.zdf.de/tmd/ptmd/test"
        val http = FakeOnlineSearchHttpClient(
            mapOf(
                indexUrl to ZdfFixtures.indexHtml("token-123"),
                detailUrl to ZdfFixtures.detailJsonWithNullTopicFields("Null Topic", ptmdUrl),
                ptmdUrl to ZdfFixtures.downloadJson("https://cdn.example/null-topic.mp4"),
            )
        )
        val service = ZdfOnlineSearchService(http)

        val result = service.loadByUrl(
            OnlineUrlRequest(OnlineSearchProvider.ZDF, "https://www.zdf.de/video/null-topic-100"),
        )

        assertEquals("Null Topic", result?.topic)
        assertEquals("https://cdn.example/null-topic.mp4", result?.normalQualityUrl)
    }

    @Test
    fun `ZDF search creates a result for each downloadable audio language variant`() = runBlocking {
        val indexUrl = "https://www.zdf.de/suche"
        val detailUrl = "https://api.zdf.de/content/documents/miami-ibiza-wismar-100.json"
        val ptmdUrl = "https://api.zdf.de/tmd/2/android_native_5/vod/ptmd/mediathek/260121_1800_sendung_sok7/3"
        val http = FakeOnlineSearchHttpClient(
            mapOf(
                indexUrl to ZdfFixtures.indexHtml("token-123"),
                detailUrl to ZdfFixtures.realisticDetailJson("Miami, Ibiza, Wismar"),
                ptmdUrl to ZdfFixtures.downloadJsonWithAudioDescription(
                    mainUrl = "https://cdn.example/soko-wismar-main.mp4",
                    audioDescriptionUrl = "https://cdn.example/soko-wismar-ad.mp4",
                ),
            )
        )
        val service = ZdfOnlineSearchService(http) { _, _, _ ->
            ZdfSearchGraphqlResult(
                canonicalPaths = listOf("miami-ibiza-wismar-100"),
                nextCursor = null,
                totalResults = 1,
            )
        }

        val page = service.search(OnlineSearchRequest(OnlineSearchProvider.ZDF, "SOKO Wismar"))

        assertEquals(2, page.results.size)
        assertEquals("Miami, Ibiza, Wismar", page.results[0].title)
        assertEquals("https://cdn.example/soko-wismar-main.mp4", page.results[0].normalQualityUrl)
        assertEquals(false, page.results[0].isAudioDescription)
        assertEquals("Miami, Ibiza, Wismar (Audiodeskription)", page.results[1].title)
        assertEquals("https://cdn.example/soko-wismar-ad.mp4", page.results[1].normalQualityUrl)
        assertEquals(true, page.results[1].isAudioDescription)
    }

    @Test
    fun `ZDF search creates sign language result from dgs stream ptmd template`() = runBlocking {
        val indexUrl = "https://www.zdf.de/suche"
        val detailUrl = "https://api.zdf.de/content/documents/miami-ibiza-wismar-100.json"
        val defaultPtmdUrl = "https://api.zdf.de/tmd/2/android_native_5/vod/ptmd/mediathek/260121_1800_sendung_sok7/3"
        val dgsPtmdUrl = "https://api.zdf.de/tmd/2/android_native_5/vod/ptmd/mediathek/260121_1800_sendung_sok7_dgs/3"
        val http = FakeOnlineSearchHttpClient(
            mapOf(
                indexUrl to ZdfFixtures.indexHtml("token-123"),
                detailUrl to ZdfFixtures.realisticDetailJsonWithDgsStream("Miami, Ibiza, Wismar"),
                defaultPtmdUrl to ZdfFixtures.downloadJson("https://cdn.example/soko-wismar-main.mp4"),
                dgsPtmdUrl to ZdfFixtures.downloadJson("https://cdn.example/soko-wismar-dgs.mp4"),
            )
        )
        val service = ZdfOnlineSearchService(http) { _, _, _ ->
            ZdfSearchGraphqlResult(
                canonicalPaths = listOf("miami-ibiza-wismar-100"),
                nextCursor = null,
                totalResults = 1,
            )
        }

        val page = service.search(OnlineSearchRequest(OnlineSearchProvider.ZDF, "SOKO Wismar"))

        assertEquals(2, page.results.size)
        assertEquals("Miami, Ibiza, Wismar", page.results[0].title)
        assertEquals(false, page.results[0].isSignLanguage)
        assertEquals("https://cdn.example/soko-wismar-main.mp4", page.results[0].normalQualityUrl)
        assertEquals("Miami, Ibiza, Wismar (Gebärdensprache)", page.results[1].title)
        assertEquals(true, page.results[1].isSignLanguage)
        assertEquals("https://cdn.example/soko-wismar-dgs.mp4", page.results[1].normalQualityUrl)
        assertEquals(dgsPtmdUrl, http.requestedUrls.last())
    }

    @Test
    fun `ZDF search refreshes cached token after expiry`() = runBlocking {
        val clock = MutableClock(Instant.parse("2026-01-01T00:00:00Z"))
        val http = object : OnlineSearchHttpClient {
            private val indexResponses = ArrayDeque(
                listOf(
                    ZdfFixtures.indexHtml("token-old", "2026-01-01T00:30:00Z"),
                    ZdfFixtures.indexHtml("token-new", "2026-01-01T02:00:00Z"),
                ),
            )
            var indexRequests = 0

            override suspend fun get(url: String, headers: Map<String, String>): String {
                assertEquals("https://www.zdf.de/suche", url)
                indexRequests++
                return indexResponses.removeFirst()
            }
        }
        val usedBearers = ArrayList<String>()
        val service = ZdfOnlineSearchService(
            httpClient = http,
            clock = clock,
            graphqlLoader = { _, _, bearer ->
                usedBearers.add(bearer)
                ZdfSearchGraphqlResult(emptyList(), null, null)
            },
        )

        service.search(OnlineSearchRequest(OnlineSearchProvider.ZDF, "test"))
        clock.instant = Instant.parse("2026-01-01T00:31:00Z")
        service.search(OnlineSearchRequest(OnlineSearchProvider.ZDF, "test"))

        assertEquals(listOf("token-old", "token-new"), usedBearers)
        assertEquals(2, http.indexRequests)
    }

    @Test
    fun `ZDF URL search refreshes token and retries once after unauthorized API response`() = runBlocking {
        val indexUrl = "https://www.zdf.de/suche"
        val detailUrl = "https://api.zdf.de/content/documents/miami-ibiza-wismar-100.json"
        val ptmdUrl = "https://api.zdf.de/tmd/2/android_native_5/vod/ptmd/mediathek/260121_1800_sendung_sok7/3"
        val usedTokens = mutableListOf<String>()
        val http = object : OnlineSearchHttpClient {
            private val indexResponses = ArrayDeque(
                listOf(
                    ZdfFixtures.indexHtml("token-old"),
                    ZdfFixtures.indexHtml("token-new"),
                ),
            )
            private var detailRequests = 0
            var indexRequests = 0

            override suspend fun get(url: String, headers: Map<String, String>): String {
                headers["api-auth"]?.removePrefix("Bearer ")?.let(usedTokens::add)
                return when (url) {
                    indexUrl -> {
                        indexRequests++
                        indexResponses.removeFirst()
                    }
                    detailUrl -> {
                        detailRequests++
                        if (detailRequests == 1) {
                            throw OnlineSearchHttpException(401, "Unauthorized")
                        }
                        ZdfFixtures.realisticDetailJson("Miami, Ibiza, Wismar")
                    }
                    ptmdUrl -> ZdfFixtures.downloadJson("https://cdn.example/soko-wismar.mp4")
                    else -> error("Unexpected URL: $url")
                }
            }
        }
        val service = ZdfOnlineSearchService(http)

        val result = service.loadByUrl(
            OnlineUrlRequest(
                OnlineSearchProvider.ZDF,
                "https://www.zdf.de/video/serien/soko-wismar-104/miami-ibiza-wismar-100",
            ),
        )

        assertEquals("Miami, Ibiza, Wismar", result?.title)
        assertEquals("https://cdn.example/soko-wismar.mp4", result?.normalQualityUrl)
        assertEquals(listOf("token-old", "token-new", "token-new"), usedTokens)
        assertEquals(2, http.indexRequests)
    }
}

private class MutableClock(
    var instant: Instant,
) : Clock() {
    override fun getZone(): ZoneId = ZoneOffset.UTC
    override fun withZone(zone: ZoneId): Clock = this
    override fun instant(): Instant = instant
}

private object ZdfFixtures {
    fun indexHtml(token: String, expiresAt: String = "2026-06-16T06:01:42+02:00") = """
        <html><body><script>window.x="{\"appToken\":{\"apiToken\":\"$token\",\"expiresAt\":\"$expiresAt\"}}";</script></body></html>
    """.trimIndent()

    fun detailJson(title: String, ptmdTemplate: String) = """
        {
          "title":"$title",
          "subtitle":"Folgentitel",
          "leadParagraph":"ZDF Beschreibung",
          "editorialDate":"2026-01-02T20:15:00.000+01:00",
          "http://zdf.de/rels/sharing-url":"https://www.zdf.de/video/testfilm-100",
          "mainVideoContent": {
            "http://zdf.de/rels/target": {"duration": 3600},
            "http://zdf.de/rels/streams/ptmd-template":"$ptmdTemplate"
          }
        }
    """.trimIndent()

    fun realisticDetailJson(title: String) = """
        {
          "title":"$title",
          "leadParagraph":"SOKO Wismar Beschreibung",
          "editorialDate":"2026-01-14T10:00:00.000+01:00",
          "http://zdf.de/rels/sharing-url":"https://www.zdf.de/video/serien/soko-wismar-104/miami-ibiza-wismar-100",
          "http://zdf.de/rels/brand": {"title":"SOKO Wismar"},
          "mainVideoContent": {
            "http://zdf.de/rels/target": {
              "duration":2616,
              "http://zdf.de/rels/streams/ptmd-template":"/tmd/2/{playerId}/vod/ptmd/mediathek/260121_1800_sendung_sok7/3",
              "streams": {
                "default": {
                  "http://zdf.de/rels/streams/ptmd-template":"/tmd/2/{playerId}/vod/ptmd/mediathek/260121_1800_sendung_sok7/3"
                }
              }
            }
          }
        }
    """.trimIndent()

    fun realisticDetailJsonWithDgsStream(title: String) = """
        {
          "title":"$title",
          "leadParagraph":"SOKO Wismar Beschreibung",
          "editorialDate":"2026-01-14T10:00:00.000+01:00",
          "http://zdf.de/rels/sharing-url":"https://www.zdf.de/video/serien/soko-wismar-104/miami-ibiza-wismar-100",
          "http://zdf.de/rels/brand": {"title":"SOKO Wismar"},
          "mainVideoContent": {
            "http://zdf.de/rels/target": {
              "duration":2616,
              "http://zdf.de/rels/streams/ptmd-template":"/tmd/2/{playerId}/vod/ptmd/mediathek/260121_1800_sendung_sok7/3",
              "streams": {
                "default": {
                  "http://zdf.de/rels/streams/ptmd-template":"/tmd/2/{playerId}/vod/ptmd/mediathek/260121_1800_sendung_sok7/3"
                },
                "dgs": {
                  "http://zdf.de/rels/streams/ptmd-template":"/tmd/2/{playerId}/vod/ptmd/mediathek/260121_1800_sendung_sok7_dgs/3"
                }
              }
            }
          }
        }
    """.trimIndent()

    fun detailJsonWithNullMainVideoContent(title: String) = """
        {
          "title":"$title",
          "mainVideoContent": null
        }
    """.trimIndent()

    fun detailJsonWithNullTopicFields(title: String, ptmdTemplate: String) = """
        {
          "title":"$title",
          "leadParagraph":"ZDF Beschreibung",
          "http://zdf.de/rels/brand": null,
          "programmeItem": [
            {"http://zdf.de/rels/target": null}
          ],
          "mainVideoContent": {
            "http://zdf.de/rels/target": {"duration": 3600},
            "http://zdf.de/rels/streams/ptmd-template":"$ptmdTemplate"
          }
        }
    """.trimIndent()

    fun downloadJson(downloadUrl: String) = """
        {
          "priorityList":[
            {
              "formitaeten":[
                {
                  "mimeType":"video/mp4",
                  "qualities":[
                    {
                      "quality":"veryhigh",
                      "highestVerticalResolution":720,
                      "audio":{"tracks":[{"class":"main","language":"deu","uri":"$downloadUrl"}]}
                    }
                  ]
                }
              ]
            }
          ]
        }
    """.trimIndent()

    fun downloadJsonWithAudioDescription(mainUrl: String, audioDescriptionUrl: String) = """
        {
          "priorityList":[
            {
              "formitaeten":[
                {
                  "mimeType":"video/mp4",
                  "qualities":[
                    {
                      "quality":"veryhigh",
                      "highestVerticalResolution":720,
                      "audio":{
                        "tracks":[
                          {"class":"main","language":"deu","uri":"$mainUrl"},
                          {"class":"ad","language":"deu","uri":"$audioDescriptionUrl"}
                        ]
                      }
                    }
                  ]
                }
              ]
            }
          ]
        }
    """.trimIndent()
}
