package mediathek.gui.tabs.tab_online_search

import kotlinx.coroutines.runBlocking
import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Assertions.assertThrows
import org.junit.jupiter.api.Test
import java.time.Duration

class ArdOnlineSearchServiceTest {
    @Test
    fun `ARD search requests first page and exposes next page`() = runBlocking {
        val searchUrl = "https://api.ardmediathek.de/search-system/search/vods/ard?query=tatort&pageNumber=0&pageSize=20&audioDes=false&signLang=false&subtitle=false&childCont=false&sortingCriteria=SCORE_DESC&platform=MEDIA_THEK"
        val detailUrl = "https://api.ardmediathek.de/page-gateway/pages/ard/item/abc123"
        val http = FakeOnlineSearchHttpClient(
            mapOf(
                searchUrl to """{"pagination":{"totalElements":25},"teasers":[{"id":"abc123"}]}""",
                detailUrl to ArdFixtures.detailJson("abc123", "Testfilm", "https://cdn.example/ard.mp4"),
            )
        )
        val service = ArdOnlineSearchService(http)

        val page = service.search(OnlineSearchRequest(OnlineSearchProvider.ARD, "tatort"))

        assertEquals(1, page.results.size)
        assertEquals("1", page.nextToken)
        assertEquals(OnlineSearchProvider.ARD, page.results.single().provider)
        assertEquals("Testfilm", page.results.single().title)
        assertEquals("https://cdn.example/ard.mp4", page.results.single().normalQualityUrl)
        assertEquals(Duration.ofMinutes(90), page.results.single().duration)
    }

    @Test
    fun `ARD search skips stale detail returning server error when another item succeeds`() = runBlocking {
        val searchUrl = "https://api.ardmediathek.de/search-system/search/vods/ard?query=stale&pageNumber=0&pageSize=20&audioDes=false&signLang=false&subtitle=false&childCont=false&sortingCriteria=SCORE_DESC&platform=MEDIA_THEK"
        val staleDetailUrl = "https://api.ardmediathek.de/page-gateway/pages/ard/item/stale-id"
        val validDetailUrl = "https://api.ardmediathek.de/page-gateway/pages/ard/item/valid-id"
        val http = object : OnlineSearchHttpClient {
            override suspend fun get(url: String, headers: Map<String, String>): String = when (url) {
                searchUrl -> """{"pagination":{"totalElements":2},"teasers":[{"id":"stale-id"},{"id":"valid-id"}]}"""
                staleDetailUrl -> throw OnlineSearchHttpException(statusCode = 500, message = "", url = url)
                validDetailUrl -> ArdFixtures.detailJson("valid-id", "Valid film", "https://cdn.example/valid.mp4")
                else -> error("Unexpected URL: $url")
            }
        }
        val service = ArdOnlineSearchService(http)

        val page = service.search(OnlineSearchRequest(OnlineSearchProvider.ARD, "stale"))

        assertEquals(1, page.results.size)
        assertEquals("Valid film", page.results.single().title)
    }

    @Test
    fun `ARD search propagates server error when every detail request fails`() {
        val searchUrl = "https://api.ardmediathek.de/search-system/search/vods/ard?query=outage&pageNumber=0&pageSize=20&audioDes=false&signLang=false&subtitle=false&childCont=false&sortingCriteria=SCORE_DESC&platform=MEDIA_THEK"
        val http = object : OnlineSearchHttpClient {
            override suspend fun get(url: String, headers: Map<String, String>): String {
                if (url == searchUrl) {
                    return """{"pagination":{"totalElements":2},"teasers":[{"id":"first"},{"id":"second"}]}"""
                }
                throw OnlineSearchHttpException(statusCode = 500, message = "", url = url)
            }
        }
        val service = ArdOnlineSearchService(http)

        val exception = assertThrows(OnlineSearchHttpException::class.java) {
            runBlocking {
                service.search(OnlineSearchRequest(OnlineSearchProvider.ARD, "outage"))
            }
        }

        assertEquals(500, exception.statusCode)
    }

    @Test
    fun `ARD URL search accepts trailing slash`() = runBlocking {
        val detailUrl = "https://api.ardmediathek.de/page-gateway/pages/ard/item/abc123"
        val http = FakeOnlineSearchHttpClient(
            mapOf(
                detailUrl to ArdFixtures.detailJson("abc123", "Testfilm", "https://cdn.example/ard.mp4"),
            )
        )
        val service = ArdOnlineSearchService(http)

        val result = service.loadByUrl(
            OnlineUrlRequest(OnlineSearchProvider.ARD, "https://www.ardmediathek.de/video/abc123/"),
        )

        assertEquals("Testfilm", result?.title)
        assertEquals(detailUrl, http.requestedUrls.single())
    }

    @Test
    fun `ARD URL search skips stale detail returning server error`() = runBlocking {
        val id = "Y3JpZDovL2JyLmRlL2Jyb2FkY2FzdC9GMjAyMldPMDEyODU5QTA"
        val detailUrl = "https://api.ardmediathek.de/page-gateway/pages/ard/item/$id"
        val http = object : OnlineSearchHttpClient {
            override suspend fun get(url: String, headers: Map<String, String>): String {
                assertEquals(detailUrl, url)
                throw OnlineSearchHttpException(statusCode = 500, message = "", url = url)
            }
        }
        val service = ArdOnlineSearchService(http)

        val result = service.loadByUrl(
            OnlineUrlRequest(OnlineSearchProvider.ARD, "https://www.ardmediathek.de/video/$id"),
        )

        assertEquals(null, result)
    }

    @Test
    fun `ARD URL search skips unavailable detail`() = runBlocking {
        val detailUrl = "https://api.ardmediathek.de/page-gateway/pages/ard/item/gone-id"
        val http = object : OnlineSearchHttpClient {
            override suspend fun get(url: String, headers: Map<String, String>): String {
                assertEquals(detailUrl, url)
                throw OnlineSearchHttpException(statusCode = 410, message = "Gone", url = url)
            }
        }
        val service = ArdOnlineSearchService(http)

        val result = service.loadByUrl(
            OnlineUrlRequest(OnlineSearchProvider.ARD, "https://www.ardmediathek.de/video/gone-id"),
        )

        assertEquals(null, result)
    }

    @Test
    fun `ARD URL search skips item with null media collection`() = runBlocking {
        val detailUrl = "https://api.ardmediathek.de/page-gateway/pages/ard/item/abc123"
        val http = FakeOnlineSearchHttpClient(
            mapOf(
                detailUrl to ArdFixtures.detailJsonWithNullMediaCollection("abc123", "Testfilm"),
            )
        )
        val service = ArdOnlineSearchService(http)

        val result = service.loadByUrl(
            OnlineUrlRequest(OnlineSearchProvider.ARD, "https://www.ardmediathek.de/video/abc123"),
        )

        assertEquals(null, result)
    }
}

private object ArdFixtures {
    fun detailJson(id: String, title: String, streamUrl: String): String = """
        {
          "widgets": [
            {
              "id": "$id",
              "title": "$title",
              "synopsis": "ARD Beschreibung",
              "broadcastedOn": "2026-01-02T20:15:00+01:00",
              "show": {"title": "ARD Thema"},
              "mediaCollection": {
                "embedded": {
                  "meta": {
                    "durationSeconds": 5400,
                    "synopsis": "ARD Beschreibung aus Metadaten"
                  },
                  "streams": [
                    {
                      "kind": "main",
                      "media": [
                        {"url": "$streamUrl", "maxHResolutionPx": 1920}
                      ]
                    }
                  ]
                }
              }
            }
          ]
        }
    """.trimIndent()

    fun detailJsonWithNullMediaCollection(id: String, title: String): String = """
        {
          "widgets": [
            {
              "id": "$id",
              "title": "$title",
              "synopsis": "ARD Beschreibung",
              "mediaCollection": null
            }
          ]
        }
    """.trimIndent()
}
