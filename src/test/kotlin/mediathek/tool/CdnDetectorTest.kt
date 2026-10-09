package mediathek.tool

import org.junit.jupiter.api.Assertions.*
import org.junit.jupiter.api.Test

internal class CdnDetectorTest {
    @Test
    fun extractsHostFromFullUrlWithoutSignalsReturnsUnknown() {
        val url = "https://cdn-storage.br.de/"

        val result = CdnDetector.detect(url, { emptyList() }, { emptyList() })

        assertEquals("cdn-storage.br.de", result.host)
        assertEquals(CdnDetector.Classification.UNKNOWN, result.classification)
    }

    @Test
    fun detectsCloudFrontFromDnsAndHeaders() {
        val result = CdnDetector.detect(
            "example.org",
            { listOf("d111111abcdef8.cloudfront.net.") },
            {
                listOf(
                    "Via: 1.1 abcdef.cloudfront.net (CloudFront)",
                    "X-Amz-Cf-Id: test",
                    "X-Cache: Miss from cloudfront",
                )
            },
        )

        assertEquals(CdnDetector.Classification.LIKELY_CLOUDFRONT, result.classification)
        assertTrue(result.evidence.any { it.contains("CloudFront signal") })
        assertTrue(CdnDetector.isCdn(result))
    }

    @Test
    fun detectsAkamaiFromTypicalEdgeSignals() {
        val result = CdnDetector.detect(
            "media.example.org",
            { listOf("a23.g.akamai.net.") },
            {
                listOf(
                    "Server: AkamaiGHost",
                    "X-Akamai-Request-ID: test",
                )
            },
        )

        assertEquals(CdnDetector.Classification.LIKELY_AKAMAI, result.classification)
        assertTrue(result.evidence.any { it.contains("Akamai edge signal") })
    }

    @Test
    fun detectsAkamaiFromAkamaiHdDnsChain() {
        val result = CdnDetector.detect(
            "https://odgeomdr-a.akamaihd.net/mp4dyn2/9/FCMS-96305a70-0efe-4cbf-bccf-940222b85b8e-70dc5e5f2727_96.mp4",
            {
                listOf(
                    "odgeomdr-a.akamaihd.net.edgesuite.net.",
                    "a189.w4.akamai.net.",
                )
            },
            { emptyList() },
        )

        assertEquals("odgeomdr-a.akamaihd.net", result.host)
        assertEquals(CdnDetector.Classification.LIKELY_AKAMAI, result.classification)
        assertTrue(result.evidence.any { it.contains("Akamai edge signal") })
    }

    @Test
    fun detectsAkamaiFromNetStorageHeaderWithoutDnsSignals() {
        val result = CdnDetector.detect(
            "https://odgeomdr-a.akamaihd.net/mp4dyn2/9/FCMS-96305a70-0efe-4cbf-bccf-940222b85b8e-70dc5e5f2727_96.mp4",
            { emptyList() },
            { listOf("Server: AkamaiNetStorage") },
        )

        assertEquals(CdnDetector.Classification.LIKELY_AKAMAI, result.classification)
        assertTrue(result.evidence.any { it.contains("Akamai origin/storage signal") })
    }

    @Test
    fun detectsCloudFrontWithAkamaiOriginForBrMediaUrl() {
        val result = CdnDetector.detect(
            "https://cdn-storage.br.de/",
            {
                listOf(
                    "brprog-stream.trafficmanager.net.",
                    "dlbj3ois8upd3.cloudfront.net.",
                )
            },
            {
                listOf(
                    "server: AkamaiNetStorage",
                    "via: 1.1 9d638fe6fe3e82d4d1292fa9e998dfbe.cloudfront.net (CloudFront)",
                    "x-amz-cf-pop: TXL50-P4",
                    "x-amz-cf-id: m-4waYqOU-leqtv6JibiWhrgpSmdHkn54pr2fINU5lM9sj2pYZWGsQ==",
                )
            },
        )

        assertEquals("cdn-storage.br.de", result.host)
        assertEquals(CdnDetector.Classification.LIKELY_CLOUDFRONT_WITH_AKAMAI_ORIGIN, result.classification)
        assertTrue(result.evidence.any { it.contains("Akamai origin/storage signal") })
        assertTrue(result.evidence.any { it.contains("CloudFront signal") })
    }

    @Test
    fun returnsUnknownWithoutRelevantSignals() {
        val result = CdnDetector.detect(
            "origin.example.org",
            { listOf("origin.example.org.") },
            { listOf("server: nginx") },
        )

        assertEquals(CdnDetector.Classification.UNKNOWN, result.classification)
        assertTrue(result.evidence.isEmpty())
        assertFalse(CdnDetector.isCdn(result))
    }
}
