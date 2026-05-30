/*
 * Copyright (c) 2026 derreisende77.
 * This code was developed as part of the MediathekView project https://github.com/mediathekview/MediathekView
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package mediathek.tool

import kotlinx.coroutines.*
import mediathek.tool.http.MVHttpClient
import okhttp3.Request
import java.net.URI
import java.util.*
import javax.naming.NamingEnumeration
import javax.naming.NamingException
import javax.naming.directory.Attributes
import javax.naming.directory.DirContext
import javax.naming.directory.InitialDirContext

/**
 * Heuristic CDN detector based on DNS names and response headers.
 * It distinguishes the edge CDN from Akamai-backed origin storage.
 */
object CdnDetector {
    fun detect(input: String?): Result = detect(input, ::collectDnsEvidence, ::collectHeaderEvidence)

    fun isCdn(result: Result): Boolean = when (result.classification) {
        Classification.LIKELY_CLOUDFRONT,
        Classification.LIKELY_AKAMAI,
        Classification.LIKELY_CLOUDFRONT_WITH_AKAMAI_ORIGIN -> true

        Classification.AMBIGUOUS,
        Classification.UNKNOWN -> false
    }

    internal fun detect(
        input: String?,
        dnsCollector: suspend (String) -> List<String>,
        headerCollector: suspend (String?) -> List<String>,
    ): Result = runBlocking {
        detectInternal(input, dnsCollector, headerCollector)
    }

    internal fun extractHost(input: String?): String {
        if (input.isNullOrBlank()) {
            return ""
        }

        val trimmedInput = input.trim()
        runCatching {
            val uri = if ("://" in trimmedInput) URI(trimmedInput) else URI("https://$trimmedInput")
            uri.host
        }.getOrNull()?.let { return it }

        var value = trimmedInput
        val schemeSeparator = value.indexOf("://")
        if (schemeSeparator >= 0) {
            value = value.substring(schemeSeparator + 3)
        }

        val slashIndex = value.indexOf('/')
        if (slashIndex >= 0) {
            value = value.substring(0, slashIndex)
        }

        val colonIndex = value.indexOf(':')
        if (colonIndex >= 0) {
            value = value.substring(0, colonIndex)
        }

        return value
    }

    private suspend fun detectInternal(
        input: String?,
        dnsCollector: suspend (String) -> List<String>,
        headerCollector: suspend (String?) -> List<String>,
    ): Result = coroutineScope {
        val host = extractHost(input)
        val state = DetectionState()

        val dnsDeferred = async(Dispatchers.IO) { dnsCollector(host) }
        val headerDeferred = async(Dispatchers.IO) { headerCollector(input) }

        awaitAll(dnsDeferred, headerDeferred)
            .flatten()
            .forEach { inspectLine(it, state) }

        Result(host, classify(state), state.evidence.toList())
    }

    private fun inspectLine(rawLine: String, state: DetectionState) {
        val line = rawLine.lowercase()

        if (containsAny(line, "cloudfront.net", "x-amz-cf-id", "x-amz-cf-pop", "cloudfront")) {
            state.cloudFrontScore += 3
            state.sawCloudFrontEdge = true
            state.evidence += "CloudFront signal: $rawLine"
        }

        if (containsAny(line, "akamaihd.net", "akamai.net", "akamaighost", "akamaiedge", "edgekey.net", "edgesuite.net", "x-akamai")) {
            state.akamaiScore += 3
            state.evidence += "Akamai edge signal: $rawLine"
        }

        if ("akamainetstorage" in line) {
            state.akamaiScore += 3
            state.sawAkamaiOrigin = true
            state.evidence += "Akamai origin/storage signal: $rawLine"
        }
    }

    private fun containsAny(value: String, vararg needles: String): Boolean = needles.any(value::contains)

    private fun classify(state: DetectionState): Classification = when {
        state.sawCloudFrontEdge && state.sawAkamaiOrigin -> Classification.LIKELY_CLOUDFRONT_WITH_AKAMAI_ORIGIN
        state.cloudFrontScore >= 3 && state.akamaiScore >= 3 -> Classification.AMBIGUOUS
        state.cloudFrontScore >= 3 -> Classification.LIKELY_CLOUDFRONT
        state.akamaiScore >= 3 -> Classification.LIKELY_AKAMAI
        else -> Classification.UNKNOWN
    }

    private fun collectDnsEvidence(host: String): List<String> {
        if (host.isBlank()) {
            return emptyList()
        }

        val evidence = mutableListOf<String>()
        val environment = Hashtable<String, String>().apply {
            put("java.naming.factory.initial", "com.sun.jndi.dns.DnsContextFactory")
            put("com.sun.jndi.dns.timeout.initial", "3000")
            put("com.sun.jndi.dns.timeout.retries", "1")
        }

        var context: DirContext? = null
        try {
            context = InitialDirContext(environment)
            collectAttributes(context, host, evidence, "CNAME", "A", "AAAA")
        } catch (_: NamingException) {
            return evidence.toList()
        } finally {
            try {
                context?.close()
            } catch (_: NamingException) {
                // Ignore close errors for best-effort detection.
            }
        }

        return evidence.toList()
    }

    private fun collectAttributes(context: DirContext, host: String, evidence: MutableList<String>, vararg types: String) {
        try {
            val attributes = context.getAttributes(host, types)
            for (type in types) {
                collectAttributeValues(attributes, type, evidence)
            }
        } catch (_: NamingException) {
            // Missing DNS records are fine, detection stays heuristic.
        }
    }

    private fun collectAttributeValues(attributes: Attributes, type: String, evidence: MutableList<String>) {
        val attribute = attributes.get(type) ?: return
        val values: NamingEnumeration<*> = attribute.all
        while (values.hasMore()) {
            evidence += values.next().toString()
        }
    }

    private fun collectHeaderEvidence(input: String?): List<String> {
        if (input.isNullOrBlank()) {
            return emptyList()
        }

        val request = Request.Builder()
            .url(normalizeUrl(input))
            .head()
            .header("User-Agent", userAgent())
            .build()

        return runCatching {
            MVHttpClient.httpClient.newCall(request).execute().use { response ->
                buildList {
                    for (headerName in response.headers.names()) {
                        for (value in response.headers.values(headerName)) {
                            add("$headerName: $value")
                        }
                    }
                }
            }
        }.getOrDefault(emptyList())
    }

    private fun normalizeUrl(input: String): String = if ("://" in input) input else "https://$input"

    private fun userAgent(): String =
        ApplicationConfiguration.getConfiguration().getString(ApplicationConfiguration.APPLICATION_USER_AGENT)

    enum class Classification {
        LIKELY_CLOUDFRONT,
        LIKELY_AKAMAI,
        LIKELY_CLOUDFRONT_WITH_AKAMAI_ORIGIN,
        AMBIGUOUS,
        UNKNOWN,
    }

    data class Result(
        val host: String,
        val classification: Classification,
        val evidence: List<String>,
    )

    private class DetectionState {
        var cloudFrontScore = 0
        var akamaiScore = 0
        var sawCloudFrontEdge = false
        var sawAkamaiOrigin = false
        val evidence = mutableListOf<String>()
    }
}
