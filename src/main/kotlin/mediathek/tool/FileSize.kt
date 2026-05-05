package mediathek.tool

import kotlinx.coroutines.runBlocking
import kotlinx.serialization.Serializable
import kotlinx.serialization.json.Json
import mediathek.config.Konstanten
import mediathek.tool.http.MVHttpClient
import okhttp3.HttpUrl
import okhttp3.HttpUrl.Companion.toHttpUrlOrNull
import okhttp3.Request
import okhttp3.Response
import org.apache.logging.log4j.LogManager
import java.io.IOException
import java.net.UnknownHostException

object FileSize {
    @Serializable
    private data class CachedHlsLookupResponse(
        val found: Boolean,
        val fileSize: Long? = null,
        val httpStatus: Int? = null,
        val resolutionUrl: String? = null,
        val quality: String? = null,
    )

    data class LookupResult(
        val byteLength: Long,
        val httpStatusCode: Int? = null,
        val resolutionUrl: HttpUrl? = null,
        val quality: String? = null,
    ) {
        val sizeText: String
            get() = convertSize(byteLength)
    }

    data class HlsLookupResult(
        val byteLength: Long,
        val resolutionUrl: HttpUrl?,
    )

    class HttpStatusException(
        val statusCode: Int,
        val requestUrl: HttpUrl,
    ) : IOException("HTTP $statusCode for $requestUrl")

    const val ONE_MiB = 1_000_000
    const val INVALID_SIZE: Byte = -1
    private val logger = LogManager.getLogger()
    private val lookupJson = Json { ignoreUnknownKeys = true }

    fun getFileLengthFromUrl(url: String, forceFetch: Boolean = false): String {
        return lookupFileSize(url, forceFetch).sizeText
    }

    @JvmStatic
    fun lookupFileSize(url: String, forceFetch: Boolean = false): LookupResult {
        return lookupFileSize(url, forceFetch, null)
    }

    @JvmStatic
    fun lookupFileSize(url: String, forceFetch: Boolean = false, quality: String?): LookupResult {
        val okUrl = url.toHttpUrlOrNull() ?: return LookupResult(INVALID_SIZE.toLong())
        return lookupFileSize(okUrl, forceFetch, quality)
    }

    @JvmStatic
    fun convertSize(byteLength: Long): String {
        return when {
            byteLength > ONE_MiB -> (byteLength / ONE_MiB).toString()
            byteLength > 0 -> "1"
            else -> ""
        }
    }

    fun getContentLength(response: Response): Long {
        val sizeStr = response.headers["Content-Length"] ?: return INVALID_SIZE.toLong()
        return sizeStr.toLongOrNull() ?: INVALID_SIZE.toLong()
    }

    fun getFileSizeFromUrl(url: HttpUrl, forceFetch: Boolean = false): Long {
        return lookupFileSize(url, forceFetch).byteLength
    }

    fun lookupFileSize(url: HttpUrl, forceFetch: Boolean = false): LookupResult {
        return lookupFileSize(url, forceFetch, null)
    }

    fun lookupFileSize(url: HttpUrl, forceFetch: Boolean = false, quality: String?): LookupResult {
        return lookupFileSize(
            url = url,
            forceFetch = forceFetch,
            quality = quality,
            directSizeLoader = ::loadDirectFileSize,
            hlsSizeLoader = ::loadHlsFileSize,
        )
    }

    internal fun lookupFileSize(
        url: HttpUrl,
        forceFetch: Boolean = false,
        quality: String?,
        cachedHlsLookup: (HttpUrl, String?) -> LookupResult? = ::lookupCachedHlsResult,
        directSizeLoader: (HttpUrl) -> Long,
        hlsSizeLoader: (HttpUrl) -> HlsLookupResult,
        hlsLookupLogger: (HttpUrl, LookupResult) -> LookupResult = ::logHlsLookupIfNeeded,
    ): LookupResult {
        if (!url.scheme.startsWith("http")) {
            return LookupResult(INVALID_SIZE.toLong())
        }

        val fetchSize = forceFetch || ApplicationConfiguration.getConfiguration()
            .getBoolean(ApplicationConfiguration.DOWNLOAD_FETCH_FILE_SIZE, true)
        if (fetchSize) {
            logger.info("Requesting file size for: {}", url)
        } else {
            logger.info("Skipping file size request due to user setting")
            return LookupResult(INVALID_SIZE.toLong())
        }

        val result = try {
            if (url.encodedPath.endsWith(".m3u8")) {
                HlsEgressPolicy.requirePublicHttpUrl(url)
                cachedHlsLookup(url, quality)?.let { return it }
                hlsSizeLoader(url)
            } else {
                HlsLookupResult(
                    byteLength = directSizeLoader(url),
                    resolutionUrl = null,
                )
            }
        } catch (exception: HttpStatusException) {
            logger.debug("File size lookup failed for {} with HTTP {}", url, exception.statusCode)
            return hlsLookupLogger(
                url,
                LookupResult(
                    byteLength = INVALID_SIZE.toLong(),
                    httpStatusCode = exception.statusCode,
                    resolutionUrl = exception.requestUrl,
                    quality = quality,
                ),
            )
        } catch (exception: IOException) {
            logLookupFailure(url, exception)
            return hlsLookupLogger(url, LookupResult(INVALID_SIZE.toLong()))
        } catch (exception: RuntimeException) {
            logger.debug("File size lookup failed for {}", url, exception)
            return hlsLookupLogger(url, LookupResult(INVALID_SIZE.toLong()))
        }

        val lookupResult = if (result.byteLength < ONE_MiB) {
            logger.debug("File size lookup for {} was below threshold: {}", url, result.byteLength)
            LookupResult(
                byteLength = INVALID_SIZE.toLong(),
                resolutionUrl = result.resolutionUrl,
                quality = quality,
            )
        } else {
            LookupResult(
                byteLength = result.byteLength,
                resolutionUrl = result.resolutionUrl,
                quality = quality,
            )
        }

        return hlsLookupLogger(url, lookupResult)
    }

    private fun logLookupFailure(url: HttpUrl, exception: IOException) {
        if (exception is UnknownHostException) {
            logger.debug("File size lookup failed for {}: unknown host ({})", url, exception.message)
        } else {
            logger.debug("File size lookup failed for {}", url, exception)
        }
    }

    private fun lookupCachedHlsResult(url: HttpUrl, quality: String?): LookupResult? {
        val lookupUrl = lookupEndpoint ?: return null
        val request = Request.Builder()
            .url(
                lookupUrl.newBuilder()
                    .addQueryParameter("m3u8Url", url.toString())
                    .addQueryParameter("country", ApplicationConfiguration.getInstance().geographicLocation.name)
                    .apply {
                        if (!quality.isNullOrBlank()) {
                            addQueryParameter("quality", quality)
                        }
                    }
                    .build(),
            )
            .header(
                "User-Agent",
                ApplicationConfiguration.getConfiguration().getString(
                    ApplicationConfiguration.APPLICATION_USER_AGENT,
                    Konstanten.PROGRAMMNAME,
                ),
            )
            .header(Konstanten.HLS_STREAM_INFO_TOKEN_HEADER, Konstanten.HLS_STREAM_INFO_TOKEN)
            .get()
            .build()

        return runCatching {
            MVHttpClient.httpClient.newCall(request).execute().use { response ->
                if (!response.isSuccessful) {
                    logger.debug("HLS stream info lookup failed for {} with HTTP {}", url, response.code)
                    return null
                }

                val payload = response.body.string()
                val cachedResult = lookupJson.decodeFromString<CachedHlsLookupResponse>(payload)
                if (!cachedResult.found) {
                    return null
                }

                logger.info("Using cached HLS stream info for {} and quality {}", url, quality)
                LookupResult(
                    byteLength = cachedResult.fileSize ?: INVALID_SIZE.toLong(),
                    httpStatusCode = cachedResult.httpStatus,
                    resolutionUrl = cachedResult.resolutionUrl?.toHttpUrlOrNull(),
                    quality = cachedResult.quality ?: quality,
                )
            }
        }.getOrElse { exception ->
            logger.debug("HLS stream info lookup failed for {}", url, exception)
            null
        }
    }

    private fun loadDirectFileSize(url: HttpUrl): Long {
        val request = Request.Builder().url(url).head().build()
        MVHttpClient.httpClient.newCall(request).execute().use { response ->
            if (response.isSuccessful) {
                return getContentLength(response)
            }
            throw HttpStatusException(response.code, url)
        }
    }

    private fun loadHlsFileSize(url: HttpUrl): HlsLookupResult = runBlocking {
        val estimate = HlsPlaylistSizeEstimator().estimate(url.toString())
        HlsLookupResult(
            byteLength = estimate.totalBytes,
            resolutionUrl = estimate.selectedVariant.playlistUrl,
        )
    }

    private fun logHlsLookupIfNeeded(url: HttpUrl, lookupResult: LookupResult): LookupResult {
        if (url.encodedPath.endsWith(".m3u8")) {
            if (lookupResult.byteLength == INVALID_SIZE.toLong() && lookupResult.httpStatusCode !in setOf(403, 404)) {
                return lookupResult
            }
            HlsStreamInfoLogger.appendEntry(
                httpStatusCode = lookupResult.httpStatusCode ?: 200,
                m3u8Url = url,
                resolutionUrl = lookupResult.resolutionUrl,
                quality = lookupResult.quality,
                fileSize = lookupResult.byteLength,
            )
        }
        return lookupResult
    }

    private val lookupEndpoint: HttpUrl?
        get() = Konstanten.HLS_STREAM_INFO_UPLOAD_URL?.newBuilder()?.addPathSegment("lookup")?.build()
}
