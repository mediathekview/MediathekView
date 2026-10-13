package mediathek.controller.starter

import okhttp3.Response
import java.io.IOException

/** Prevents appending bytes from a different entity or an unexpected range. */
internal class DownloadRangeValidator {
    private var etag: String? = null

    fun rememberEntity(response: Response) {
        val currentEtag = response.header("ETag") ?: return
        val previousEtag = etag
        if (previousEtag != null && previousEtag != currentEtag) {
            throw IOException("Download entity changed during transfer")
        }
        etag = currentEtag
    }

    fun validate(
        response: Response,
        start: Long,
        end: Long? = null,
        total: Long? = null,
    ) {
        if (start > 0 || end != null) {
            if (response.code != 206) throw IOException("Server ignored download byte range")
            val match = CONTENT_RANGE.matchEntire(response.header("Content-Range").orEmpty())
                ?: throw IOException("Missing or invalid Content-Range")
            val actualStart = match.groupValues[1].toLongOrNull()
            val actualEnd = match.groupValues[2].toLongOrNull()
            val actualTotal = match.groupValues[3].toLongOrNull()
            if (actualStart != start || actualEnd == null || actualEnd < start ||
                (end != null && actualEnd != end) || (total != null && actualTotal != total)
            ) {
                throw IOException("Unexpected download byte range: ${response.header("Content-Range")}")
            }
        }
        rememberEntity(response)
    }

    private companion object {
        val CONTENT_RANGE = Regex("bytes (\\d+)-(\\d+)/(\\d+|\\*)")
    }
}
