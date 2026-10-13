package mediathek.controller.starter

import okhttp3.Protocol
import okhttp3.Request
import okhttp3.Response
import org.junit.jupiter.api.Assertions.assertDoesNotThrow
import org.junit.jupiter.api.Assertions.assertThrows
import org.junit.jupiter.api.Test
import java.io.IOException

internal class DownloadRangeValidatorTest {
    @Test
    fun acceptsMatchingRangesAcrossResponses() {
        val validator = DownloadRangeValidator()
        validator.rememberEntity(response(200, etag = "same"))
        assertDoesNotThrow {
            validator.validate(response(206, "bytes 1024-2047/4096", "same"), 1024, 2047, 4096)
        }
    }

    @Test
    fun rejectsIgnoredRangeAndDifferentContent() {
        val validator = DownloadRangeValidator()
        validator.rememberEntity(response(200, etag = "old"))
        assertThrows(IOException::class.java) { validator.validate(response(200, etag = "old"), 1024) }
        assertThrows(IOException::class.java) {
            validator.validate(response(206, "bytes 0-1023/4096", "old"), 1024)
        }
        assertThrows(IOException::class.java) {
            validator.validate(response(206, "bytes 1024-2047/4096", "new"), 1024)
        }
    }

    private fun response(code: Int, contentRange: String? = null, etag: String? = null): Response =
        Response.Builder()
            .request(Request.Builder().url("https://example.test/video.mp4").build())
            .protocol(Protocol.HTTP_1_1)
            .code(code)
            .message("test")
            .apply {
                if (contentRange != null) header("Content-Range", contentRange)
                if (etag != null) header("ETag", etag)
            }
            .build()
}
