package mediathek.tool.subtitles.detector

import org.junit.jupiter.api.Assertions.*
import org.junit.jupiter.api.Test
import org.junit.jupiter.api.io.TempDir
import java.nio.file.Files
import java.nio.file.Path

internal class WebVttStrictValidatorTest {
    @TempDir
    lateinit var tempDir: Path

    @Test
    fun acceptsValidCueWithMiddleAlignAlias() {
        val result = validate(
            """
            WEBVTT

            00:00.000 --> 00:01.000 align:middle position:50% line:90% size:80%
            Hello
            """.trimIndent()
        )

        assertTrue(result.headerPresent)
        assertTrue(result.valid)
        assertEquals(1, result.cueCount)
        assertTrue(result.errors.isEmpty())
    }

    @Test
    fun rejectsMissingHeader() {
        val result = validate(
            """
            00:00.000 --> 00:01.000
            Hello
            """.trimIndent()
        )

        assertFalse(result.headerPresent)
        assertFalse(result.valid)
        assertEquals(0, result.cueCount)
        assertTrue(result.errors.any { it.contains("WEBVTT header") })
    }

    @Test
    fun rejectsHeaderOnlyFileWhenCueIsRequired() {
        val result = validate(
            """
            WEBVTT

            NOTE no cue here
            """.trimIndent()
        )

        assertTrue(result.headerPresent)
        assertFalse(result.valid)
        assertEquals(0, result.cueCount)
        assertTrue(result.errors.any { it.contains("No cues found") })
    }

    @Test
    fun allowsHeaderOnlyFileWhenCueIsNotRequired() {
        val result = validate(
            """
            WEBVTT

            NOTE no cue here
            """.trimIndent(),
            requireAtLeastOneCue = false
        )

        assertTrue(result.headerPresent)
        assertTrue(result.valid)
        assertEquals(0, result.cueCount)
        assertTrue(result.errors.isEmpty())
    }

    @Test
    fun rejectsUnknownCueSetting() {
        val result = validate(
            """
            WEBVTT

            00:00.000 --> 00:01.000 unexpected:value
            Hello
            """.trimIndent()
        )

        assertFalse(result.valid)
        assertEquals(1, result.cueCount)
        assertTrue(result.errors.any { it.contains("Unknown cue setting key 'unexpected'") })
    }

    @Test
    fun rejectsCueEndBeforeStart() {
        val result = validate(
            """
            WEBVTT

            00:02.000 --> 00:01.000
            Hello
            """.trimIndent()
        )

        assertFalse(result.valid)
        assertEquals(0, result.cueCount)
        assertTrue(result.errors.any { it.contains("Cue end must be > start") })
    }

    private fun validate(
        content: String,
        requireAtLeastOneCue: Boolean = true
    ): WebVttStrictValidator.ValidationResult {
        val path = tempDir.resolve("subtitle.vtt")
        Files.writeString(path, content)
        return WebVttStrictValidator.validate(path, requireAtLeastOneCue)
    }
}
