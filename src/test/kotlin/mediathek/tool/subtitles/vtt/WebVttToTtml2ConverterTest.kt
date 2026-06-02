package mediathek.tool.subtitles.vtt

import mediathek.tool.subtitles.ttml2.Ttml2Parser
import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Assertions.assertThrows
import org.junit.jupiter.api.Assertions.assertTrue
import org.junit.jupiter.api.Test
import org.junit.jupiter.api.io.TempDir
import java.nio.file.Files
import java.nio.file.Path
import java.time.Duration

internal class WebVttToTtml2ConverterTest {
    @TempDir
    lateinit var tmp: Path

    @Test
    fun convertsCueTimingSettingsAndInlineMarkup() {
        val webVtt = tmp.resolve("subtitle.vtt")
        val ttml = tmp.resolve("subtitle.ttml")
        Files.writeString(
            webVtt,
            """
            WEBVTT

            1
            00:00:01.000 --> 00:00:02.500 align:middle
            <b>Bold</b>
            <c.textYellow>Yellow</c> & more <00:00:01.500>
            """.trimIndent(),
        )

        val converter = WebVttToTtml2Converter()
        val xml = converter.convertToString(webVtt)
        Files.writeString(ttml, xml)

        val wrapperOutput = tmp.resolve("wrapper.ttml")
        converter.convert(webVtt, wrapperOutput)
        assertEquals(xml, Files.readString(wrapperOutput))

        assertTrue(xml.contains("xml:id=\"c1\""))
        assertTrue(xml.contains("begin=\"00:00:01.000\""))
        assertTrue(xml.contains("end=\"00:00:02.500\""))
        assertTrue(xml.contains("tts:textAlign=\"center\""))
        assertTrue(xml.contains("<span tts:fontWeight=\"bold\">Bold</span><br/>"))
        assertTrue(xml.contains("<span style=\"cTextYellow\">Yellow</span> &amp; more "))
        assertTrue(!xml.contains("00:00:01.500"))

        val doc = Ttml2Parser().parse(ttml)
        val cue = doc.cues.single()
        assertEquals(Duration.ofSeconds(1), cue.start)
        assertEquals(Duration.ofMillis(2500), cue.end)
        assertEquals("Bold\\nYellow& more", cue.plainText())
    }

    @Test
    fun rejectsInvalidHeader() {
        val webVtt = tmp.resolve("invalid.vtt")
        val ttml = tmp.resolve("invalid.ttml")
        Files.writeString(webVtt, "not webvtt\n")

        assertThrows(WebVttToTtml2Converter.VttParseException::class.java) {
            WebVttToTtml2Converter().convert(webVtt, ttml)
        }
    }
}
