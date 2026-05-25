package mediathek.tool.ttml2

import mediathek.tool.subtitles.ttml2.Rgba
import mediathek.tool.subtitles.ttml2.SubRipHtmlExporter
import mediathek.tool.subtitles.ttml2.Ttml2Parser
import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Assertions.assertTrue
import org.junit.jupiter.api.Test
import org.junit.jupiter.api.io.TempDir
import java.nio.file.Files
import java.nio.file.Path
import java.time.Duration

internal class Ttml2ParserFeaturesTest {
    @TempDir
    lateinit var tmp: Path

    private fun writeTtml(xml: String): Path =
        tmp.resolve("in.ttml").also { path ->
            Files.writeString(path, xml)
        }

    @Test
    fun parsesTimingBeginEnd() {
        val xml =
            "<tt xmlns=\"http://www.w3.org/ns/ttml\">" +
                "  <body><div>" +
                "    <p begin=\"00:00:01.000\" end=\"00:00:02.500\">Hello</p>" +
                "  </div></body>" +
                "</tt>"
        val doc = Ttml2Parser().parse(writeTtml(xml))
        assertEquals(1, doc.cues().size)
        val cue = doc.cues().first()
        assertEquals(Duration.ofMillis(1000), cue.start())
        assertEquals(Duration.ofMillis(2500), cue.end())
        assertEquals("Hello", cue.plainText())
    }

    @Test
    fun parsesMixedSpansBoldItalicUnderline() {
        val xml =
            "<tt xmlns=\"http://www.w3.org/ns/ttml\" xmlns:tts=\"http://www.w3.org/ns/ttml#styling\">" +
                "  <body><div>" +
                "    <p begin=\"0s\" dur=\"2s\">" +
                "      normal <span tts:fontWeight=\"bold\">B</span>" +
                "      <span tts:fontStyle=\"italic\">I</span>" +
                "      <span tts:textDecoration=\"underline\">U</span>" +
                "    </p>" +
                "  </div></body>" +
                "</tt>"
        val doc = Ttml2Parser().parse(writeTtml(xml))
        val runs = doc.cues().first().runs()
        assertTrue(runs.any { run -> run.text().contains("B") && run.style().bold() })
        assertTrue(runs.any { run -> run.text().contains("I") && run.style().italic() })
        assertTrue(runs.any { run -> run.text().contains("U") && run.style().underline() })
    }

    @Test
    fun parsesColorsAllLexicalForms() {
        val xml =
            "<tt xmlns=\"http://www.w3.org/ns/ttml\" xmlns:tts=\"http://www.w3.org/ns/ttml#styling\">" +
                "  <body><div>" +
                "    <p begin=\"0s\" dur=\"2s\">" +
                "      <span tts:color=\"#FF0000\">R</span>" +
                "      <span tts:color=\"rgb(0,255,0)\">G</span>" +
                "      <span tts:color=\"rgba(0,0,255,128)\">B</span>" +
                "      <span tts:color=\"white\">W</span>" +
                "    </p>" +
                "  </div></body>" +
                "</tt>"
        val doc = Ttml2Parser().parse(writeTtml(xml))
        val runs = doc.cues().first().runs()
        assertTrue(runs.any { run -> run.text().contains("R") && Rgba(255, 0, 0, 255) == run.style().color() })
        assertTrue(runs.any { run -> run.text().contains("G") && Rgba(0, 255, 0, 255) == run.style().color() })
        assertTrue(runs.any { run -> run.text().contains("B") && Rgba(0, 0, 255, 128) == run.style().color() })
        assertTrue(runs.any { run -> run.text().contains("W") && Rgba(255, 255, 255, 255) == run.style().color() })
    }

    @Test
    fun parsesBrAsNewlineSentinel() {
        val xml =
            "<tt xmlns=\"http://www.w3.org/ns/ttml\">" +
                "  <body><div>" +
                "    <p begin=\"0s\" dur=\"1s\">A<br/>B</p>" +
                "  </div></body>" +
                "</tt>"
        val doc = Ttml2Parser().parse(writeTtml(xml))
        val concat = doc.cues().first().plainText()
        assertTrue(concat.contains("A") && concat.contains("B"))
    }

    @Test
    fun parsesEscapedBrAsNewline() {
        val xml =
            "<tt xmlns=\"http://www.w3.org/ns/ttml\">" +
                "  <body><div>" +
                "    <p begin=\"0s\" dur=\"1s\">A &lt;br/&gt; B</p>" +
                "  </div></body>" +
                "</tt>"
        val doc = Ttml2Parser().parse(writeTtml(xml))
        val srt = SubRipHtmlExporter().export(doc)

        assertTrue(srt.contains("A\r\nB") || srt.contains("A\r\n B"))
    }
}
