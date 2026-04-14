package mediathek.tool.ttml2

import mediathek.tool.subtitles.ttml2.AssExporter
import mediathek.tool.subtitles.ttml2.SubRipHtmlExporter
import mediathek.tool.subtitles.ttml2.Ttml2Parser
import org.junit.jupiter.api.Assertions.assertTrue
import org.junit.jupiter.api.Test
import org.junit.jupiter.api.io.TempDir
import java.nio.file.Files
import java.nio.file.Path

internal class ExportersTest {
    @TempDir
    lateinit var tmp: Path

    private fun writeTtml(xml: String): Path =
        tmp.resolve("in.ttml").also { path ->
            Files.writeString(path, xml)
        }

    @Test
    fun exportsSrtHtmlWithStyleTagsAndColor() {
        val xml =
            "<tt xmlns=\"http://www.w3.org/ns/ttml\" xmlns:tts=\"http://www.w3.org/ns/ttml#styling\">" +
                "  <body><div>" +
                "    <p begin=\"0s\" dur=\"2s\">" +
                "      <span tts:fontWeight=\"bold\" tts:color=\"#00FF00\">Hi</span>" +
                "    </p>" +
                "  </div></body>" +
                "</tt>"
        val doc = Ttml2Parser().parse(writeTtml(xml))
        val srt = SubRipHtmlExporter().export(doc)

        assertTrue(srt.contains("<b>") && srt.contains("</b>"))
        assertTrue(srt.contains("<font color=\"#00FF00\">") && srt.contains("</font>"))
        assertTrue(srt.contains("00:00:00,000 --> 00:00:02,000"))
    }

    @Test
    fun exportsAssWithRunOverridesAndAlpha() {
        val xml =
            "<tt xmlns=\"http://www.w3.org/ns/ttml\" xmlns:tts=\"http://www.w3.org/ns/ttml#styling\">" +
                "  <body><div>" +
                "    <p begin=\"0s\" dur=\"2s\">" +
                "      <span tts:color=\"rgba(255,0,0,128)\" tts:fontStyle=\"italic\">X</span>" +
                "    </p>" +
                "  </div></body>" +
                "</tt>"
        val doc = Ttml2Parser().parse(writeTtml(xml))
        val ass = AssExporter(AssExporter.Options(384, 288, false)).export(doc)

        assertTrue(ass.contains("\\\\1c&H0000FF&") || ass.contains("\\1c&H0000FF&"))
        assertTrue(ass.contains("\\\\1a&H7F&") || ass.contains("\\1a&H7F&"))
        assertTrue(ass.contains("\\\\i1") || ass.contains("\\i1"))
    }

    @Test
    fun exportsAssWithRegionPositioning() {
        val xml =
            "<tt xmlns=\"http://www.w3.org/ns/ttml\" xmlns:tts=\"http://www.w3.org/ns/ttml#styling\" xmlns:xml=\"http://www.w3.org/XML/1998/namespace\">" +
                "  <head><layout>" +
                "    <region xml:id=\"r1\" tts:origin=\"10% 20%\" tts:extent=\"50% 40%\" tts:textAlign=\"center\" tts:displayAlign=\"after\"/>" +
                "  </layout></head>" +
                "  <body region=\"r1\"><div>" +
                "    <p begin=\"0s\" dur=\"1s\">Hello</p>" +
                "  </div></body>" +
                "</tt>"
        val doc = Ttml2Parser().parse(writeTtml(xml))
        val ass = AssExporter(AssExporter.Options(1000, 500, false)).export(doc)

        assertTrue(ass.contains("\\\\pos(350,300)") || ass.contains("\\pos(350,300)"))
        assertTrue(ass.contains("\\\\an2") || ass.contains("\\an2"))
    }

    @Test
    fun exportsBrElementAsNewlineInSrtAndAss() {
        val xml =
            "<tt:tt xmlns:tt=\"http://www.w3.org/ns/ttml\" xmlns:tts=\"http://www.w3.org/ns/ttml#styling\">" +
                "  <tt:body><tt:div>" +
                "    <tt:p begin=\"0s\" dur=\"1s\">A<tt:br />B</tt:p>" +
                "  </tt:div></tt:body>" +
                "</tt:tt>"
        val doc = Ttml2Parser().parse(writeTtml(xml))

        val srt = SubRipHtmlExporter().export(doc)
        assertTrue(srt.contains("A\nB") || srt.contains("A\r\nB"))

        val ass = AssExporter(AssExporter.Options(384, 288, false)).export(doc)
        assertTrue(ass.contains("A\\\\NB") || ass.contains("A\\NB"))
    }
}
