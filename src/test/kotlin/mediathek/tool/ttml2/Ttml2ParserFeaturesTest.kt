package mediathek.tool.ttml2

import mediathek.tool.subtitles.Rgba
import mediathek.tool.subtitles.ttml2.SubRipHtmlExporter
import mediathek.tool.subtitles.ttml2.Ttml2Parser
import org.apache.logging.log4j.Level
import org.apache.logging.log4j.LogManager
import org.apache.logging.log4j.core.LogEvent
import org.apache.logging.log4j.core.Logger
import org.apache.logging.log4j.core.appender.AbstractAppender
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
        assertEquals(1, doc.cues.size)
        val cue = doc.cues.first()
        assertEquals(Duration.ofMillis(1000), cue.start)
        assertEquals(Duration.ofMillis(2500), cue.end)
        assertEquals("Hello", cue.plainText())
    }

    @Test
    fun correctsWholeHourOffsetWhenCuesOtherwiseFitFilmDuration() {
        val xml =
            "<tt xmlns=\"http://www.w3.org/ns/ttml\" xmlns:ttp=\"http://www.w3.org/ns/ttml#parameter\" ttp:timeBase=\"media\">" +
                "  <body><div>" +
                "    <p begin=\"10:00:08.040\" end=\"10:00:10.080\">Hello</p>" +
                "    <p begin=\"10:42:43.440\" end=\"10:42:47.360\">Bye</p>" +
                "  </div></body>" +
                "</tt>"
        val path = writeTtml(xml)
        val events = mutableListOf<LogEvent>()
        val appender = object : AbstractAppender("ttml-offset-test", null, null, false, emptyArray()) {
            override fun append(event: LogEvent) {
                events += event.toImmutable()
            }
        }
        val parserLogger = LogManager.getLogger(Ttml2Parser::class.java) as Logger
        appender.start()
        parserLogger.addAppender(appender)

        try {
            val doc = Ttml2Parser().parseAndCorrect(path, Duration.ofMinutes(44))

            assertEquals(Duration.ofSeconds(8, 40_000_000), doc.cues.first().start)
            assertEquals(Duration.ofSeconds(10, 80_000_000), doc.cues.first().end)
            val correctedXml = Files.readString(path)
            assertTrue(correctedXml.contains("begin=\"00:00:08.04\""))
            assertTrue(correctedXml.contains("end=\"00:42:47.36\""))
            assertTrue(events.any { it.level == Level.WARN && it.message.formattedMessage.contains("PT10H") })
        } finally {
            parserLogger.removeAppender(appender)
            appender.stop()
        }
    }

    @Test
    fun keepsTimingWhenCuesFitFilmDuration() {
        val xml =
            "<tt xmlns=\"http://www.w3.org/ns/ttml\">" +
                "  <body><div>" +
                "    <p begin=\"01:00:08.040\" end=\"01:00:10.080\">Hello</p>" +
                "  </div></body>" +
                "</tt>"
        val path = writeTtml(xml)

        val doc = Ttml2Parser().parseAndCorrect(path, Duration.ofMinutes(90))

        assertEquals(Duration.ofHours(1).plusMillis(8040), doc.cues.single().start)
        assertTrue(Files.readString(path).contains("begin=\"01:00:08.040\""))
    }

    @Test
    fun keepsClockBasedTimingEvenWhenItExceedsFilmDuration() {
        val xml =
            "<tt xmlns=\"http://www.w3.org/ns/ttml\" xmlns:ttp=\"http://www.w3.org/ns/ttml#parameter\" ttp:timeBase=\"clock\">" +
                "  <body><div>" +
                "    <p begin=\"10:00:08.040\" end=\"10:00:10.080\">Hello</p>" +
                "  </div></body>" +
                "</tt>"
        val path = writeTtml(xml)

        val doc = Ttml2Parser().parseAndCorrect(path, Duration.ofMinutes(44))

        assertEquals(Duration.ofHours(10).plusMillis(8040), doc.cues.single().start)
        assertTrue(Files.readString(path).contains("begin=\"10:00:08.040\""))
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
        val runs = doc.cues.first().runs
        assertTrue(runs.any { run -> run.text.contains("B") && run.style.bold })
        assertTrue(runs.any { run -> run.text.contains("I") && run.style.italic })
        assertTrue(runs.any { run -> run.text.contains("U") && run.style.underline })
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
        val runs = doc.cues.first().runs
        assertTrue(runs.any { run -> run.text.contains("R") && Rgba(255, 0, 0, 255) == run.style.color })
        assertTrue(runs.any { run -> run.text.contains("G") && Rgba(0, 255, 0, 255) == run.style.color })
        assertTrue(runs.any { run -> run.text.contains("B") && Rgba(0, 0, 255, 128) == run.style.color })
        assertTrue(runs.any { run -> run.text.contains("W") && Rgba(255, 255, 255, 255) == run.style.color })
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
        val concat = doc.cues.first().plainText()
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
