package mediathek.tool.ttml2;

import mediathek.tool.subtitles.ttml2.Rgba;
import mediathek.tool.subtitles.ttml2.SubRipHtmlExporter;
import mediathek.tool.subtitles.ttml2.SubtitleDocument;
import mediathek.tool.subtitles.ttml2.Ttml2Parser;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import java.nio.file.Files;
import java.nio.file.Path;
import java.time.Duration;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

public class Ttml2ParserFeaturesTest {

    @TempDir
    Path tmp;

    private Path writeTtml(String xml) throws Exception {
        Path p = tmp.resolve("in.ttml");
        Files.writeString(p, xml);
        return p;
    }

    @Test
    void parsesTimingBeginEnd() throws Exception {
        String xml =
                "<tt xmlns=\"http://www.w3.org/ns/ttml\">" +
                "  <body><div>" +
                "    <p begin=\"00:00:01.000\" end=\"00:00:02.500\">Hello</p>" +
                "  </div></body>" +
                "</tt>";
        SubtitleDocument doc = new Ttml2Parser().parse(writeTtml(xml));
        assertEquals(1, doc.cues().size());
        SubtitleDocument.Cue c = doc.cues().getFirst();
        assertEquals(Duration.ofMillis(1000), c.start());
        assertEquals(Duration.ofMillis(2500), c.end());
        assertEquals("Hello", c.plainText());
    }

    @Test
    void parsesMixedSpansBoldItalicUnderline() throws Exception {
        String xml =
                "<tt xmlns=\"http://www.w3.org/ns/ttml\" xmlns:tts=\"http://www.w3.org/ns/ttml#styling\">" +
                "  <body><div>" +
                "    <p begin=\"0s\" dur=\"2s\">" +
                "      normal <span tts:fontWeight=\"bold\">B</span>" +
                "      <span tts:fontStyle=\"italic\">I</span>" +
                "      <span tts:textDecoration=\"underline\">U</span>" +
                "    </p>" +
                "  </div></body>" +
                "</tt>";
        SubtitleDocument doc = new Ttml2Parser().parse(writeTtml(xml));
        List<SubtitleDocument.StyledRun> runs = doc.cues().getFirst().runs();
        assertTrue(runs.stream().anyMatch(r -> r.text().contains("B") && r.style().bold()));
        assertTrue(runs.stream().anyMatch(r -> r.text().contains("I") && r.style().italic()));
        assertTrue(runs.stream().anyMatch(r -> r.text().contains("U") && r.style().underline()));
    }

    @Test
    void parsesColorsAllLexicalForms() throws Exception {
        String xml =
                "<tt xmlns=\"http://www.w3.org/ns/ttml\" xmlns:tts=\"http://www.w3.org/ns/ttml#styling\">" +
                "  <body><div>" +
                "    <p begin=\"0s\" dur=\"2s\">" +
                "      <span tts:color=\"#FF0000\">R</span>" +
                "      <span tts:color=\"rgb(0,255,0)\">G</span>" +
                "      <span tts:color=\"rgba(0,0,255,128)\">B</span>" +
                "      <span tts:color=\"white\">W</span>" +
                "    </p>" +
                "  </div></body>" +
                "</tt>";
        SubtitleDocument doc = new Ttml2Parser().parse(writeTtml(xml));
        var runs = doc.cues().getFirst().runs();
        assertTrue(runs.stream().anyMatch(r -> r.text().contains("R") && new Rgba(255,0,0,255).equals(r.style().color())));
        assertTrue(runs.stream().anyMatch(r -> r.text().contains("G") && new Rgba(0,255,0,255).equals(r.style().color())));
        assertTrue(runs.stream().anyMatch(r -> r.text().contains("B") && new Rgba(0,0,255,128).equals(r.style().color())));
        assertTrue(runs.stream().anyMatch(r -> r.text().contains("W") && new Rgba(255,255,255,255).equals(r.style().color())));
    }

    @Test
    void parsesBrAsNewlineSentinel() throws Exception {
        String xml =
                "<tt xmlns=\"http://www.w3.org/ns/ttml\">" +
                "  <body><div>" +
                "    <p begin=\"0s\" dur=\"1s\">A<br/>B</p>" +
                "  </div></body>" +
                "</tt>";
        SubtitleDocument doc = new Ttml2Parser().parse(writeTtml(xml));
        String concat = doc.cues().getFirst().plainText();
        assertTrue(concat.contains("A") && concat.contains("B"));
    }
    @Test
    void parsesEscapedBrAsNewline() throws Exception {
        String xml =
                "<tt xmlns=\"http://www.w3.org/ns/ttml\">" +
                "  <body><div>" +
                "    <p begin=\"0s\" dur=\"1s\">A &lt;br/&gt; B</p>" +
                "  </div></body>" +
                "</tt>";
        SubtitleDocument doc = new Ttml2Parser().parse(writeTtml(xml));
        String srt = new SubRipHtmlExporter().export(doc);
        // Expect a real line break in the rendered SRT text
        assertTrue(srt.contains("A\nB") || srt.contains("A\n B") || srt.contains("A\r\nB") || srt.contains("A\r\n B"));
    }

}
