package mediathek.tool.ttml2;

import mediathek.tool.subtitles.ttml2.SubtitleDocument;
import mediathek.tool.subtitles.ttml2.Ttml2Parser;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import java.nio.file.Files;
import java.nio.file.Path;
import java.time.Duration;

import static org.junit.jupiter.api.Assertions.assertEquals;

public class SeqContainerTest {

    @TempDir
    Path tmp;

    private Path writeTtml(String xml) throws Exception {
        Path p = tmp.resolve("seq.ttml");
        Files.writeString(p, xml);
        return p;
    }

    @Test
    void seqWithoutBeginUsesCursor() throws Exception {
        String xml =
                "<tt xmlns=\"http://www.w3.org/ns/ttml\">" +
                "  <body>" +
                "    <div timeContainer=\"seq\">" +
                "      <p dur=\"1s\">A</p>" +
                "      <p dur=\"2s\">B</p>" +
                "    </div>" +
                "  </body>" +
                "</tt>";
        SubtitleDocument doc = new Ttml2Parser().parse(writeTtml(xml));
        assertEquals(2, doc.cues().size());
        assertEquals(Duration.ZERO, doc.cues().get(0).start());
        assertEquals(Duration.ofSeconds(1), doc.cues().get(0).end());
        assertEquals(Duration.ofSeconds(1), doc.cues().get(1).start());
        assertEquals(Duration.ofSeconds(3), doc.cues().get(1).end());
    }
}
