package mediathek.tool.ttml2

import mediathek.tool.subtitles.ttml2.Ttml2Parser
import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Test
import org.junit.jupiter.api.io.TempDir
import java.nio.file.Files
import java.nio.file.Path
import java.time.Duration

internal class SeqContainerTest {
    @TempDir
    lateinit var tmp: Path

    private fun writeTtml(xml: String): Path =
        tmp.resolve("seq.ttml").also { path ->
            Files.writeString(path, xml)
        }

    @Test
    fun seqWithoutBeginUsesCursor() {
        val xml =
            "<tt xmlns=\"http://www.w3.org/ns/ttml\">" +
                "  <body>" +
                "    <div timeContainer=\"seq\">" +
                "      <p dur=\"1s\">A</p>" +
                "      <p dur=\"2s\">B</p>" +
                "    </div>" +
                "  </body>" +
                "</tt>"
        val doc = Ttml2Parser().parse(writeTtml(xml))
        assertEquals(2, doc.cues().size)
        assertEquals(Duration.ZERO, doc.cues()[0].start())
        assertEquals(Duration.ofSeconds(1), doc.cues()[0].end())
        assertEquals(Duration.ofSeconds(1), doc.cues()[1].start())
        assertEquals(Duration.ofSeconds(3), doc.cues()[1].end())
    }
}
