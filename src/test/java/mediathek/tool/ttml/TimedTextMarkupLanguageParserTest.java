package mediathek.tool.ttml;

import mediathek.tool.ttml.parsers.EbuTimedTextMarkupLanguageParser;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertThrows;

class TimedTextMarkupLanguageParserTest {
    @Test
    void getSubtitleList() {
        try (TimedTextMarkupLanguageParser parser = new EbuTimedTextMarkupLanguageParser()) {
            var list =  parser.getSubtitleList();
            assertThrows(UnsupportedOperationException.class, () -> list.add(new Subtitle()));
        }}
}
