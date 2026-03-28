package mediathek.tool.ttml2;

import mediathek.tool.subtitles.ttml2.Rgba;
import mediathek.tool.subtitles.ttml2.Ttml2Color;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;

public class Ttml2ColorTest {

    @Test
    void parsesHexRgb() {
        assertEquals(new Rgba(255, 0, 16, 255), Ttml2Color.parse("#FF0010"));
    }

    @Test
    void parsesHexRgba() {
        assertEquals(new Rgba(1, 2, 3, 4), Ttml2Color.parse("#01020304"));
    }

    @Test
    void parsesRgbFunc() {
        assertEquals(new Rgba(10, 20, 30, 255), Ttml2Color.parse("rgb(10,20,30)"));
        assertEquals(new Rgba(10, 20, 30, 255), Ttml2Color.parse("RGB( 10 , 20 , 30 )"));
    }

    @Test
    void parsesRgbaFunc() {
        assertEquals(new Rgba(10, 20, 30, 40), Ttml2Color.parse("rgba(10,20,30,40)"));
    }

    @Test
    void parsesNamedColorsCaseInsensitive() {
        assertEquals(new Rgba(255, 255, 255, 255), Ttml2Color.parse("WHITE"));
        assertEquals(new Rgba(0, 0, 0, 0), Ttml2Color.parse("transparent"));
        assertEquals(new Rgba(0, 255, 255, 255), Ttml2Color.parse("Cyan"));
    }

    @Test
    void rejectsOutOfRangeComponents() {
        assertThrows(IllegalArgumentException.class, () -> Ttml2Color.parse("rgb(256,0,0)"));
        assertThrows(IllegalArgumentException.class, () -> Ttml2Color.parse("rgba(0,0,0,-1)"));
    }

    @Test
    void rejectsUnknownLexicalForms() {
        assertThrows(IllegalArgumentException.class, () -> Ttml2Color.parse("#123"));
        assertThrows(IllegalArgumentException.class, () -> Ttml2Color.parse("hsl(0,0,0)"));
        assertThrows(IllegalArgumentException.class, () -> Ttml2Color.parse("notacolor"));
    }
}
