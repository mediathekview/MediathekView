package mediathek.tool.ttml2

import mediathek.tool.subtitles.Rgba
import mediathek.tool.subtitles.ttml2.Ttml2Color
import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Assertions.assertThrows
import org.junit.jupiter.api.Test

internal class Ttml2ColorTest {

    @Test
    fun parsesHexRgb() {
        assertEquals(Rgba(255, 0, 16, 255), Ttml2Color.parse("#FF0010"))
    }

    @Test
    fun parsesHexRgba() {
        assertEquals(Rgba(1, 2, 3, 4), Ttml2Color.parse("#01020304"))
    }

    @Test
    fun parsesRgbFunc() {
        assertEquals(Rgba(10, 20, 30, 255), Ttml2Color.parse("rgb(10,20,30)"))
        assertEquals(Rgba(10, 20, 30, 255), Ttml2Color.parse("RGB( 10 , 20 , 30 )"))
    }

    @Test
    fun parsesRgbaFunc() {
        assertEquals(Rgba(10, 20, 30, 40), Ttml2Color.parse("rgba(10,20,30,40)"))
    }

    @Test
    fun parsesNamedColorsCaseInsensitive() {
        assertEquals(Rgba(255, 255, 255, 255), Ttml2Color.parse("WHITE"))
        assertEquals(Rgba(0, 0, 0, 0), Ttml2Color.parse("transparent"))
        assertEquals(Rgba(0, 255, 255, 255), Ttml2Color.parse("Cyan"))
    }

    @Test
    fun rejectsOutOfRangeComponents() {
        assertThrows(IllegalArgumentException::class.java) { Ttml2Color.parse("rgb(256,0,0)") }
        assertThrows(IllegalArgumentException::class.java) { Ttml2Color.parse("rgba(0,0,0,-1)") }
    }

    @Test
    fun rejectsUnknownLexicalForms() {
        assertThrows(IllegalArgumentException::class.java) { Ttml2Color.parse("#123") }
        assertThrows(IllegalArgumentException::class.java) { Ttml2Color.parse("hsl(0,0,0)") }
        assertThrows(IllegalArgumentException::class.java) { Ttml2Color.parse("notacolor") }
    }
}
