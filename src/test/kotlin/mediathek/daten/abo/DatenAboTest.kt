package mediathek.daten.abo

import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Assertions.assertThrows
import org.junit.jupiter.api.Test
import org.junit.jupiter.params.ParameterizedTest
import org.junit.jupiter.params.provider.Arguments
import org.junit.jupiter.params.provider.MethodSource
import java.lang.reflect.Proxy
import java.util.stream.Stream
import javax.xml.stream.XMLStreamException
import javax.xml.stream.XMLStreamWriter

class DatenAboTest {

    @Test
    fun `string fields default to empty text`() {
        val abo = DatenAbo()

        assertEquals("", abo.name)
        assertEquals("", abo.sender)
        assertEquals("", abo.thema)
        assertEquals("", abo.title)
        assertEquals("", abo.themaTitel)
        assertEquals("", abo.irgendwo)
        assertEquals("", abo.zielpfad)
        assertEquals("", abo.psetName)
    }

    @Test
    fun `minimum duration clamps negative values to zero`() {
        val abo = DatenAbo()

        abo.mindestDauerMinuten = -1

        assertEquals(0, abo.mindestDauerMinuten)
    }

    @Test
    fun `write config propagates xml writer failures`() {
        val writer = failingXmlWriter()

        assertThrows(XMLStreamException::class.java) {
            DatenAbo().writeToConfig(writer)
        }
    }

    @ParameterizedTest
    @MethodSource("filterValidationCases")
    fun `invalid filter helper matches legacy empty-field validation`(
        sender: String,
        thema: String,
        title: String,
        themaTitel: String,
        irgendwo: String,
    ) {
        val oldValidationResult = sender.isEmpty() &&
            thema.isEmpty() &&
            title.isEmpty() &&
            themaTitel.isEmpty() &&
            irgendwo.isEmpty()
        val abo = DatenAbo().apply {
            this.sender = sender
            this.thema = thema
            this.title = title
            this.themaTitel = themaTitel
            this.irgendwo = irgendwo
        }

        assertEquals(oldValidationResult, DatenAbo.isInvalidFilter(sender, thema, title, themaTitel, irgendwo))
        assertEquals(oldValidationResult, abo.isInvalid)
    }

    private companion object {
        fun failingXmlWriter(): XMLStreamWriter =
            Proxy.newProxyInstance(
                XMLStreamWriter::class.java.classLoader,
                arrayOf(XMLStreamWriter::class.java),
            ) { _, method, _ ->
                if (method.name == "writeStartElement") {
                    throw XMLStreamException("write failed")
                }
                null
            } as XMLStreamWriter

        @JvmStatic
        fun filterValidationCases(): Stream<Arguments> =
            Stream.of(
                Arguments.of("", "", "", "", ""),
                Arguments.of("ARD", "", "", "", ""),
                Arguments.of("", "Nachrichten", "", "", ""),
                Arguments.of("", "", "Tagesschau", "", ""),
                Arguments.of("", "", "", "Nachrichten Tagesschau", ""),
                Arguments.of("", "", "", "", "Politik"),
                Arguments.of("ZDF", "Heute", "Journal", "Heute Journal", "Nachrichten"),
            )
    }
}
