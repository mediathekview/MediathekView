package mediathek.daten.abo

import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.params.ParameterizedTest
import org.junit.jupiter.params.provider.Arguments
import org.junit.jupiter.params.provider.MethodSource
import java.util.stream.Stream

class DatenAboTest {

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
