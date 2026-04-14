package mediathek.tool

import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Test

internal class MVInfoFileTest {

    @Test
    fun splitDescriptionTextIntoOneLine() {
        val infoFile = TestableMVInfoFile()
        assertEquals(
            "The Big Brown Fox Jumps over the Lazy Dog",
            infoFile.callSplitStringIntoMaxFixedLengthLines("The Big Brown Fox Jumps over the Lazy Dog", 50),
        )
    }

    @Test
    fun splitDescriptionTextIntoMore() {
        val result =
            "Weit hinten, hinter den Wortbergen, fern der Länder Vokalien${System.lineSeparator()}" +
                "und Konsonantien leben die Blindtexte. Abgeschieden wohnen sie${System.lineSeparator()}" +
                "in Buchstabhausen an der Küste des Semantik, eines großen${System.lineSeparator()}" +
                "Sprachozeans. Ein kleines Bächlein namens Duden fließt durch${System.lineSeparator()}" +
                "ihren Ort und versorgt sie mit den nötigen Regelialien. Es ist${System.lineSeparator()}" +
                "ein paradiesmatisches Land, in dem einem gebratene Satzteile${System.lineSeparator()}" +
                "in den Mund fliegen."
        val infoFile = TestableMVInfoFile()
        assertEquals(result, infoFile.callSplitStringIntoMaxFixedLengthLines(DESCRIPTION_TEXT, 62))
    }

    @Test
    fun appendFormatedTableStringToEmptyStringBuilder() {
        val infoFile = TestableMVInfoFile()
        val result = infoFile.callAppendFormattedTableLine(StringBuilder(), "%-12s %s", "Größe [MB]", "194")
        assertEquals("Größe [MB]:  194${System.lineSeparator()}", result.toString())
    }

    private companion object {
        private const val DESCRIPTION_TEXT =
            "Weit hinten, hinter den Wortbergen, fern der Länder Vokalien und Konsonantien leben die Blindtexte. Abgeschieden wohnen sie in Buchstabhausen an der Küste des Semantik, eines großen Sprachozeans. Ein kleines Bächlein namens Duden fließt durch ihren Ort und versorgt sie mit den nötigen Regelialien. Es ist ein paradiesmatisches Land, in dem einem gebratene Satzteile in den Mund fliegen."
    }

    private class TestableMVInfoFile : MVInfoFile() {
        fun callSplitStringIntoMaxFixedLengthLines(input: String?, lineLength: Int): String =
            splitStringIntoMaxFixedLengthLines(input, lineLength)

        fun callAppendFormattedTableLine(sb: StringBuilder, formatString: String, keyTitle: String, value: String): StringBuilder =
            appendFormattedTableLine(sb, formatString, keyTitle, value)
    }
}
