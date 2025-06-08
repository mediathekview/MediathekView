package mediathek.tool;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.junit.jupiter.MockitoExtension;

@ExtendWith(MockitoExtension.class)
class MVInfoFileTest {
    private static final String DESCRIPTION_TEXT = "Weit hinten, hinter den Wortbergen, fern der Länder Vokalien und Konsonantien leben die Blindtexte. Abgeschieden wohnen sie in Buchstabhausen an der Küste des Semantik, eines großen Sprachozeans. Ein kleines Bächlein namens Duden fließt durch ihren Ort und versorgt sie mit den nötigen Regelialien. Es ist ein paradiesmatisches Land, in dem einem gebratene Satzteile in den Mund fliegen.";

/*
    @Mock
    DatenFilm datenFilm;
*/

    @Test
    void splitDescriptionTextIntoOneLine() {
        MVInfoFile infoFile = new MVInfoFile();
        Assertions.assertEquals("The Big Brown Fox Jumps over the Lazy Dog", infoFile.splitStringIntoMaxFixedLengthLines("The Big Brown Fox Jumps over the Lazy Dog", 50));
    }

    @Test
    void splitDescriptionTextIntoMore() {
        String result = "Weit hinten, hinter den Wortbergen, fern der Länder Vokalien" + System.lineSeparator()
                + "und Konsonantien leben die Blindtexte. Abgeschieden wohnen sie" + System.lineSeparator()
                + "in Buchstabhausen an der Küste des Semantik, eines großen" + System.lineSeparator()
                + "Sprachozeans. Ein kleines Bächlein namens Duden fließt durch" + System.lineSeparator()
                + "ihren Ort und versorgt sie mit den nötigen Regelialien. Es ist" + System.lineSeparator()
                + "ein paradiesmatisches Land, in dem einem gebratene Satzteile" + System.lineSeparator()
                + "in den Mund fliegen.";
        MVInfoFile infoFile = new MVInfoFile();
        Assertions.assertEquals(result, infoFile.splitStringIntoMaxFixedLengthLines(DESCRIPTION_TEXT, 62));
    }

    @Test
    void appendFormatedTableStringToEmptyStringBuilder() {
        StringBuilder sb = new StringBuilder();
        MVInfoFile infoFile = new MVInfoFile();
        sb = infoFile.appendFormattedTableLine(sb, "%-12s %s", "Größe [MB]", "194");
        Assertions.assertEquals("Größe [MB]:  194" + System.lineSeparator(), sb.toString());
    }
}