package mediathek.daten

import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Assertions.assertNotNull
import org.junit.jupiter.api.Assertions.assertNull
import org.junit.jupiter.api.Assertions.assertTrue
import org.junit.jupiter.api.Test

internal class ListePsetVorlagenTest {
    @Test
    fun importPsetTextPreservesProgramSetsAndFollowingPrograms() {
        val liste = ListePsetVorlagen.importPsetText(
            """
            <?xml version="1.0" encoding="UTF-8"?>
            <Mediathek>
                <Programmset>
                    <Name>Save</Name>
                    <Speichern>true</Speichern>
                    <Info-URL>https://example.invalid/info</Info-URL>
                </Programmset>
                <Programm>
                    <Programmname>ffmpeg</Programmname>
                    <Programmpfad>/usr/bin/ffmpeg</Programmpfad>
                    <Programmschalter>-i %f **</Programmschalter>
                    <Praefix>http</Praefix>
                    <Suffix>m3u8</Suffix>
                    <Restart>true</Restart>
                </Programm>
                <Programm>
                    <Programmname>vlc</Programmname>
                    <Programmpfad>/usr/bin/vlc</Programmpfad>
                    <Programmschalter>%f --play-and-exit</Programmschalter>
                </Programm>
                <Programmset>
                    <Name>Play</Name>
                    <Abspielen>true</Abspielen>
                </Programmset>
                <Programm>
                    <Programmname>player</Programmname>
                    <Programmpfad>/usr/bin/player</Programmpfad>
                </Programm>
            </Mediathek>
            """.trimIndent(),
            false,
        )

        assertNotNull(liste)
        requireNotNull(liste)
        assertEquals(2, liste.size)

        val save = liste[0]
        assertEquals("Save", save.name)
        assertEquals("https://example.invalid/info", save[DatenPset.PROGRAMMSET_INFO_URL])
        assertEquals(2, save.listeProg.size)
        assertEquals("ffmpeg", save.getProg(0).name)
        assertEquals("/usr/bin/ffmpeg", save.getProg(0).programPath)
        assertEquals("-i %f **", save.getProg(0).switches)
        assertEquals("http", save.getProg(0).prefix)
        assertEquals("m3u8", save.getProg(0).suffix)
        assertTrue(save.getProg(0).isRestart)
        assertEquals("vlc", save.getProg(1).name)

        val play = liste[1]
        assertEquals("Play", play.name)
        assertEquals(1, play.listeProg.size)
        assertEquals("player", play.getProg(0).name)
    }

    @Test
    fun importPsetTextReturnsNullWhenNoProgramSetExists() {
        assertNull(ListePsetVorlagen.importPsetText("<Mediathek></Mediathek>", false))
    }

    @Test
    fun importedProgramSetListKeepsMutableVersionField() {
        val liste = ListePsetVorlagen.importPsetText(
            """
            <Mediathek>
                <Programmset>
                    <Name>Save</Name>
                </Programmset>
            </Mediathek>
            """.trimIndent(),
            false,
        )

        requireNotNull(liste)
        liste.version = "2026.1"

        assertEquals("2026.1", liste.version)
    }

    @Test
    fun createModelFiltersTemplatesByOperatingSystemText() {
        val vorlagen = ListePsetVorlagen().apply {
            add(arrayOf("Linux Set", "Linux description", "1", "Linux", "https://example.invalid/linux.xml", ""))
            add(arrayOf("Mac Set", "Mac description", "2", "Mac", "https://example.invalid/mac.xml", ""))
        }

        val linuxModel = vorlagen.createModel("Linux")
        val allModel = vorlagen.createModel("")

        assertEquals(1, linuxModel.rowCount)
        assertEquals("Linux Set", linuxModel.getValueAt(0, ListePsetVorlagen.PGR_NAME_NR))
        assertEquals(2, allModel.rowCount)
    }
}
