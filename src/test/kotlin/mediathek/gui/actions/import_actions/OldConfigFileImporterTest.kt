package mediathek.gui.actions.import_actions

import mediathek.config.Daten
import mediathek.daten.abo.FilmLengthState
import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Assertions.assertTrue
import org.junit.jupiter.api.Test
import org.junit.jupiter.api.io.TempDir
import java.nio.file.Files
import java.nio.file.Path
import java.time.LocalDate

internal class OldConfigFileImporterTest {
    @TempDir
    lateinit var tempDir: Path

    @Test
    fun importAboBlacklistStillImportsLegacyXmlAbos() {
        val abos = Daten.getInstance().listeAbo
        val originalAbos = ArrayList(abos)
        try {
            abos.clear()
            val configFile = tempDir.resolve("old-mediathek.xml")
            Files.writeString(
                configFile,
                """
                <?xml version="1.0" encoding="UTF-8"?>
                <Mediathek>
                    <Abonnement>
                        <aktiv>false</aktiv>
                        <Name>Imported Legacy Abo</Name>
                        <Sender>ARD</Sender>
                        <Thema>News</Thema>
                        <Titel>tagesschau</Titel>
                        <Thema-Titel>Politics</Thema-Titel>
                        <Irgendwo>Berlin</Irgendwo>
                        <Mindestdauer>15</Mindestdauer>
                        <min_max>false</min_max>
                        <Zielpfad>/tmp/imported</Zielpfad>
                        <letztes_Abo>25.06.2026</letztes_Abo>
                        <Programmset>Save</Programmset>
                        <nicht_automatisch_starten>true</nicht_automatisch_starten>
                    </Abonnement>
                </Mediathek>
                """.trimIndent(),
            )

            val result = OldConfigFileImporter().importAboBlacklist(
                configFile.toString(),
                importAbo = true,
                importBlacklist = false,
                importReplaceList = false,
            )

            assertEquals(1, result.foundAbos)
            val imported = abos.single()
            assertEquals("Imported Legacy Abo", imported.name)
            assertEquals("ARD", imported.sender)
            assertEquals("News", imported.thema)
            assertEquals("tagesschau", imported.title)
            assertEquals("Politics", imported.themaTitel)
            assertEquals("Berlin", imported.irgendwo)
            assertEquals(15, imported.mindestDauerMinuten)
            assertEquals(FilmLengthState.MAXIMUM, imported.filmLengthState)
            assertEquals("/tmp/imported", imported.zielpfad)
            assertEquals(LocalDate.of(2026, 6, 25), imported.downloadDate)
            assertEquals("Save", imported.psetName)
            assertTrue(imported.isDoNotStartAutomatically)
        } finally {
            abos.clear()
            abos.addAll(originalAbos)
        }
    }
}
