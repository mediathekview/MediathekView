package mediathek.controller

import mediathek.daten.abo.DatenAbo
import mediathek.daten.abo.FilmLengthState
import org.junit.jupiter.api.Assertions.*
import org.junit.jupiter.api.Test
import org.junit.jupiter.api.io.TempDir
import java.nio.file.Files
import java.nio.file.Path
import java.time.LocalDate

internal class AboRuleStorageTest {
    @TempDir
    lateinit var tempDir: Path

    @Test
    fun writeAndReadRoundTripsClassicAboRules() {
        val storageFile = tempDir.resolve("abo-rules.json")
        val abo = DatenAbo().apply {
            isActive = false
            name = "Daily News"
            sender = "ARD"
            thema = "Nachrichten"
            title = "tagesschau"
            themaTitel = "Politik"
            irgendwo = "Berlin"
            mindestDauerMinuten = 12
            filmLengthState = FilmLengthState.MAXIMUM
            zielpfad = "/tmp/news"
            downloadDate = LocalDate.of(2026, 6, 25)
            psetName = "Speichern"
            isDoNotStartAutomatically = true
        }

        AboRuleStorage.write(storageFile, listOf(abo))

        val json = Files.readString(storageFile)
        assertTrue(json.contains("\"version\""))
        assertTrue(json.contains("\"rules\""))
        assertTrue(json.contains("\"type\": \"classic\""))
        assertTrue(json.contains("\"lastDownloadDate\": \"2026-06-25\""))
        assertFalse(json.contains("Abonnement"))

        val restored = AboRuleStorage.read(storageFile).single()
        assertFalse(restored.isActive)
        assertEquals("Daily News", restored.name)
        assertEquals("ARD", restored.sender)
        assertEquals("Nachrichten", restored.thema)
        assertEquals("tagesschau", restored.title)
        assertEquals("Politik", restored.themaTitel)
        assertEquals("Berlin", restored.irgendwo)
        assertEquals(12, restored.mindestDauerMinuten)
        assertEquals(FilmLengthState.MAXIMUM, restored.filmLengthState)
        assertEquals("/tmp/news", restored.zielpfad)
        assertEquals(LocalDate.of(2026, 6, 25), restored.downloadDate)
        assertEquals("Speichern", restored.psetName)
        assertTrue(restored.isDoNotStartAutomatically)
    }

    @Test
    fun readReturnsEmptyListWhenFileDoesNotExist() {
        assertTrue(AboRuleStorage.read(tempDir.resolve("missing.json")).isEmpty())
    }

    @Test
    fun readIgnoresUnknownFutureRuleTypes() {
        val storageFile = tempDir.resolve("abo-rules.json")
        Files.writeString(
            storageFile,
            """
            {
              "version": 1,
              "rules": [
                { "type": "future", "name": "Not supported yet" },
                { "type": "classic", "name": "Supported", "sender": "ZDF" }
              ]
            }
            """.trimIndent(),
        )

        val restored = AboRuleStorage.read(storageFile)

        assertEquals(1, restored.size)
        assertEquals("Supported", restored.single().name)
        assertEquals("ZDF", restored.single().sender)
    }

    @Test
    fun readTreatsInvalidDownloadDateAsMissing() {
        val storageFile = tempDir.resolve("abo-rules.json")
        Files.writeString(
            storageFile,
            """
            {
              "version": 1,
              "rules": [
                { "type": "classic", "name": "Invalid Date", "lastDownloadDate": "25.06.2026" }
              ]
            }
            """.trimIndent(),
        )

        val restored = AboRuleStorage.read(storageFile).single()

        assertEquals("Invalid Date", restored.name)
        assertEquals(null, restored.downloadDate)
    }
}
