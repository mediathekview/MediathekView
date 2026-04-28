package mediathek.config

import org.junit.jupiter.api.AfterEach
import org.junit.jupiter.api.Assertions.*
import org.junit.jupiter.api.Test
import org.junit.jupiter.api.io.TempDir
import java.awt.Color
import java.nio.file.Path
import kotlin.io.path.exists
import kotlin.io.path.readText

internal class MVColorTest {
    @TempDir
    lateinit var tempDir: Path

    @AfterEach
    fun tearDown() {
        MVColor.reset()
        StandardLocations.portableBaseDirectory = null
        legacyKeys().forEach(MVConfig::remove)
    }

    @Test
    fun savesAndLoadsOverridesFromJsonFile() {
        StandardLocations.portableBaseDirectory = tempDir.toString()

        MVColor.reset()
        MVColor.NEW_COLOR.setColor(false, Color(10, 20, 30))
        MVColor.NEW_COLOR.setColor(true, Color(40, 50, 60))
        MVColor.SELECTED_COLOR.setColor(false, Color(70, 80, 90))
        MVColor.save()

        val storageFile = tempDir.resolve("app-colors.json")
        assertTrue(storageFile.exists())
        val savedJson = storageFile.readText()
        assertTrue(savedJson.contains("\"film_new\""))
        assertTrue(savedJson.contains("\"light\": {"))
        assertTrue(savedJson.contains("\"r\": 10"))
        assertTrue(savedJson.contains("\"g\": 20"))
        assertTrue(savedJson.contains("\"b\": 30"))
        assertTrue(savedJson.contains("\"a\": 255"))
        assertTrue(savedJson.contains("\"dark\": {"))

        MVColor.reset()
        MVColor.load()

        assertEquals(Color(10, 20, 30), MVColor.NEW_COLOR.getOverrideColor(false))
        assertEquals(Color(40, 50, 60), MVColor.NEW_COLOR.getOverrideColor(true))
        assertEquals(Color(70, 80, 90), MVColor.SELECTED_COLOR.getOverrideColor(false))
        assertNull(MVColor.SELECTED_COLOR.getOverrideColor(true))
    }

    @Test
    fun migratesLegacyMvConfigEntriesOnce() {
        StandardLocations.portableBaseDirectory = tempDir.toString()

        val legacyLight = Color(101, 102, 103)
        val legacyDark = Color(131, 132, 133)
        val sharedColor = Color(55, 66, 77)

        MVConfig.add("FARBE_FILM_NEU", "${legacyLight.rgb}${MVConfig.TRENNER}${legacyDark.rgb}")
        MVConfig.add("FARBE_SELECTED_ICON", sharedColor.rgb.toString())

        MVColor.reset()
        MVColor.load()

        val storageFile = tempDir.resolve("app-colors.json")
        assertTrue(storageFile.exists())
        assertEquals(legacyLight, MVColor.NEW_COLOR.getOverrideColor(false))
        assertEquals(legacyDark, MVColor.NEW_COLOR.getOverrideColor(true))
        assertEquals(sharedColor, MVColor.SELECTED_COLOR.getOverrideColor(false))
        assertEquals(sharedColor, MVColor.SELECTED_COLOR.getOverrideColor(true))
        assertEquals("", MVConfig.get("FARBE_FILM_NEU"))
        assertEquals("", MVConfig.get("FARBE_SELECTED_ICON"))

        MVColor.reset()
        MVColor.load()

        assertEquals(legacyLight, MVColor.NEW_COLOR.getOverrideColor(false))
        assertEquals(legacyDark, MVColor.NEW_COLOR.getOverrideColor(true))
        assertEquals(sharedColor, MVColor.SELECTED_COLOR.getOverrideColor(false))
    }

    private fun legacyKeys() = listOf(
        "FARBE_FILM_HISTORY",
        "FARBE_FILM_BOOKMARKED",
        "FARBE_FILM_DUPLICATE",
        "FARBE_FILM_NEU",
        "FARBE_FILTER_REGEX",
        "FARBE_SELECTED_ICON",
        "FARBE_TABLE_ALTERNATE_ROW",
        "FARBE_DOWNLOAD_IST_ABO",
        "FARBE_DOWNLOAD_IST_DIREKTER_DOWNLOAD",
        "FARBE_DOWNLOAD_WAIT",
        "FARBE_DOWNLOAD_WAIT_SEL",
        "FARBE_DOWNLOAD_RUN",
        "FARBE_DOWNLOAD_RUN_SEL",
        "FARBE_DOWNLOAD_FERTIG",
        "FARBE_DOWNLOAD_FERTIG_SEL",
        "FARBE_DOWNLOAD_FEHLER",
        "FARBE_DOWNLOAD_FEHLER_SEL",
        "FARBE_DOWNLOAD_DATEINAME_NEU",
        "FARBE_DOWNLOAD_DATEINAME_ALT"
    )
}
