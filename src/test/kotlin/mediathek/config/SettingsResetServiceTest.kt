package mediathek.config

import org.junit.jupiter.api.AfterEach
import org.junit.jupiter.api.Assertions.*
import org.junit.jupiter.api.Test
import org.junit.jupiter.api.io.TempDir
import java.nio.file.Files
import java.nio.file.Path
import kotlin.io.path.readText
import kotlin.io.path.writeText

internal class SettingsResetServiceTest {
    @TempDir
    lateinit var tempDir: Path

    @AfterEach
    fun tearDown() {
        StandardLocations.configureDefault()
    }

    @Test
    fun `reset archives settings and bookmarks without restoring them to active directory`() {
        val settingsDirectory = tempDir.resolve(".mediathek3")
        StandardLocations.configurePortable(settingsDirectory.toString())
        Files.createDirectories(settingsDirectory)
        settingsDirectory.resolve("settings.xml").writeText("old settings")
        settingsDirectory.resolve("bookmarks.json").writeText("old bookmarks")

        SettingsResetService.moveSettingsDirectoryAside()

        assertFalse(Files.exists(settingsDirectory))
        val archives = Files.list(tempDir).use { paths ->
            paths.filter { it.fileName.toString().startsWith(".mediathek3--") }.toList()
        }
        assertEquals(1, archives.size)
        assertEquals("old settings", archives.single().resolve("settings.xml").readText())
        assertEquals("old bookmarks", archives.single().resolve("bookmarks.json").readText())

        assertEquals(settingsDirectory, StandardLocations.getSettingsDirectory())
        assertTrue(Files.isDirectory(settingsDirectory))
        assertFalse(Files.exists(settingsDirectory.resolve("settings.xml")))
        assertFalse(Files.exists(settingsDirectory.resolve("bookmarks.json")))
    }
}
