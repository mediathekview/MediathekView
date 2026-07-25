package mediathek.config

import mediathek.tool.sql.SqlDatabaseConfig
import org.apache.commons.lang3.SystemUtils
import org.junit.jupiter.api.AfterEach
import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Assertions.assertTrue
import org.junit.jupiter.api.Test
import org.junit.jupiter.api.condition.EnabledOnOs
import org.junit.jupiter.api.condition.OS
import org.junit.jupiter.api.io.TempDir
import picocli.CommandLine
import java.nio.file.Files
import java.nio.file.Path
import java.nio.file.Paths
import kotlin.io.path.isDirectory
import kotlin.io.path.relativeToOrNull

internal class StandardLocationsTest {
    @TempDir
    lateinit var tempDir: Path

    @AfterEach
    fun tearDown() {
        CommandLineOptions.baseFilePath = null
        CommandLineOptions.setPortableMode(false)
        StandardLocations.configureDefault()
    }

    @Test
    fun getSettingsDirectory() {
        //tests ONLY non-portable configuration!
        val testPath = Paths.get(SystemUtils.USER_HOME, Konstanten.VERZEICHNIS_EINSTELLUNGEN)
        assertTrue { testPath == StandardLocations.getSettingsDirectory() }
    }

    @Test
    fun getMediathekXmlFile() {
        val xmlFilePath = StandardLocations.getMediathekXmlFile()
        //tests ONLY non-portable configuration!
        val testPath = Paths.get(SystemUtils.USER_HOME, Konstanten.VERZEICHNIS_EINSTELLUNGEN,Konstanten.CONFIG_FILE)
        assertTrue { testPath == xmlFilePath }
    }

    @Test
    fun portablePathsUseParsedSettingsDirectory() {
        val settingsDirectory = tempDir.resolve("Einstellungen/.mediathek3")
        configurePortableModeFromCommandLine(settingsDirectory)

        assertTrue(StandardLocations.isPortableMode())
        assertEquals(settingsDirectory, StandardLocations.getSettingsDirectory())
        assertEquals(settingsDirectory.resolve(Konstanten.CONFIG_FILE), StandardLocations.getMediathekXmlFile())
        assertEquals(settingsDirectory.resolve("MediathekView.lock"), StandardLocations.getLockFilePath())
        assertEquals(settingsDirectory.resolve(Konstanten.JSON_DATEI_FILME).toString(), StandardLocations.getFilmlistFilePathString())
        assertEquals(settingsDirectory.resolve("mv_index"), StandardLocations.getFilmIndexPath())
        assertEquals(settingsDirectory.resolve("watchlist.db"), StandardLocations.getWatchlistDatabasePath())
    }

    @Test
    fun portableLogFilePathUsesAndCreatesSettingsDirectory() {
        val settingsDirectory = tempDir.resolve("Einstellungen/.mediathek3")
        configurePortableModeFromCommandLine(settingsDirectory)

        assertEquals(settingsDirectory.resolve("mediathekview.log"), StandardLocations.getLogFilePath())
        assertTrue(Files.isDirectory(settingsDirectory))
    }

    @Test
    fun historyDatabasePathFollowsCurrentSettingsDirectory() {
        SqlDatabaseConfig.historyDbPath
        val settingsDirectory = tempDir.resolve("Einstellungen/.mediathek3")
        configurePortableModeFromCommandLine(settingsDirectory)

        assertEquals(settingsDirectory.resolve("history.db"), SqlDatabaseConfig.historyDbPath)
    }

    @Test
    fun defaultConfigurationDisablesPortableModeEvenWhenCommandLineFlagWasPreviouslySet() {
        CommandLineOptions.setPortableMode(true)

        StandardLocations.configureDefault()

        assertEquals(false, StandardLocations.isPortableMode())
    }

    private fun configurePortableModeFromCommandLine(settingsDirectory: Path) {
        val parseResult = CommandLine(CommandLineOptions).parseArgs(settingsDirectory.toString())
        CommandLineOptions.setPortableMode(parseResult.hasMatchedPositional(0))
        if (parseResult.hasMatchedPositional(0)) {
            StandardLocations.configurePortable(CommandLineOptions.baseFilePath)
        } else {
            StandardLocations.configureDefault()
        }
    }

    @Test
    @EnabledOnOs(OS.LINUX)
    fun getLinuxXDGDownloadDirectory() {
            assertTrue { StandardLocations.getXDGDownloadDirectory().isPresent }
            assertTrue { StandardLocations.getXDGDownloadDirectory().get().isDirectory() }
            assertTrue { StandardLocations.getXDGDownloadDirectory().get().relativeToOrNull(Paths.get(SystemUtils.USER_HOME)) != null }
    }

    @Test
    @EnabledOnOs(OS.MAC)
    fun getMacStandardDownloadPath() {
        val path = Paths.get(SystemUtils.USER_HOME, "Downloads")
        assertEquals(StandardLocations.getStandardDownloadPath(), path.toAbsolutePath().toString())
    }

    @Test
    @EnabledOnOs(OS.LINUX)
    fun getLinuxStandardDownloadPath() {
        val path = StandardLocations.getXDGDownloadDirectory().orElse(Paths.get(SystemUtils.USER_HOME, Konstanten.VERZEICHNIS_DOWNLOADS))
        assertEquals(StandardLocations.getStandardDownloadPath(), path.toAbsolutePath().toString())
    }

    @Test
    @EnabledOnOs(OS.WINDOWS)
    fun getWindowsStandardDownloadPath() {
        val path = Paths.get(SystemUtils.USER_HOME, Konstanten.VERZEICHNIS_DOWNLOADS)
        assertEquals(StandardLocations.getStandardDownloadPath(), path.toAbsolutePath().toString())
    }
}
