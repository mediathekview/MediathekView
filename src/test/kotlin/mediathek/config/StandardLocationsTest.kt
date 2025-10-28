package mediathek.config

import org.apache.commons.lang3.SystemUtils
import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Assertions.assertTrue
import org.junit.jupiter.api.Test
import org.junit.jupiter.api.condition.EnabledOnOs
import org.junit.jupiter.api.condition.OS
import java.nio.file.Paths
import kotlin.io.path.isDirectory
import kotlin.io.path.relativeToOrNull

internal class StandardLocationsTest {

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