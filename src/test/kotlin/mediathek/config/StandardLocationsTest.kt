package mediathek.config

import org.apache.commons.lang3.SystemUtils
import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Assertions.assertTrue
import org.junit.jupiter.api.Test
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
    fun getXDGDownloadDirectory() {
        if (SystemUtils.IS_OS_LINUX) {
            assertTrue { StandardLocations.getXDGDownloadDirectory().isPresent }
            assertTrue { StandardLocations.getXDGDownloadDirectory().get().isDirectory() }
            assertTrue { StandardLocations.getXDGDownloadDirectory().get().relativeToOrNull(Paths.get(SystemUtils.USER_HOME)) != null }
        } else {
            assertTrue { StandardLocations.getXDGDownloadDirectory().isEmpty }
        }
    }

    @Test
    fun getStandardDownloadPath() {
        val path = if (SystemUtils.IS_OS_MAC_OSX) {
            Paths.get(SystemUtils.USER_HOME, "Downloads")
        } else if (SystemUtils.IS_OS_LINUX) {
            StandardLocations.getXDGDownloadDirectory().orElse(Paths.get(SystemUtils.USER_HOME, Konstanten.VERZEICHNIS_DOWNLOADS))
        } else {
            Paths.get(SystemUtils.USER_HOME, Konstanten.VERZEICHNIS_DOWNLOADS)
        }
        val loc2 = path.toAbsolutePath().toString()
        assertEquals(StandardLocations.getStandardDownloadPath(), loc2)
    }
}