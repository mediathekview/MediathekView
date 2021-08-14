package mediathek.config

import org.apache.commons.lang3.SystemUtils
import org.junit.jupiter.api.Assertions.assertTrue
import org.junit.jupiter.api.Test
import java.nio.file.Paths

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
    fun getStandardDownloadPath() {
        val path = if (SystemUtils.IS_OS_MAC_OSX) {
            Paths.get(SystemUtils.USER_HOME, "Downloads")
        } else {
            Paths.get(SystemUtils.USER_HOME, Konstanten.VERZEICHNIS_DOWNLOADS)
        }
        val loc2 = path.toAbsolutePath().toString()
        assertTrue { StandardLocations.getStandardDownloadPath() == loc2 }
    }
}