package mediathek.tool

import mediathek.config.application.ApplicationConfiguration
import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Assertions.assertSame
import org.junit.jupiter.api.Test

internal class FilmListUpdateTypeTest {
    @Test
    fun fromConfigReturnsConfiguredAutomaticValue() {
        val config = ApplicationConfiguration.getInstance()
        val previousValue = config.filmListUpdateType
        config.filmListUpdateType = FilmListUpdateType.AUTOMATIC.configValue
        try {
            val result = FilmListUpdateType.fromConfig()

            assertSame(FilmListUpdateType.AUTOMATIC, result)
        } finally {
            config.filmListUpdateType = previousValue
        }
    }

    @Test
    fun keepsLegacyConfigValues() {
        assertEquals(0, FilmListUpdateType.MANUAL.configValue)
        assertEquals(2, FilmListUpdateType.AUTOMATIC.configValue)
        assertSame(FilmListUpdateType.MANUAL, FilmListUpdateType.fromConfigValue(0))
        assertSame(FilmListUpdateType.AUTOMATIC, FilmListUpdateType.fromConfigValue(2))
        assertSame(FilmListUpdateType.AUTOMATIC, FilmListUpdateType.fromConfigValue(1))
    }
}
