package mediathek.tool

import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Assertions.assertSame
import org.junit.jupiter.api.Test

internal class FilmListUpdateTypeTest {
    @Test
    fun fromConfigReturnsAutomaticByDefault() {
        val result = FilmListUpdateType.fromConfig()

        assertSame(FilmListUpdateType.AUTOMATIC, result)
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
