package mediathek.config.application

import org.apache.commons.configuration2.XMLConfiguration
import org.junit.jupiter.api.Assertions.assertFalse
import org.junit.jupiter.api.Assertions.assertTrue
import org.junit.jupiter.api.Test

internal class ApplicationSearchConfigurationTest {
    @Test
    fun `LRU history is disabled by default and persists either choice`() {
        val xml = XMLConfiguration()
        val configuration = ApplicationSearchConfiguration(xml)

        assertFalse(configuration.useLruSearchHistory)
        configuration.useLruSearchHistory = true
        assertTrue(ApplicationSearchConfiguration(xml).useLruSearchHistory)
        configuration.useLruSearchHistory = false
        assertFalse(ApplicationSearchConfiguration(xml).useLruSearchHistory)
    }
}
