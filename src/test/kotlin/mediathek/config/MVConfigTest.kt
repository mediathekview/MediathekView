package mediathek.config

import org.junit.jupiter.api.Assertions.assertFalse
import org.junit.jupiter.api.Assertions.assertTrue
import org.junit.jupiter.api.Test
import java.io.StringWriter
import javax.xml.stream.XMLOutputFactory

class MVConfigTest {
    @Test
    fun isSystemElement_invalid() {
        val res = MVConfig.isSystemElement("blaXblubb")
        assertFalse(res)
    }

    @Test
    fun isSystemElement_valid() {
        val res = MVConfig.isSystemElement("system")
        assertTrue(res)
    }

    @Test
    fun writeSystemConfiguration_skips_unknown_loaded_config_keys() {
        val knownKey = MVConfig.Configs.SYSTEM_BLACKLIST_ZUKUNFT_NICHT_ANZEIGEN
        val originalValue = MVConfig.get(knownKey)
        MVConfig.add("blaXblubb", "unused")
        MVConfig.add(knownKey, "true")

        try {
            val writer = StringWriter()
            val xmlWriter = XMLOutputFactory.newFactory().createXMLStreamWriter(writer)

            MVConfig.writeSystemConfiguration(xmlWriter)
            xmlWriter.flush()

            val xml = writer.toString()
            assertFalse(xml.contains("blaXblubb"))
            assertTrue(xml.contains("<Blacklist-Zukunft-nicht-anzeigen>true</Blacklist-Zukunft-nicht-anzeigen>"))
        } finally {
            MVConfig.remove("blaXblubb")
            MVConfig.add(knownKey, originalValue)
        }
    }
}
