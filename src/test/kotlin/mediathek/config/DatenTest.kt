package mediathek.config

import mediathek.controller.AboRuleStorage
import mediathek.controller.BlacklistRuleStorage
import mediathek.daten.abo.DatenAbo
import mediathek.daten.blacklist.BlacklistRule
import org.junit.jupiter.api.AfterEach
import org.junit.jupiter.api.Assertions.*
import org.junit.jupiter.api.BeforeEach
import org.junit.jupiter.api.Test
import org.junit.jupiter.api.io.TempDir
import java.nio.file.Files
import java.nio.file.Path
import java.time.LocalDate

internal class DatenTest {
    @TempDir
    lateinit var tempDir: Path

    private val previousPortableBaseDirectory = StandardLocations.portableBaseDirectory
    private lateinit var daten: Daten

    @BeforeEach
    fun setUp() {
        daten = Daten()
    }

    @AfterEach
    fun tearDown() {
        StandardLocations.portableBaseDirectory = previousPortableBaseDirectory
        daten.downloads.shutdown()
    }

    @Test
    fun configurationPersistenceWritesBlacklistRulesToJsonOnly() {
        StandardLocations.portableBaseDirectory = tempDir.toString()
        val blacklist = daten.blacklist.rules
        val originalBlacklist = ArrayList(blacklist)
        try {
            blacklist.clear()
            blacklist.add(BlacklistRule(sender = "ARD", thema = "News", titel = "tagesschau"))
            blacklist.add(BlacklistRule(sender = "ARD", thema = "News", titel = "tagesschau"))

            daten.configurationPersistence.saveAll()

            val xml = Files.readString(StandardLocations.getMediathekXmlFile())
            assertTrue(Files.exists(StandardLocations.getBlacklistRulesFilePath()))
            assertFalse(xml.contains("<Blacklist>"))
            assertFalse(xml.contains("black-sender"))
            assertFalse(xml.contains("black-thema"))
            assertFalse(xml.contains("black-titel"))
            assertEquals(
                listOf(BlacklistRule(sender = "ARD", thema = "News", titel = "tagesschau")),
                BlacklistRuleStorage.read(StandardLocations.getBlacklistRulesFilePath()),
            )
        } finally {
            blacklist.clear()
            blacklist.addAll(originalBlacklist)
        }
    }

    @Test
    fun configurationPersistenceWritesAboRulesToJsonOnly() {
        StandardLocations.portableBaseDirectory = tempDir.toString()
        val abos = daten.abos.list
        val originalAbos = ArrayList(abos)
        try {
            abos.clear()
            abos.add(
                DatenAbo().apply {
                    name = "JSON Abo"
                    sender = "ARD"
                    title = "tagesschau"
                    downloadDate = LocalDate.of(2026, 6, 25)
                },
            )

            daten.configurationPersistence.saveAll()

            val xml = Files.readString(StandardLocations.getMediathekXmlFile())
            assertTrue(Files.exists(StandardLocations.getAboRulesFilePath()))
            assertFalse(xml.contains("<Abonnement>"))
            assertFalse(xml.contains("JSON Abo"))

            val restored = AboRuleStorage.read(StandardLocations.getAboRulesFilePath()).single()
            assertEquals("JSON Abo", restored.name)
            assertEquals("ARD", restored.sender)
            assertEquals("tagesschau", restored.title)
            assertEquals(LocalDate.of(2026, 6, 25), restored.downloadDate)
        } finally {
            abos.clear()
            abos.addAll(originalAbos)
        }
    }
}
