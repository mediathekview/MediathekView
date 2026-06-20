package mediathek.config

import mediathek.controller.BlacklistRuleStorage
import mediathek.daten.blacklist.BlacklistRule
import org.junit.jupiter.api.AfterEach
import org.junit.jupiter.api.Assertions.*
import org.junit.jupiter.api.BeforeEach
import org.junit.jupiter.api.Test
import org.junit.jupiter.api.io.TempDir
import java.nio.file.Files
import java.nio.file.Path

internal class DatenTest {
    @TempDir
    lateinit var tempDir: Path

    private val previousPortableBaseDirectory = StandardLocations.portableBaseDirectory
    private var previousBackupAlreadyHandled = false

    @BeforeEach
    fun setUp() {
        previousBackupAlreadyHandled = backupAlreadyHandled
    }

    @AfterEach
    fun tearDown() {
        StandardLocations.portableBaseDirectory = previousPortableBaseDirectory
        backupAlreadyHandled = previousBackupAlreadyHandled
    }

    @Test
    fun allesSpeichernWritesBlacklistRulesToJsonOnly() {
        StandardLocations.portableBaseDirectory = tempDir.toString()
        val daten = Daten.getInstance()
        val blacklist = daten.listeBlacklist
        val originalBlacklist = ArrayList(blacklist)
        try {
            blacklist.clear()
            blacklist.add(BlacklistRule(sender = "ARD", thema = "News", titel = "tagesschau"))
            blacklist.add(BlacklistRule(sender = "ARD", thema = "News", titel = "tagesschau"))

            daten.allesSpeichern()

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

    private var backupAlreadyHandled: Boolean
        get() = backupAlreadyHandledField.getBoolean(Daten.getInstance())
        set(value) {
            backupAlreadyHandledField.setBoolean(Daten.getInstance(), value)
        }

    private companion object {
        private val backupAlreadyHandledField = Daten::class.java.getDeclaredField("backupAlreadyHandled").apply {
            isAccessible = true
        }
    }
}
