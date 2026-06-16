package mediathek.controller

import mediathek.daten.blacklist.BlacklistRule
import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Assertions.assertFalse
import org.junit.jupiter.api.Assertions.assertTrue
import org.junit.jupiter.api.Test
import org.junit.jupiter.api.io.TempDir
import java.nio.file.Path
import kotlin.io.path.readText
import kotlin.io.path.writeText

internal class BlacklistRuleStorageTest {
    @TempDir
    lateinit var tempDir: Path

    @Test
    fun writeAndReadPreservesExplicitRuleFields() {
        val storagePath = tempDir.resolve("blacklist-rules.json")
        val rule = BlacklistRule(
            sender = "ARD",
            thema = "News",
            titel = "tagesschau",
            thema_titel = "News tagesschau",
        )

        BlacklistRuleStorage.write(storagePath, listOf(rule))
        val json = storagePath.readText()

        assertTrue(json.contains("\"version\": 1"))
        assertTrue(json.contains("\"rules\""))
        assertTrue(json.contains("\"sender\": \"ARD\""))
        assertTrue(json.contains("\"topic\": \"News\""))
        assertTrue(json.contains("\"title\": \"tagesschau\""))
        assertTrue(json.contains("\"topicTitle\": \"News tagesschau\""))
        assertFalse(json.contains("\"active\""))

        assertEquals(listOf(rule), BlacklistRuleStorage.read(storagePath))
    }

    @Test
    fun inactiveRuleRoundTripsWithActiveFalse() {
        val storagePath = tempDir.resolve("blacklist-rules.json")
        val rule = BlacklistRule(sender = "ARD", titel = "tagesschau", active = false)

        BlacklistRuleStorage.write(storagePath, listOf(rule))
        val json = storagePath.readText()

        assertTrue(json.contains("\"active\": false"))
        assertEquals(listOf(rule), BlacklistRuleStorage.read(storagePath))
    }

    @Test
    fun missingActiveFieldReadsAsActiveRule() {
        val storagePath = tempDir.resolve("blacklist-rules.json")
        storagePath.writeText(
            """
            {
                "version": 1,
                "rules": [
                    {
                        "sender": "ARD",
                        "title": "tagesschau"
                    }
                ]
            }
            """.trimIndent()
        )

        assertEquals(
            listOf(BlacklistRule(sender = "ARD", titel = "tagesschau", active = true)),
            BlacklistRuleStorage.read(storagePath),
        )
    }

    @Test
    fun writeOmitsEmptyRuleFieldsAndRemovesDuplicates() {
        val storagePath = tempDir.resolve("blacklist-rules.json")

        BlacklistRuleStorage.write(
            storagePath,
            listOf(
                BlacklistRule(sender = "ARD", titel = "tagesschau"),
                BlacklistRule(sender = "ARD", titel = "tagesschau", active = false),
            ),
        )
        val json = storagePath.readText()

        assertFalse(json.contains("\"topic\""))
        assertFalse(json.contains("\"topicTitle\""))
        assertEquals(
            listOf(BlacklistRule(sender = "ARD", titel = "tagesschau")),
            BlacklistRuleStorage.read(storagePath),
        )
    }

    @Test
    fun readReturnsEmptyListWhenStorageFileIsMissing() {
        assertEquals(emptyList<BlacklistRule>(), BlacklistRuleStorage.read(tempDir.resolve("blacklist-rules.json")))
    }
}
