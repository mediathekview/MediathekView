package mediathek.config

import org.junit.jupiter.api.AfterEach
import org.junit.jupiter.api.Assertions.*
import org.junit.jupiter.api.Test
import org.junit.jupiter.api.io.TempDir
import java.nio.file.Files
import java.nio.file.Path
import java.nio.file.attribute.FileTime
import java.time.Instant
import java.time.temporal.ChronoUnit
import kotlin.io.path.exists
import kotlin.io.path.readText
import kotlin.io.path.writeText

internal class ConfigurationBackupServiceTest {
    @TempDir
    lateinit var tempDir: Path

    @AfterEach
    fun tearDown() {
        StandardLocations.portableBaseDirectory = null
    }

    @Test
    fun rotatesBackupsAndDeletesCopiesAboveConfiguredMaximum() {
        StandardLocations.portableBaseDirectory = tempDir.toString()
        tempDir.resolve(Konstanten.CONFIG_FILE).writeText("current")
        backupPath(1).writeText("copy-1")
        Files.setLastModifiedTime(backupPath(1), FileTime.from(Instant.now().minus(1, ChronoUnit.DAYS)))
        val maxBackupCopies = Konstanten.MAX_NUM_BACKUP_FILE_COPIES.toInt()
        backupPath(maxBackupCopies).writeText("last-retained-copy")
        backupPath(maxBackupCopies + 1).writeText("superfluous-copy")
        val similarlyPrefixedFile = tempDir.resolve("${Konstanten.CONFIG_FILE_COPY}legacy")
        similarlyPrefixedFile.writeText("not a numbered backup")

        assertTrue(ConfigurationBackupService.createConfigurationBackupCopies())

        assertEquals("current", backupPath(1).readText())
        assertEquals("copy-1", backupPath(2).readText())
        assertFalse(backupPath(maxBackupCopies + 1).exists())
        assertTrue(similarlyPrefixedFile.exists())
    }

    @Test
    fun deletesSuperfluousBackupsEvenWhenTodaysBackupAlreadyExists() {
        StandardLocations.portableBaseDirectory = tempDir.toString()
        backupPath(1).writeText("today")
        val maxBackupCopies = Konstanten.MAX_NUM_BACKUP_FILE_COPIES.toInt()
        backupPath(maxBackupCopies + 1).writeText("superfluous-copy")
        Files.setLastModifiedTime(backupPath(1), Files.getLastModifiedTime(tempDir))

        assertTrue(ConfigurationBackupService.createConfigurationBackupCopies())

        assertFalse(backupPath(maxBackupCopies + 1).exists())
    }

    private fun backupPath(index: Int): Path =
        tempDir.resolve(Konstanten.CONFIG_FILE_COPY + index)
}
