package mediathek.controller.starter

import org.junit.jupiter.api.Assertions.*
import org.junit.jupiter.api.Test
import org.junit.jupiter.api.io.TempDir
import java.io.IOException
import java.nio.file.FileAlreadyExistsException
import java.nio.file.Files
import java.nio.file.InvalidPathException
import java.nio.file.Path

internal class DownloadTargetDirectoryTest {
    @TempDir
    lateinit var tempDir: Path

    @Test
    fun createsNestedDirectoryWithHashCharacters() {
        val target = tempDir.resolve("#test").resolve("test#2")

        val result = createDownloadTargetDirectory(target.toString())

        assertEquals(target, result)
        assertTrue(Files.isDirectory(target))
    }

    @Test
    fun reportsExistingFileAsDirectoryCreationFailure() {
        val target = tempDir.resolve("existing-file")
        Files.writeString(target, "content")

        val exception = assertThrows(IOException::class.java) {
            createDownloadTargetDirectory(target.toString())
        }

        assertTrue(exception.message.orEmpty().contains(target.toAbsolutePath().toString()))
        assertInstanceOf(FileAlreadyExistsException::class.java, exception.cause)
    }

    @Test
    fun reportsInvalidPathAsDirectoryCreationFailure() {
        val exception = assertThrows(IOException::class.java) {
            createDownloadTargetDirectory("invalid\u0000path")
        }

        assertTrue(exception.message.orEmpty().contains("invalid"))
        assertInstanceOf(InvalidPathException::class.java, exception.cause)
    }
}
