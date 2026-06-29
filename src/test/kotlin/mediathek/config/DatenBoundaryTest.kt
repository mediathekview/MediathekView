package mediathek.config

import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Test
import java.io.File
import java.nio.file.Files
import java.nio.file.Path
import kotlin.streams.asSequence

internal class DatenBoundaryTest {
    @Test
    fun `Daten is only imported by composition roots`() {
        val violations = productionSourceFiles()
            .filter { sourceFile ->
                sourceFile.repoPath() !in allowedDatenImportFiles &&
                    Files.readAllLines(sourceFile).any(::isDatenImport)
            }
            .map { it.repoPath() }
            .sorted()

        assertEquals(emptyList<String>(), violations)
    }

    @Test
    fun `Daten is only constructed at application entry point`() {
        val violations = productionSourceFiles()
            .filter { sourceFile ->
                sourceFile.repoPath() !in allowedDatenConstructionFiles &&
                    datenConstructionPatterns.any { it.containsMatchIn(Files.readString(sourceFile)) }
            }
            .map { it.repoPath() }
            .sorted()

        assertEquals(emptyList<String>(), violations)
    }

    private fun productionSourceFiles(): List<Path> = productionSourceRoots
        .filter(Files::exists)
        .flatMap { sourceRoot ->
            Files.walk(sourceRoot).use { stream ->
                stream.asSequence()
                    .filter(Files::isRegularFile)
                    .filter { it.fileName.toString().endsWith(".kt") || it.fileName.toString().endsWith(".java") }
                    .toList()
            }
        }

    private fun isDatenImport(line: String): Boolean {
        val trimmed = line.trim()
        return trimmed == "import mediathek.config.Daten" || trimmed == "import mediathek.config.Daten;"
    }

    private fun Path.repoPath(): String = toString().replace(File.separatorChar, '/')

    private companion object {
        val productionSourceRoots = listOf(
            Path.of("src/main/kotlin"),
            Path.of("src/main/java"),
        )
        val allowedDatenImportFiles = setOf(
            "src/main/kotlin/mediathek/mainwindow/MediathekGui.kt",
            "src/main/kotlin/mediathek/mac/MediathekGuiMac.kt",
            "src/main/kotlin/mediathek/windows/MediathekGuiWindows.kt",
            "src/main/kotlin/mediathek/x11/MediathekGuiX11.kt",
        )
        val allowedDatenConstructionFiles = setOf(
            "src/main/kotlin/mediathek/Main.kt",
        )
        val datenConstructionPatterns = listOf(
            Regex("""(?<![\p{L}\p{N}_])Daten\(\)"""),
            Regex("""\bnew\s+Daten\s*\("""),
        )
    }
}
