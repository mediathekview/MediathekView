package mediathek.controller.starter

import mediathek.daten.DatenDownload
import mediathek.daten.DatenProg
import mediathek.daten.DatenPset
import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Assertions.assertNull
import org.junit.jupiter.api.Test
import org.junit.jupiter.api.io.TempDir
import java.nio.file.Files
import java.nio.file.Path

internal class FfmpegExecutableResolverTest {
    @TempDir
    lateinit var tempDir: Path

    @Test
    fun prefersExecutableFromDownloadProgramSet() {
        val ffmpeg = executable("ffmpeg")
        val pset = DatenPset("Set").apply {
            addProg(DatenProg("ffmpeg", ffmpeg.toString(), "-i %f **", false.toString(), false.toString()))
        }
        val download = DatenDownload().apply {
            pSet = pset
            programName = "ffmpeg"
            downloadUrl = "https://example.invalid/video.m3u8"
        }

        assertEquals(ffmpeg, FfmpegExecutableResolver.resolve(download, configuredPath = "", pathLookup = { null }))
    }

    @Test
    fun prefersPathExecutableOverConfiguredExecutableWhenDownloadProgramIsUnavailable() {
        val pathFfmpeg = executable("ffmpeg")
        val configuredFfmpeg = executable("configured/ffmpeg")
        val download = DatenDownload()

        assertEquals(
            pathFfmpeg,
            FfmpegExecutableResolver.resolve(
                download,
                configuredPath = configuredFfmpeg.toString(),
                pathLookup = { pathFfmpeg },
            ),
        )
    }

    @Test
    fun usesConfiguredExecutableWhenDownloadProgramAndPathAreUnavailable() {
        val ffmpeg = executable("configured/ffmpeg")
        val download = DatenDownload()

        assertEquals(ffmpeg, FfmpegExecutableResolver.resolve(download, configuredPath = ffmpeg.toString(), pathLookup = { null }))
    }

    @Test
    fun returnsNullWhenNoExecutableCanBeResolved() {
        assertNull(FfmpegExecutableResolver.resolve(DatenDownload(), configuredPath = "", pathLookup = { null }))
    }

    private fun executable(relativePath: String): Path {
        val path = tempDir.resolve(relativePath)
        Files.createDirectories(path.parent)
        Files.writeString(path, "")
        path.toFile().setExecutable(true)
        return path
    }
}
