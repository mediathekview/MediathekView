package mediathek.controller.starter

import org.junit.jupiter.api.Assertions.*
import org.junit.jupiter.api.Test
import org.junit.jupiter.api.io.TempDir
import java.io.DataInputStream
import java.io.DataOutputStream
import java.nio.file.Files
import java.nio.file.Path
import java.util.concurrent.TimeUnit

class RuntimeExecTest {
    @TempDir
    lateinit var directory: Path

    @Test
    fun `launches explicit arguments without splitting quoting or delimiter loss`() {
        val output = directory.resolve("argument echo ü.bin")
        val javaName = if (System.getProperty("os.name").startsWith("Windows")) "java.exe" else "java"
        val java = Path.of(System.getProperty("java.home"), "bin", javaName).toString()
        val expected = listOf(
            "a film.mp4", "Français: version d'origine\\original <> ?", "", " leading and trailing ",
            "-metadata:s:a:0", "handler_name=Audiodeskription <> ä",
        )
        val args = listOf(java, "-cp", System.getProperty("java.class.path"),
            RuntimeExecArgumentEcho::class.java.name, output.toString()) + expected
        val process = requireNotNull(RuntimeExec(null, null, "unused fallback", "", args).exec(false))
        try {
            assertTrue(process.waitFor(20, TimeUnit.SECONDS), "Argument echo process timed out")
            assertEquals(0, process.exitValue())
            DataInputStream(Files.newInputStream(output)).use { input ->
                val actual = List(input.readInt()) { input.readUTF() }
                assertEquals(expected, actual)
            }
        } finally {
            process.destroyForcibly()
        }
    }

    @Test
    fun parsesFfmpegDurationFromInputHeader() {
        val input = "  Duration: 02:46:33.40, start: 0.141000, bitrate: N/A"

        val duration = RuntimeExec.parseDurationSeconds(input)

        assertTrue(duration.isPresent)
        assertEquals(9993.40, duration.asDouble, 0.0001)
    }

    @Test
    fun parsesFfmpeg81ProgressTime() {
        val input = "frame=  138 fps=0.0 q=-1.0 size=    4608KiB time=00:00:05.39 bitrate=6994.0kbits/s speed=10.8x elapsed=0:00:00.50"

        val progress = RuntimeExec.parseProgressTimeSeconds(input)

        assertTrue(progress.isPresent)
        assertEquals(5.39, progress.asDouble, 0.0001)
    }

    @Test
    fun parsesBinarySizeUnitsFromFfmpeg81() {
        val input = "frame=  138 fps=0.0 q=-1.0 size=    4608KiB time=00:00:05.39 bitrate=6994.0kbits/s speed=10.8x elapsed=0:00:00.50"

        val size = RuntimeExec.parseSizeBytes(input)

        assertTrue(size.isPresent)
        assertEquals(4_718_592L, size.asLong)
    }

    @Test
    fun parsesLegacyDecimalSizeUnits() {
        val input = "frame=  147 fps= 17 q=-1.0 size=    1588kB time=00:00:05.84 bitrate=2226.0kbits/s"

        val size = RuntimeExec.parseSizeBytes(input)

        assertTrue(size.isPresent)
        assertEquals(1_588_000L, size.asLong)
    }

    @Test
    fun ignoresNonProgressLinesWithoutSize() {
        val input = "Press [q] to stop, [?] for help"

        assertFalse(RuntimeExec.parseSizeBytes(input).isPresent)
        assertFalse(RuntimeExec.parseProgressTimeSeconds(input).isPresent)
    }
}

/** Child-process entry point for verifying the actual RuntimeExec launch boundary. */
internal object RuntimeExecArgumentEcho {
    @JvmStatic
    fun main(args: Array<String>) {
        DataOutputStream(Files.newOutputStream(Path.of(args[0]))).use { output ->
            output.writeInt(args.size - 1)
            args.drop(1).forEach(output::writeUTF)
        }
    }
}
