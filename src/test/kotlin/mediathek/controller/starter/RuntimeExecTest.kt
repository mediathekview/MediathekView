package mediathek.controller.starter

import org.junit.jupiter.api.Assertions.*
import org.junit.jupiter.api.Test

class RuntimeExecTest {

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
