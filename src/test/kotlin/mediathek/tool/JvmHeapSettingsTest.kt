package mediathek.tool

import com.sun.management.HotSpotDiagnosticMXBean
import mediathek.Main
import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Assertions.assertTrue
import org.junit.jupiter.api.Assumptions.assumeTrue
import org.junit.jupiter.api.io.TempDir
import org.junit.jupiter.params.ParameterizedTest
import org.junit.jupiter.params.provider.CsvSource
import java.lang.management.ManagementFactory
import java.nio.file.Path
import java.util.concurrent.TimeUnit
import kotlin.io.path.readText

internal class JvmHeapSettingsTest {
    @ParameterizedTest
    @CsvSource(
        "Shenandoah, 2048,     , 1536",
        "Shenandoah, 1280,     , 1280",
        "Shenandoah, 1536,     , 1536",
        "Shenandoah, 2048, 1024, 1024",
        "Shenandoah, 2048, 1792, 1792",
        "G1,         2048,     , 2048",
    )
    fun defaultTargetUsesEffectiveHeapLimit(
        collector: String,
        hardMiB: Int,
        explicitMiB: Int?,
        expectedSoftMiB: Int,
        @TempDir directory: Path,
    ) {
        val collectorOption = "Use${collector}GC"
        val diagnostics = ManagementFactory.getPlatformMXBean(HotSpotDiagnosticMXBean::class.java)
        assumeTrue(runCatching { diagnostics?.getVMOption(collectorOption) }.getOrNull() != null)
        val java = ProcessHandle.current().info().command().orElseThrow()
        val output = directory.resolve("heap-settings.log")
        val command = buildList {
            addAll(listOf(java, "-Xmx${hardMiB}m", "-XX:+$collectorOption"))
            if (collector == "Shenandoah") {
                add("-XX:ShenandoahGCHeuristics=compact")
            }
            explicitMiB?.let { add("-XX:SoftMaxHeapSize=${it}m") }
            addAll(listOf("-cp", System.getProperty("java.class.path"), HeapSettingsProbe::class.java.name))
        }
        val process = ProcessBuilder(command).redirectErrorStream(true).redirectOutput(output.toFile()).start()
        try {
            assertTrue(process.waitFor(20, TimeUnit.SECONDS), "Application entry point timed out")
            val log = output.readText()
            assertEquals(0, process.exitValue(), log)
            val bytesPerMiB = 1024L * 1024
            assertTrue("max=${hardMiB * bytesPerMiB} soft=${expectedSoftMiB * bytesPerMiB}" in log, log)
        } finally {
            if (process.isAlive) {
                process.destroyForcibly()
                process.waitFor(5, TimeUnit.SECONDS)
            }
        }
    }

    object HeapSettingsProbe {
        @JvmStatic
        fun main(@Suppress("UNUSED_PARAMETER") args: Array<String>) {
            Runtime.getRuntime().addShutdownHook(Thread {
                val diagnostics = ManagementFactory.getPlatformMXBean(HotSpotDiagnosticMXBean::class.java)
                val max = diagnostics.getVMOption("MaxHeapSize").value
                val soft = diagnostics.getVMOption("SoftMaxHeapSize").value
                println("max=$max soft=$soft")
            })
            Main.main(arrayOf("--help"))
        }
    }
}
