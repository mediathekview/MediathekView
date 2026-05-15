package mediathek.mac

import org.apache.logging.log4j.LogManager
import java.io.IOException
import kotlin.time.Duration.Companion.seconds
import kotlin.time.toJavaDuration

/**
 * Prevents system sleep on macOS.
 */
class OsxPowerManager : AutoCloseable {
    private var caffeinateProcess: Process? = null

    @Synchronized
    fun disablePowerManagement() {
        if (caffeinateProcess?.isAlive == true) {
            return
        }

        caffeinateProcess = null
        try {
            caffeinateProcess = ProcessBuilder(CAFFEINATE_PATH).start()
            logger.trace("power management disabled")
        } catch (ex: IOException) {
            caffeinateProcess = null
            logger.error("disabling power management failed", ex)
        }
    }

    @Synchronized
    fun enablePowerManagement() {
        val process = caffeinateProcess ?: return
        caffeinateProcess = null
        stopCaffeinateProcess(process)
        logger.trace("power management enabled")
    }

    private fun stopCaffeinateProcess(process: Process) {
        if (!process.isAlive) {
            return
        }

        process.destroy()
        try {
            if (!process.waitFor(PROCESS_STOP_TIMEOUT.toJavaDuration()) && process.isAlive) {
                logger.warn("caffeinate did not stop gracefully; destroying forcibly")
                process.destroyForcibly()
                process.waitFor(PROCESS_STOP_TIMEOUT.toJavaDuration())
            }
        } catch (ex: InterruptedException) {
            Thread.currentThread().interrupt()
            if (process.isAlive) {
                process.destroyForcibly()
            }
            logger.warn("Interrupted while stopping caffeinate", ex)
        }
    }

    override fun close() {
        enablePowerManagement()
    }

    private companion object {
        private val logger = LogManager.getLogger(OsxPowerManager::class.java)
        private const val CAFFEINATE_PATH = "/usr/bin/caffeinate"
        private val PROCESS_STOP_TIMEOUT = 2.seconds
    }
}
