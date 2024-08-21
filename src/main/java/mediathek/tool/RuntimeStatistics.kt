package mediathek.tool

import mediathek.tool.FileUtils.humanReadableByteCountBinary
import mediathek.tool.http.MVHttpClient
import org.apache.logging.log4j.LogManager
import org.apache.logging.log4j.Logger
import java.time.Duration
import java.time.LocalDateTime
import java.time.LocalTime
import java.time.format.DateTimeFormatter

object RuntimeStatistics {
    @JvmField
    val startZeit: LocalDateTime = LocalDateTime.now()
    val logger: Logger = LogManager.getLogger()

    /**
     * Output runtime statistics to console and log file
     */
    @JvmStatic
    fun printRuntimeStatistics() {
        val formatter = DateTimeFormatter.ISO_LOCAL_DATE_TIME
        val endZeit = LocalDateTime.now()
        val runTime = LocalTime.MIN.plusSeconds(Duration.between(startZeit, endZeit).toSeconds())

        logger.info("   --> Start:    ${formatter.format(startZeit)}")
        logger.info("   --> Ende:     ${formatter.format(endZeit)}")
        logger.info("   --> Laufzeit: ${runTime.hour}h ${runTime.minute}m ${runTime.second}s")
    }

    @JvmStatic
    fun printDataUsageStatistics() {
        val byteCounter = MVHttpClient.getInstance().byteCounter
        logger.info("Total data sent:     ${humanReadableByteCountBinary(byteCounter.totalBytesWritten())}")
        logger.info("Total data received: ${humanReadableByteCountBinary(byteCounter.totalBytesRead())}")
    }
}
