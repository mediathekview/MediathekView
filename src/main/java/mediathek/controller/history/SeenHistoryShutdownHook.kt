package mediathek.controller.history

import org.apache.logging.log4j.LogManager
import java.sql.Connection

/**
 * Shutdown hook implementation to close a SQL connection if it is still open at program end.
 */
class SeenHistoryShutdownHook(private val connection: Connection?) : Thread() {
    override fun run() {
        if (connection == null)
            return
        else {
            if (!connection.isClosed) {
                logger.trace("Closing seen history database connection.")
                connection.close()
            }
        }
    }

    companion object {
        private val logger = LogManager.getLogger()
    }
}