package mediathek.controller.history

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
                connection.close()
            }
        }
    }
}