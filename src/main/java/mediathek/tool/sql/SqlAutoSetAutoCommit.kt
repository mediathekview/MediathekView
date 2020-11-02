package mediathek.tool.sql

import java.sql.Connection
import java.sql.SQLException

/**
 * Set the autocommit state of a connection and restore to original on close.
 */
class SqlAutoSetAutoCommit(private val conn: Connection, autoCommit: Boolean) : AutoCloseable {
    private val originalAutoCommit: Boolean = conn.autoCommit

    @Throws(SQLException::class)
    override fun close() {
        conn.autoCommit = originalAutoCommit
    }

    init {
        conn.autoCommit = autoCommit
    }
}