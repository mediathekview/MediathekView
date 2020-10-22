package mediathek.tool.sql

import java.sql.Connection
import java.sql.SQLException

/**
 * Automatically rollback a connection if it hs not been commited on close.
 * Helper for try-with-resources.
 */
class SqlAutoRollback(private val conn: Connection) : AutoCloseable {
    private var committed = false
    @Throws(SQLException::class)
    fun commit() {
        conn.commit()
        committed = true
    }

    @Throws(SQLException::class)
    override fun close() {
        if (!committed) {
            conn.rollback()
        }
    }
}