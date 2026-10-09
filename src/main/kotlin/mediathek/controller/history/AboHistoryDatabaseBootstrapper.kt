/*
 * Copyright (c) 2026 derreisende77.
 * This code was developed as part of the MediathekView project https://github.com/mediathekview/MediathekView
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package mediathek.controller.history

import org.apache.logging.log4j.LogManager
import org.sqlite.SQLiteDataSource
import java.io.IOException
import java.nio.file.Files
import java.nio.file.Path
import java.sql.Connection
import java.sql.SQLException
import java.sql.Statement

internal class AboHistoryDatabaseBootstrapper(
    private val databasePath: Path,
    private val dataSource: SQLiteDataSource,
) {
    fun bootstrap() {
        ensureDatabaseDirectoryExists()

        try {
            openConnection().use { connection ->
                connection.createStatement().use { statement ->
                    applyConnectionPragmas(statement)
                    migrateIfRequired(connection, statement)
                }
            }
        } catch (ex: SQLException) {
            throw IllegalStateException("Could not initialize abo history database", ex)
        }
    }

    private fun ensureDatabaseDirectoryExists() {
        try {
            databasePath.parent?.let(Files::createDirectories)
        } catch (ex: IOException) {
            throw IllegalStateException("Could not create abo history database directory", ex)
        }
    }

    private fun openConnection(): Connection =
        dataSource.connection.apply {
            transactionIsolation = Connection.TRANSACTION_SERIALIZABLE
        }

    private fun applyConnectionPragmas(statement: Statement) {
        statement.executeUpdate("PRAGMA encoding='UTF-8'")
        statement.executeUpdate("PRAGMA journal_mode=WAL")
        statement.executeUpdate("PRAGMA synchronous=NORMAL")
        statement.executeUpdate("PRAGMA foreign_keys=OFF")
    }

    private fun migrateIfRequired(connection: Connection, statement: Statement) {
        val currentVersion = readSchemaVersion(statement)
        check(currentVersion <= CURRENT_SCHEMA_VERSION) {
            "Unsupported abo history schema version $currentVersion"
        }
        if (currentVersion == CURRENT_SCHEMA_VERSION) {
            return
        }

        val previousAutoCommit = connection.autoCommit
        connection.autoCommit = false
        try {
            for (version in currentVersion until CURRENT_SCHEMA_VERSION) {
                migrateOneStep(statement, version, version + 1)
                setSchemaVersion(statement, version + 1)
            }
            connection.commit()
        } catch (ex: SQLException) {
            connection.rollback()
            throw ex
        } catch (ex: RuntimeException) {
            connection.rollback()
            throw ex
        } finally {
            connection.autoCommit = previousAutoCommit
        }
    }

    private fun readSchemaVersion(statement: Statement): Int =
        statement.executeQuery("PRAGMA user_version").use { resultSet ->
            if (resultSet.next()) resultSet.getInt(1) else 0
        }

    private fun setSchemaVersion(statement: Statement, version: Int) {
        statement.executeUpdate("PRAGMA user_version=$version")
    }

    private fun migrateOneStep(statement: Statement, fromVersion: Int, toVersion: Int) {
        logger.info("Migrating abo history database from schema version {} to {}", fromVersion, toVersion)
        when (fromVersion) {
            0 -> migrateFromV0ToV1(statement)
            else -> error("No abo history migration defined from version $fromVersion")
        }
    }

    private fun migrateFromV0ToV1(statement: Statement) {
        statement.executeUpdate(CREATE_TABLE_V1_SQL)
        statement.executeUpdate(CREATE_URL_INDEX_V1_SQL)
    }

    companion object {
        const val CURRENT_SCHEMA_VERSION: Int = 1
        private val logger = LogManager.getLogger()
        private const val CREATE_TABLE_V1_SQL =
            "CREATE TABLE IF NOT EXISTS abo_history (" +
                "id INTEGER PRIMARY KEY," +
                "datum TEXT NOT NULL," +
                "thema TEXT," +
                "titel TEXT," +
                "url TEXT NOT NULL" +
                ")"
        private const val CREATE_URL_INDEX_V1_SQL =
            "CREATE UNIQUE INDEX IF NOT EXISTS idx_abo_history_url " +
                "ON abo_history(url)"
    }
}
