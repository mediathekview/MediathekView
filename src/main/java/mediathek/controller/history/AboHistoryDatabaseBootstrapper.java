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

package mediathek.controller.history;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.jspecify.annotations.NonNull;
import org.sqlite.SQLiteDataSource;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.sql.Connection;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;

final class AboHistoryDatabaseBootstrapper {
    static final int CURRENT_SCHEMA_VERSION = 1;

    private static final Logger logger = LogManager.getLogger();
    private static final String CREATE_TABLE_V1_SQL = """
            CREATE TABLE IF NOT EXISTS abo_history (
                id INTEGER PRIMARY KEY,
                datum TEXT NOT NULL,
                thema TEXT,
                titel TEXT,
                url TEXT NOT NULL
            )
            """;
    private static final String CREATE_URL_INDEX_V1_SQL = """
            CREATE UNIQUE INDEX IF NOT EXISTS idx_abo_history_url
            ON abo_history(url)
            """;

    private final Path databasePath;
    private final SQLiteDataSource dataSource;

    AboHistoryDatabaseBootstrapper(@NonNull Path databasePath, @NonNull SQLiteDataSource dataSource) {
        this.databasePath = databasePath;
        this.dataSource = dataSource;
    }

    void bootstrap() {
        ensureDatabaseDirectoryExists();

        try (var connection = openConnection();
             var statement = connection.createStatement()) {
            applyConnectionPragmas(statement);
            migrateIfRequired(connection, statement);
        } catch (SQLException e) {
            throw new IllegalStateException("Could not initialize abo history database", e);
        }
    }

    private void ensureDatabaseDirectoryExists() {
        try {
            final var parentDirectory = databasePath.getParent();
            if (parentDirectory != null) {
                Files.createDirectories(parentDirectory);
            }
        } catch (IOException e) {
            throw new IllegalStateException("Could not create abo history database directory", e);
        }
    }

    private Connection openConnection() throws SQLException {
        final var connection = dataSource.getConnection();
        connection.setTransactionIsolation(Connection.TRANSACTION_SERIALIZABLE);
        return connection;
    }

    private void applyConnectionPragmas(Statement statement) throws SQLException {
        statement.executeUpdate("PRAGMA encoding='UTF-8'");
        statement.executeUpdate("PRAGMA journal_mode=WAL");
        statement.executeUpdate("PRAGMA synchronous=NORMAL");
        statement.executeUpdate("PRAGMA foreign_keys=OFF");
    }

    private void migrateIfRequired(Connection connection, Statement statement) throws SQLException {
        final var currentVersion = readSchemaVersion(statement);
        if (currentVersion > CURRENT_SCHEMA_VERSION) {
            throw new IllegalStateException("Unsupported abo history schema version " + currentVersion);
        }
        if (currentVersion == CURRENT_SCHEMA_VERSION) {
            return;
        }

        final var previousAutoCommit = connection.getAutoCommit();
        connection.setAutoCommit(false);
        try {
            for (var version = currentVersion; version < CURRENT_SCHEMA_VERSION; version++) {
                migrateOneStep(statement, version, version + 1);
                setSchemaVersion(statement, version + 1);
            }
            connection.commit();
        } catch (SQLException | RuntimeException e) {
            connection.rollback();
            throw e;
        } finally {
            connection.setAutoCommit(previousAutoCommit);
        }
    }

    private int readSchemaVersion(Statement statement) throws SQLException {
        try (ResultSet resultSet = statement.executeQuery("PRAGMA user_version")) {
            return resultSet.next() ? resultSet.getInt(1) : 0;
        }
    }

    @SuppressWarnings("java:S2077")
    private void setSchemaVersion(Statement statement, int version) throws SQLException {
        statement.executeUpdate("PRAGMA user_version=" + version);
    }

    private void migrateOneStep(Statement statement, int fromVersion, int toVersion) throws SQLException {
        logger.info("Migrating abo history database from schema version {} to {}", fromVersion, toVersion);
        switch (fromVersion) {
            case 0 -> migrateFromV0ToV1(statement);
            default -> throw new IllegalStateException("No abo history migration defined from version " + fromVersion);
        }
    }

    private void migrateFromV0ToV1(Statement statement) throws SQLException {
        statement.executeUpdate(CREATE_TABLE_V1_SQL);
        statement.executeUpdate(CREATE_URL_INDEX_V1_SQL);
    }
}
