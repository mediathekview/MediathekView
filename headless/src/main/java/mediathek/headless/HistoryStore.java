/*
 * Copyright (c) 2026 MediathekView contributors.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 */
package mediathek.headless;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;
import java.time.Instant;
import java.util.ArrayList;
import java.util.List;

import static mediathek.headless.Model.DownloadRecord;
import static mediathek.headless.Model.Film;
import static mediathek.headless.Model.Quality;

final class HistoryStore {
    private final String jdbcUrl;

    HistoryStore(Path databasePath) throws IOException, SQLException {
        Path absolutePath = databasePath.toAbsolutePath().normalize();
        Path parent = absolutePath.getParent();
        if (parent != null) {
            Files.createDirectories(parent);
        }
        jdbcUrl = "jdbc:sqlite:" + absolutePath;
        initialize();
    }

    boolean isCompleted(String id) throws SQLException {
        String sql = "SELECT 1 FROM downloads WHERE id = ? AND status = 'completed'";
        try (Connection connection = connect();
             PreparedStatement statement = connection.prepareStatement(sql)) {
            statement.setString(1, id);
            try (ResultSet result = statement.executeQuery()) {
                return result.next();
            }
        }
    }

    boolean isCompletedSource(String sourceUrl) throws SQLException {
        if (sourceUrl == null || sourceUrl.isBlank()) {
            return false;
        }
        String sql = "SELECT 1 FROM downloads WHERE source_url = ? AND status = 'completed'";
        try (Connection connection = connect();
             PreparedStatement statement = connection.prepareStatement(sql)) {
            statement.setString(1, sourceUrl);
            try (ResultSet result = statement.executeQuery()) {
                return result.next();
            }
        }
    }

    void begin(Film film, Quality quality, Path outputPath, String sourceUrl) throws SQLException {
        String sql = """
                INSERT INTO downloads (
                    id, status, channel, topic, title, quality, output_path, source_url,
                    started_at, completed_at, error
                ) VALUES (?, 'downloading', ?, ?, ?, ?, ?, ?, ?, NULL, NULL)
                ON CONFLICT(id) DO UPDATE SET
                    status = 'downloading', channel = excluded.channel, topic = excluded.topic,
                    title = excluded.title, quality = excluded.quality,
                    output_path = excluded.output_path, source_url = excluded.source_url,
                    started_at = excluded.started_at, completed_at = NULL, error = NULL
                """;
        try (Connection connection = connect();
             PreparedStatement statement = connection.prepareStatement(sql)) {
            statement.setString(1, film.id());
            statement.setString(2, film.channel());
            statement.setString(3, film.topic());
            statement.setString(4, film.title());
            statement.setString(5, quality.name().toLowerCase());
            statement.setString(6, outputPath.toString());
            statement.setString(7, sourceUrl);
            statement.setString(8, Instant.now().toString());
            statement.executeUpdate();
        }
    }

    void complete(String id, Path outputPath) throws SQLException {
        String sql = "UPDATE downloads SET status = 'completed', output_path = ?, completed_at = ?, error = NULL WHERE id = ?";
        try (Connection connection = connect();
             PreparedStatement statement = connection.prepareStatement(sql)) {
            statement.setString(1, outputPath.toString());
            statement.setString(2, Instant.now().toString());
            statement.setString(3, id);
            statement.executeUpdate();
        }
    }

    void fail(String id, Throwable error) throws SQLException {
        String sql = "UPDATE downloads SET status = 'failed', error = ? WHERE id = ?";
        try (Connection connection = connect();
             PreparedStatement statement = connection.prepareStatement(sql)) {
            statement.setString(1, compactError(error));
            statement.setString(2, id);
            statement.executeUpdate();
        }
    }

    List<DownloadRecord> list(int limit) throws SQLException {
        int safeLimit = Math.max(1, Math.min(limit, 1000));
        String sql = """
                SELECT id, status, channel, topic, title, quality, output_path, source_url,
                       started_at, completed_at, error
                FROM downloads
                ORDER BY started_at DESC
                LIMIT ?
                """;
        List<DownloadRecord> records = new ArrayList<>();
        try (Connection connection = connect();
             PreparedStatement statement = connection.prepareStatement(sql)) {
            statement.setInt(1, safeLimit);
            try (ResultSet result = statement.executeQuery()) {
                while (result.next()) {
                    records.add(new DownloadRecord(
                            result.getString("id"),
                            result.getString("status"),
                            result.getString("channel"),
                            result.getString("topic"),
                            result.getString("title"),
                            result.getString("quality"),
                            result.getString("output_path"),
                            result.getString("source_url"),
                            result.getString("started_at"),
                            result.getString("completed_at"),
                            result.getString("error")));
                }
            }
        }
        return records;
    }

    private void initialize() throws SQLException {
        try (Connection connection = connect();
             Statement statement = connection.createStatement()) {
            statement.execute("PRAGMA journal_mode = WAL");
            statement.execute("PRAGMA busy_timeout = 10000");
            statement.execute("""
                    CREATE TABLE IF NOT EXISTS downloads (
                        id TEXT PRIMARY KEY,
                        status TEXT NOT NULL,
                        channel TEXT,
                        topic TEXT,
                        title TEXT,
                        quality TEXT,
                        output_path TEXT,
                        source_url TEXT,
                        started_at TEXT NOT NULL,
                        completed_at TEXT,
                        error TEXT
                    )
                    """);
            statement.execute("CREATE INDEX IF NOT EXISTS downloads_status_idx ON downloads(status)");
            statement.execute("CREATE INDEX IF NOT EXISTS downloads_source_url_idx ON downloads(source_url)");
        }
    }

    private Connection connect() throws SQLException {
        Connection connection = DriverManager.getConnection(jdbcUrl);
        try (Statement statement = connection.createStatement()) {
            statement.execute("PRAGMA busy_timeout = 10000");
        }
        return connection;
    }

    private static String compactError(Throwable error) {
        String message = error.getMessage();
        if (message == null || message.isBlank()) {
            message = error.getClass().getSimpleName();
        }
        return message.length() > 2000 ? message.substring(0, 2000) : message;
    }
}
