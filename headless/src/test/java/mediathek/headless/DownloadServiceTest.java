/*
 * Copyright (c) 2026 MediathekView contributors.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 */
package mediathek.headless;

import com.sun.net.httpserver.HttpServer;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import java.net.InetSocketAddress;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.time.Duration;

import static mediathek.headless.Model.Film;
import static mediathek.headless.Model.Quality;
import static org.junit.jupiter.api.Assertions.assertArrayEquals;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

class DownloadServiceTest {
    @TempDir
    Path tempDirectory;

    private HttpServer server;

    @AfterEach
    void tearDown() {
        if (server != null) {
            server.stop(0);
        }
    }

    @Test
    void downloadsAtomicallyAndDeduplicatesByEntryId() throws Exception {
        byte[] video = "fake-video-content".getBytes(StandardCharsets.UTF_8);
        server = HttpServer.create(new InetSocketAddress("127.0.0.1", 0), 0);
        server.createContext("/video.mp4", exchange -> {
            exchange.sendResponseHeaders(200, video.length);
            exchange.getResponseBody().write(video);
            exchange.close();
        });
        server.start();
        String videoUrl = "http://127.0.0.1:" + server.getAddress().getPort() + "/video.mp4";

        HistoryStore history = new HistoryStore(tempDirectory.resolve("state/history.db"));
        DownloadService service = new DownloadService(history, Duration.ofSeconds(5));
        Film film = film("stable-id", videoUrl);

        Path downloaded = service.download(film, tempDirectory, "Maus", Quality.HD, false, false);

        assertTrue(downloaded.startsWith(tempDirectory.resolve("Maus")));
        assertArrayEquals(video, Files.readAllBytes(downloaded));
        assertTrue(history.isCompleted("stable-id"));
        assertTrue(history.isCompletedSource(videoUrl));
        assertEquals("completed", history.list(10).get(0).status());
        assertNull(service.download(film, tempDirectory, "Maus", Quality.HD, false, false));
        try (var files = Files.walk(tempDirectory)) {
            assertTrue(files.noneMatch(path -> path.getFileName().toString().contains(".part")));
        }
    }

    @Test
    void sanitizesUnsafeFilenameCharacters() {
        assertEquals("folder_name_", DownloadService.sanitizePathSegment(" folder/name? "));
        assertEquals("download", DownloadService.sanitizePathSegment(".."));
    }

    private static Film film(String id, String videoUrl) {
        return new Film(
                id, "WDR", "Die Maus", "Eine Folge: Test?", "Description",
                1700000000, 1800, 100, "https://example.test", "",
                videoUrl, "", "");
    }
}
