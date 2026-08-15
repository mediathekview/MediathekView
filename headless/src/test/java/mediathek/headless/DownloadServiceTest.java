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

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.net.InetSocketAddress;
import java.net.http.HttpClient;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.time.Duration;
import java.util.List;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicReference;

import static mediathek.headless.Model.Film;
import static mediathek.headless.Model.Quality;
import static org.junit.jupiter.api.Assertions.assertArrayEquals;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
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
        assertTrue(downloaded.getFileName().toString().contains(
                "[src-" + EpisodeIdentity.sourceFingerprint(videoUrl) + "]"));
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

    @Test
    void skipsAnEntryAlreadyPresentUnderALegacyFilename() throws Exception {
        String sourceUrl = "https://example.test/unreachable.mp4";
        Path directory = Files.createDirectories(
                tempDirectory.resolve("Anna, Nina, Pia und die wilden Tiere"));
        Path existing = Files.writeString(directory.resolve(
                "Anna, Nina, Pia und die wilden Tiere-Die Raubkatzen von Brasilien-"
                        + EpisodeIdentity.legacySourceHash(sourceUrl) + ".mp4"),
                "existing-video");
        HistoryStore history = new HistoryStore(tempDirectory.resolve("state/history.db"));
        DownloadService service = new DownloadService(history, Duration.ofSeconds(1));
        Film film = new Film(
                "new-catalog-id", "BR", "Anna, Nina, Pia und die wilden Tiere",
                "Die Raubkatzen von Brasilien", "", 1700000000, 1400, 1,
                "", "", sourceUrl, "", "");

        assertNull(service.download(film, tempDirectory, "Anna und die wilden Tiere",
                Quality.HD, false, false));
        assertEquals("existing-video", Files.readString(existing));
        assertTrue(history.list(10).isEmpty());
    }

    @Test
    void timesOutAndTerminatesAStalledFfmpegProcess() throws Exception {
        HistoryStore history = new HistoryStore(tempDirectory.resolve("state/history.db"));
        AtomicReference<List<String>> command = new AtomicReference<>();
        AtomicReference<BlockingProcess> launched = new AtomicReference<>();
        DownloadService.ProcessLauncher launcher = builder -> {
            command.set(List.copyOf(builder.command()));
            BlockingProcess process = new BlockingProcess();
            launched.set(process);
            return process;
        };
        DownloadService service = new DownloadService(
                history,
                Duration.ofSeconds(5),
                HttpClient.newHttpClient(),
                Duration.ofMillis(10),
                launcher);
        Film film = film("stalled-id", "https://example.test/stalled.m3u8");

        var exception = assertThrows(IOException.class, () -> service.download(
                film, tempDirectory, "HLS", Quality.HD, false, false));

        assertTrue(exception.getMessage().contains("ffmpeg timed out after"));
        int timeoutOption = command.get().indexOf("-rw_timeout");
        assertTrue(timeoutOption >= 0);
        assertEquals("5000000", command.get().get(timeoutOption + 1));
        assertFalse(launched.get().isAlive());
        assertEquals("failed", history.list(10).get(0).status());
        try (var files = Files.walk(tempDirectory)) {
            assertTrue(files.noneMatch(path -> {
                String name = path.getFileName().toString();
                return name.contains(".part") || name.startsWith(".ffmpeg-");
            }));
        }
    }

    private static Film film(String id, String videoUrl) {
        return new Film(
                id, "WDR", "Die Maus", "Eine Folge: Test?", "Description",
                1700000000, 1800, 100, "https://example.test", "",
                videoUrl, "", "");
    }

    private static final class BlockingProcess extends Process {
        private final CountDownLatch terminated = new CountDownLatch(1);
        private volatile boolean alive = true;

        @Override
        public OutputStream getOutputStream() {
            return OutputStream.nullOutputStream();
        }

        @Override
        public InputStream getInputStream() {
            return InputStream.nullInputStream();
        }

        @Override
        public InputStream getErrorStream() {
            return InputStream.nullInputStream();
        }

        @Override
        public int waitFor() throws InterruptedException {
            terminated.await();
            return 143;
        }

        @Override
        public boolean waitFor(long timeout, TimeUnit unit) throws InterruptedException {
            return terminated.await(timeout, unit);
        }

        @Override
        public int exitValue() {
            if (alive) {
                throw new IllegalThreadStateException("process is still running");
            }
            return 143;
        }

        @Override
        public void destroy() {
            alive = false;
            terminated.countDown();
        }

        @Override
        public Process destroyForcibly() {
            destroy();
            return this;
        }

        @Override
        public boolean isAlive() {
            return alive;
        }
    }
}
