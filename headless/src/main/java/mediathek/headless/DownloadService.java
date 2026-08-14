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
import java.net.URI;
import java.net.http.HttpClient;
import java.net.http.HttpRequest;
import java.net.http.HttpResponse;
import java.nio.charset.StandardCharsets;
import java.nio.file.FileAlreadyExistsException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.StandardCopyOption;
import java.time.Duration;
import java.time.Instant;
import java.time.ZoneOffset;
import java.time.format.DateTimeFormatter;
import java.util.HexFormat;

import static mediathek.headless.Model.Film;
import static mediathek.headless.Model.Quality;

final class DownloadService {
    private static final DateTimeFormatter DATE_FORMAT = DateTimeFormatter.ofPattern("yyyy-MM-dd")
            .withZone(ZoneOffset.UTC);
    private static final int MAX_FILENAME_LENGTH = 180;

    private final HistoryStore history;
    private final HttpClient httpClient;
    private final Duration timeout;

    DownloadService(HistoryStore history, Duration timeout) {
        this(history, timeout, HttpClient.newBuilder()
                .connectTimeout(timeout)
                .followRedirects(HttpClient.Redirect.NORMAL)
                .build());
    }

    DownloadService(HistoryStore history, Duration timeout, HttpClient httpClient) {
        this.history = history;
        this.timeout = timeout;
        this.httpClient = httpClient;
    }

    Path download(Film film, Path outputRoot, String requestedSubdirectory, Quality quality,
                  boolean subtitles, boolean force) throws Exception {
        if (!force && history.isCompleted(film.id())) {
            return null;
        }
        Path normalizedOutputRoot = outputRoot.toAbsolutePath().normalize();
        requireOutputRoot(normalizedOutputRoot);

        String sourceUrl = quality.selectUrl(film);
        if (sourceUrl.isBlank()) {
            throw new IOException("Entry has no downloadable video URL: " + film.id());
        }

        String subdirectory = requestedSubdirectory == null || requestedSubdirectory.isBlank()
                ? film.topic()
                : requestedSubdirectory;
        Path destinationDirectory = normalizedOutputRoot.resolve(sanitizePathSegment(subdirectory)).normalize();
        if (!destinationDirectory.startsWith(normalizedOutputRoot)) {
            throw new IOException("Download directory escaped the configured output root");
        }
        Files.createDirectories(destinationDirectory);

        String extension = chooseExtension(sourceUrl);
        String date = DATE_FORMAT.format(Instant.ofEpochSecond(film.timestamp()));
        String baseName = sanitizePathSegment(date + " - " + film.title());
        Path destination = uniqueDestination(destinationDirectory, baseName, extension, film.id());
        Path partial = destination.resolveSibling(destination.getFileName() + ".part" + extension);

        history.begin(film, quality, destination, sourceUrl);
        try {
            Files.deleteIfExists(partial);
            if (isHls(sourceUrl)) {
                downloadHls(sourceUrl, partial);
            }
            else {
                downloadHttp(sourceUrl, partial);
            }
            if (Files.size(partial) == 0) {
                throw new IOException("Downloaded file is empty");
            }
            moveCompleted(partial, destination);

            if (subtitles && film.subtitleUrl() != null && !film.subtitleUrl().isBlank()) {
                try {
                    downloadSubtitle(film.subtitleUrl(), destination);
                }
                catch (Exception subtitleError) {
                    System.err.println("Subtitle download failed for " + film.id() + ": " + subtitleError.getMessage());
                }
            }
            history.complete(film.id(), destination);
            return destination;
        }
        catch (Exception exception) {
            try {
                Files.deleteIfExists(partial);
            }
            catch (IOException cleanupError) {
                exception.addSuppressed(cleanupError);
            }
            history.fail(film.id(), exception);
            throw exception;
        }
    }

    static String sanitizePathSegment(String value) {
        String sanitized = value == null ? "download" : value
                .replaceAll("[\\p{Cntrl}/\\\\:*?\"<>|]", "_")
                .replaceAll("\\s+", " ")
                .trim()
                .replaceAll("[. ]+$", "");
        if (sanitized.isBlank() || sanitized.equals(".") || sanitized.equals("..")) {
            sanitized = "download";
        }
        if (sanitized.length() > MAX_FILENAME_LENGTH) {
            sanitized = sanitized.substring(0, MAX_FILENAME_LENGTH).trim();
        }
        return sanitized;
    }

    private void requireOutputRoot(Path outputRoot) throws IOException {
        Path absolute = outputRoot.toAbsolutePath().normalize();
        if (!Files.isDirectory(absolute)) {
            throw new IOException("Output root must already exist: " + absolute);
        }
        if (!Files.isWritable(absolute)) {
            throw new IOException("Output root is not writable: " + absolute);
        }
    }

    private void downloadHttp(String sourceUrl, Path partial) throws IOException, InterruptedException {
        HttpRequest request = HttpRequest.newBuilder(URI.create(sourceUrl))
                .timeout(Duration.ofHours(6))
                .header("User-Agent", "MediathekView-Headless/0.1")
                .GET()
                .build();
        HttpResponse<Path> response = httpClient.send(request, HttpResponse.BodyHandlers.ofFile(partial));
        if (response.statusCode() < 200 || response.statusCode() >= 300) {
            throw new IOException("Video server returned HTTP " + response.statusCode());
        }
    }

    private void downloadHls(String sourceUrl, Path partial) throws IOException, InterruptedException {
        Process process;
        try {
            process = new ProcessBuilder(
                    "ffmpeg", "-hide_banner", "-loglevel", "error", "-nostdin", "-y",
                    "-i", sourceUrl, "-c", "copy", partial.toString())
                    .redirectErrorStream(true)
                    .start();
        }
        catch (IOException exception) {
            throw new IOException("ffmpeg is required for HLS downloads", exception);
        }
        String output = new String(process.getInputStream().readAllBytes(), StandardCharsets.UTF_8);
        int exitCode = process.waitFor();
        if (exitCode != 0) {
            throw new IOException("ffmpeg failed with exit code " + exitCode + ": " + output.trim());
        }
    }

    private void downloadSubtitle(String sourceUrl, Path videoPath) throws IOException, InterruptedException {
        String extension = subtitleExtension(sourceUrl);
        Path destination = replaceExtension(videoPath, extension);
        Path partial = destination.resolveSibling(destination.getFileName() + ".part");
        try {
            HttpRequest request = HttpRequest.newBuilder(URI.create(sourceUrl))
                    .timeout(timeout)
                    .header("User-Agent", "MediathekView-Headless/0.1")
                    .GET()
                    .build();
            HttpResponse<Path> response = httpClient.send(request, HttpResponse.BodyHandlers.ofFile(partial));
            if (response.statusCode() >= 200 && response.statusCode() < 300 && Files.size(partial) > 0) {
                moveCompleted(partial, destination);
            }
            else {
                Files.deleteIfExists(partial);
            }
        }
        catch (IllegalArgumentException exception) {
            Files.deleteIfExists(partial);
        }
    }

    private static Path uniqueDestination(Path directory, String baseName, String extension, String id)
            throws IOException {
        Path preferred = directory.resolve(baseName + extension);
        if (!Files.exists(preferred)) {
            return preferred;
        }
        String suffix = shortId(id);
        Path withId = directory.resolve(baseName + " [" + suffix + "]" + extension);
        if (!Files.exists(withId)) {
            return withId;
        }
        throw new FileAlreadyExistsException(withId.toString());
    }

    private static String chooseExtension(String url) {
        String path = URI.create(url).getPath().toLowerCase();
        if (path.endsWith(".webm")) {
            return ".webm";
        }
        if (path.endsWith(".mkv")) {
            return ".mkv";
        }
        return ".mp4";
    }

    private static String subtitleExtension(String url) {
        String path = URI.create(url).getPath().toLowerCase();
        if (path.endsWith(".vtt")) {
            return ".vtt";
        }
        if (path.endsWith(".srt")) {
            return ".srt";
        }
        return ".ttml";
    }

    private static Path replaceExtension(Path path, String extension) {
        String name = path.getFileName().toString();
        int dot = name.lastIndexOf('.');
        String withoutExtension = dot > 0 ? name.substring(0, dot) : name;
        return path.resolveSibling(withoutExtension + extension);
    }

    private static boolean isHls(String url) {
        return URI.create(url).getPath().toLowerCase().endsWith(".m3u8");
    }

    private static String shortId(String id) {
        byte[] bytes = id.getBytes(StandardCharsets.UTF_8);
        return HexFormat.of().formatHex(bytes, 0, Math.min(bytes.length, 4));
    }

    private static void moveCompleted(Path source, Path destination) throws IOException {
        try {
            Files.move(source, destination, StandardCopyOption.ATOMIC_MOVE);
        }
        catch (IOException exception) {
            Files.move(source, destination);
        }
    }
}
