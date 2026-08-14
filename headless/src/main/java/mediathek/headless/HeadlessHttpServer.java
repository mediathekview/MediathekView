/*
 * Copyright (c) 2026 MediathekView contributors.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 */
package mediathek.headless;

import com.sun.net.httpserver.HttpExchange;
import com.sun.net.httpserver.HttpServer;

import java.io.IOException;
import java.io.InputStream;
import java.net.InetSocketAddress;
import java.nio.charset.StandardCharsets;
import java.nio.file.Path;
import java.util.List;
import java.util.Map;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

import static mediathek.headless.Model.ApiDownloadRequest;
import static mediathek.headless.Model.Film;
import static mediathek.headless.Model.Quality;
import static mediathek.headless.Model.SearchRequest;
import static mediathek.headless.Model.SubscriptionConfig;

final class HeadlessHttpServer implements AutoCloseable {
    private final HttpServer server;
    private final ExecutorService requestExecutor = Executors.newFixedThreadPool(4);
    private final ExecutorService workExecutor = Executors.newSingleThreadExecutor();
    private final CountDownLatch stopped = new CountDownLatch(1);
    private final MediathekViewWebClient client;
    private final DownloadService downloads;
    private final HistoryStore history;
    private final SubscriptionService subscriptions;
    private final SubscriptionConfig config;
    private final Path outputRoot;

    HeadlessHttpServer(String bindAddress, int port,
                       MediathekViewWebClient client,
                       DownloadService downloads,
                       HistoryStore history,
                       SubscriptionService subscriptions,
                       SubscriptionConfig config) throws IOException {
        this.client = client;
        this.downloads = downloads;
        this.history = history;
        this.subscriptions = subscriptions;
        this.config = config;
        this.outputRoot = config == null || config.outputDirectory() == null
                ? null
                : Path.of(config.outputDirectory()).toAbsolutePath().normalize();
        server = HttpServer.create(new InetSocketAddress(bindAddress, port), 32);
        server.setExecutor(requestExecutor);
        server.createContext("/health", this::health);
        server.createContext("/api/search", this::search);
        server.createContext("/api/entries", this::entries);
        server.createContext("/api/downloads", this::downloads);
        server.createContext("/api/sync", this::sync);
    }

    void start() {
        server.start();
    }

    void await() throws InterruptedException {
        stopped.await();
    }

    private void health(HttpExchange exchange) throws IOException {
        if (!method(exchange, "GET")) {
            return;
        }
        send(exchange, 200, Map.of("status", "ok"));
    }

    private void search(HttpExchange exchange) throws IOException {
        if (!method(exchange, "POST")) {
            return;
        }
        try {
            SearchRequest request = readBody(exchange, SearchRequest.class);
            send(exchange, 200, client.search(request));
        }
        catch (Exception exception) {
            sendError(exchange, 502, exception);
        }
    }

    private void entries(HttpExchange exchange) throws IOException {
        if (!method(exchange, "POST")) {
            return;
        }
        try {
            List<String> ids;
            try (InputStream input = exchange.getRequestBody()) {
                ids = JsonSupport.MAPPER.readValue(
                        input,
                        JsonSupport.MAPPER.getTypeFactory().constructCollectionType(List.class, String.class));
            }
            send(exchange, 200, Map.of("results", client.entries(ids)));
        }
        catch (Exception exception) {
            sendError(exchange, 502, exception);
        }
    }

    private void downloads(HttpExchange exchange) throws IOException {
        if (exchange.getRequestMethod().equalsIgnoreCase("GET")) {
            try {
                send(exchange, 200, Map.of("downloads", history.list(100)));
            }
            catch (Exception exception) {
                sendError(exchange, 500, exception);
            }
            return;
        }
        if (!method(exchange, "POST")) {
            return;
        }
        if (outputRoot == null) {
            send(exchange, 409, Map.of("error", "Server has no subscription config/outputDirectory"));
            return;
        }
        try {
            ApiDownloadRequest request = readBody(exchange, ApiDownloadRequest.class);
            if (request.id() == null || request.id().isBlank()) {
                send(exchange, 400, Map.of("error", "id is required"));
                return;
            }
            Film film = client.entry(request.id());
            Quality quality = Quality.parse(request.quality());
            boolean subtitles = request.subtitles() == null || request.subtitles();
            workExecutor.submit(() -> {
                try {
                    downloads.download(film, outputRoot, request.subdirectory(), quality, subtitles, false);
                }
                catch (Exception exception) {
                    System.err.println("Download failed for " + film.id() + ": " + exception.getMessage());
                }
            });
            send(exchange, 202, Map.of("id", film.id(), "status", "queued"));
        }
        catch (IllegalArgumentException exception) {
            sendError(exchange, 400, exception);
        }
        catch (Exception exception) {
            sendError(exchange, 502, exception);
        }
    }

    private void sync(HttpExchange exchange) throws IOException {
        if (!method(exchange, "POST")) {
            return;
        }
        if (config == null) {
            send(exchange, 409, Map.of("error", "Server was started without --config"));
            return;
        }
        workExecutor.submit(() -> {
            try {
                subscriptions.sync(config);
            }
            catch (Exception exception) {
                System.err.println("Subscription sync failed: " + exception.getMessage());
            }
        });
        send(exchange, 202, Map.of("status", "queued"));
    }

    private static boolean method(HttpExchange exchange, String expected) throws IOException {
        if (exchange.getRequestMethod().equalsIgnoreCase(expected)) {
            return true;
        }
        exchange.getResponseHeaders().set("Allow", expected);
        send(exchange, 405, Map.of("error", "Method not allowed"));
        return false;
    }

    private static <T> T readBody(HttpExchange exchange, Class<T> type) throws IOException {
        try (InputStream input = exchange.getRequestBody()) {
            return JsonSupport.MAPPER.readValue(input, type);
        }
    }

    private static void sendError(HttpExchange exchange, int status, Exception exception) throws IOException {
        String message = exception.getMessage();
        if (message == null || message.isBlank()) {
            message = exception.getClass().getSimpleName();
        }
        send(exchange, status, Map.of("error", message));
    }

    private static void send(HttpExchange exchange, int status, Object value) throws IOException {
        byte[] body = JsonSupport.write(value).getBytes(StandardCharsets.UTF_8);
        exchange.getResponseHeaders().set("Content-Type", "application/json; charset=utf-8");
        exchange.getResponseHeaders().set("Cache-Control", "no-store");
        exchange.sendResponseHeaders(status, body.length);
        try (var output = exchange.getResponseBody()) {
            output.write(body);
        }
    }

    @Override
    public void close() {
        server.stop(1);
        workExecutor.shutdownNow();
        requestExecutor.shutdownNow();
        stopped.countDown();
    }
}
