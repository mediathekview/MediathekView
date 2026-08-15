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
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import java.io.IOException;
import java.net.InetSocketAddress;
import java.net.URI;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.time.Duration;
import java.util.List;

import static mediathek.headless.Model.QueryClause;
import static mediathek.headless.Model.Subscription;
import static mediathek.headless.Model.SubscriptionConfig;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

class SubscriptionServiceTest {
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
    void filtersBeforeApplyingLimitAndDryRunDoesNotDownload() throws Exception {
        server = HttpServer.create(new InetSocketAddress("127.0.0.1", 0), 0);
        server.createContext("/api/query", exchange -> send(exchange, """
                {"result":{"results":[
                  {"id":"latest-ad","channel":"WDR","topic":"Die Sendung mit der Maus","title":"Die Sendung mit der Maus vom 09.08.2026 (Audiodeskription)","description":"","timestamp":600,"duration":1800,"size":1,"url_website":"","url_subtitle":"","url_video":"https://example.test/ad.mp4","url_video_low":"","url_video_hd":""},
                  {"id":"latest-sign","channel":"WDR","topic":"Die Sendung mit der Maus","title":"Die Sendung mit der Maus vom 09.08.2026 (Gebärdensprache)","description":"","timestamp":600,"duration":1800,"size":1,"url_website":"","url_subtitle":"","url_video":"https://example.test/sign.mp4","url_video_low":"","url_video_hd":""},
                  {"id":"latest-standard","channel":"WDR","topic":"Die Sendung mit der Maus","title":"Die Sendung mit der Maus vom 09.08.2026","description":"","timestamp":600,"duration":1800,"size":1,"url_website":"","url_subtitle":"","url_video":"https://example.test/latest.mp4","url_video_low":"","url_video_hd":""},
                  {"id":"latest-standard-copy","channel":"ARD","topic":"Die Sendung mit der Maus","title":"Die Sendung mit der Maus vom 09.08.2026","description":"","timestamp":600,"duration":1800,"size":1,"url_website":"","url_subtitle":"","url_video":"https://example.test/latest.mp4","url_video_low":"","url_video_hd":""},
                  {"id":"older-ad","channel":"WDR","topic":"Die Sendung mit der Maus","title":"Die Sendung mit der Maus vom 02.08.2026 (Audiodeskription)","description":"","timestamp":500,"duration":1800,"size":1,"url_website":"","url_subtitle":"","url_video":"https://example.test/older-ad.mp4","url_video_low":"","url_video_hd":""},
                  {"id":"older-standard","channel":"WDR","topic":"Die Sendung mit der Maus","title":"Die Sendung mit der Maus vom 02.08.2026","description":"","timestamp":500,"duration":1800,"size":1,"url_website":"","url_subtitle":"","url_video":"https://example.test/older.mp4","url_video_low":"","url_video_hd":""},
                  {"id":"segment","channel":"WDR","topic":"Die Sendung mit der Maus","title":"Apfelstiel","description":"","timestamp":400,"duration":300,"size":1,"url_website":"","url_subtitle":"","url_video":"https://example.test/segment.mp4","url_video_low":"","url_video_hd":""}
                ],"queryInfo":{"filmlisteTimestamp":700,"searchEngineTime":"1","resultCount":7,"totalResults":7,"totalRelation":"eq","totalEntries":7}},"err":null}
                """));
        server.start();

        Path output = Files.createDirectory(tempDirectory.resolve("output"));
        HistoryStore history = new HistoryStore(tempDirectory.resolve("state/history.db"));
        MediathekViewWebClient client = new MediathekViewWebClient(
                URI.create("http://127.0.0.1:" + server.getAddress().getPort()), Duration.ofSeconds(5));
        SubscriptionService service = new SubscriptionService(
                client, new DownloadService(history, Duration.ofSeconds(5)), history);
        Subscription subscription = new Subscription(
                "Maus",
                List.of(new QueryClause(List.of("topic"), "Die Sendung mit der Maus")),
                "Maus",
                "HD",
                2,
                1200,
                null,
                false,
                true,
                "^Die Sendung mit der Maus vom",
                "\\((Audiodeskription|Gebärdensprache)\\)",
                null,
                null);

        var plan = service.plan(new SubscriptionConfig(output.toString(), List.of(subscription)));

        assertEquals(2, plan.matched());
        assertEquals(List.of("older-standard", "latest-standard"),
                plan.selections().stream().map(Model.SyncSelection::id).toList());
        assertTrue(plan.selections().stream().allMatch(item -> item.status().equals("would-download")));
        assertTrue(history.list(10).isEmpty());
        try (var children = Files.list(output)) {
            assertTrue(children.findAny().isEmpty());
        }
    }

    @Test
    void reportsInvalidRegexWithoutQuerying() throws Exception {
        Path output = Files.createDirectory(tempDirectory.resolve("output"));
        HistoryStore history = new HistoryStore(tempDirectory.resolve("state/history.db"));
        MediathekViewWebClient client = new MediathekViewWebClient(
                URI.create("http://127.0.0.1:1"), Duration.ofMillis(100));
        SubscriptionService service = new SubscriptionService(
                client, new DownloadService(history, Duration.ofSeconds(1)), history);
        Subscription subscription = new Subscription(
                "Broken",
                List.of(new QueryClause(List.of("topic"), "Maus")),
                "Maus", "HD", 1, null, null, false, true,
                "[", null, null, null);

        var plan = service.plan(new SubscriptionConfig(output.toString(), List.of(subscription)));

        assertTrue(plan.selections().isEmpty());
        assertEquals(1, plan.errors().size());
        assertTrue(plan.errors().get(0).contains("includeTitleRegex is invalid"));
    }

    private static void send(HttpExchange exchange, String json) throws IOException {
        assertEquals("POST", exchange.getRequestMethod());
        exchange.getRequestBody().readAllBytes();
        byte[] body = json.getBytes(StandardCharsets.UTF_8);
        exchange.getResponseHeaders().set("Content-Type", "application/json");
        exchange.sendResponseHeaders(200, body.length);
        exchange.getResponseBody().write(body);
        exchange.close();
    }
}
