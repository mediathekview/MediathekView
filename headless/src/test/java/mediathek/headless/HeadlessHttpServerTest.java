/*
 * Copyright (c) 2026 MediathekView contributors.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 */
package mediathek.headless;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import java.net.URI;
import java.net.http.HttpClient;
import java.net.http.HttpRequest;
import java.net.http.HttpResponse;
import java.nio.file.Path;
import java.time.Duration;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;

class HeadlessHttpServerTest {
    @TempDir
    Path tempDirectory;

    @Test
    void rejectsDownloadsWhenOutputDirectoryIsBlank() throws Exception {
        HistoryStore history = new HistoryStore(tempDirectory.resolve("state/history.db"));
        MediathekViewWebClient client = new MediathekViewWebClient(
                URI.create("http://127.0.0.1:1"), Duration.ofMillis(100));
        DownloadService downloads = new DownloadService(history, Duration.ofSeconds(1));
        SubscriptionService subscriptions = new SubscriptionService(client, downloads, history);
        Model.SubscriptionConfig config = new Model.SubscriptionConfig("", List.of());

        try (HeadlessHttpServer server = new HeadlessHttpServer(
                "127.0.0.1", 0, client, downloads, history, subscriptions, config)) {
            server.start();
            HttpRequest request = HttpRequest.newBuilder(
                            URI.create("http://127.0.0.1:" + server.port() + "/api/downloads"))
                    .header("Content-Type", "application/json")
                    .POST(HttpRequest.BodyPublishers.ofString("{\"id\":\"must-not-resolve\"}"))
                    .build();

            HttpResponse<String> response = HttpClient.newHttpClient().send(
                    request, HttpResponse.BodyHandlers.ofString());

            assertEquals(409, response.statusCode());
            assertEquals("Server has no subscription config/outputDirectory",
                    JsonSupport.MAPPER.readTree(response.body()).get("error").asText());
        }
    }
}
