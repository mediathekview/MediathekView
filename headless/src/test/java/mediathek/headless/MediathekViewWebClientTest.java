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
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.io.IOException;
import java.net.InetSocketAddress;
import java.net.URI;
import java.nio.charset.StandardCharsets;
import java.time.Duration;
import java.util.List;

import static mediathek.headless.Model.QueryClause;
import static mediathek.headless.Model.SearchRequest;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

class MediathekViewWebClientTest {
    private HttpServer server;
    private MediathekViewWebClient client;

    @BeforeEach
    void setUp() throws IOException {
        server = HttpServer.create(new InetSocketAddress("127.0.0.1", 0), 0);
        server.createContext("/api/query", exchange -> send(exchange, """
                {"result":{"results":[{
                  "id":"film-id","channel":"WDR","topic":"Die Maus","title":"Folge 1",
                  "description":"Test","timestamp":1700000000,"duration":1800,"size":123,
                  "url_website":"https://example.test/page","url_subtitle":"",
                  "url_video":"https://example.test/video.mp4","url_video_low":"","url_video_hd":""
                }],"queryInfo":{"filmlisteTimestamp":1700000100,"searchEngineTime":"1.25",
                  "resultCount":1,"totalResults":1,"totalRelation":"eq","totalEntries":700000}},"err":null}
                """));
        server.createContext("/api/entries", exchange -> send(exchange, """
                {"result":{"results":[{
                  "id":null,"channel":"WDR","topic":"Die Maus","title":"Folge 1",
                  "description":"Test","timestamp":1700000000,"duration":1800,"size":123,
                  "url_website":"https://example.test/page","url_subtitle":"",
                  "url_video":"https://example.test/video.mp4","url_video_low":"","url_video_hd":""
                }]},"err":null}
                """));
        server.start();
        URI baseUri = URI.create("http://127.0.0.1:" + server.getAddress().getPort());
        client = new MediathekViewWebClient(baseUri, Duration.ofSeconds(5));
    }

    @AfterEach
    void tearDown() {
        server.stop(0);
    }

    @Test
    void searchesAndMapsSnakeCaseFields() throws Exception {
        SearchRequest request = new SearchRequest(
                List.of(new QueryClause(List.of("title", "topic"), "Maus")),
                "timestamp", "desc", false, 0, 10, null, null);

        var result = client.search(request);

        assertEquals(1, result.results().size());
        assertEquals("film-id", result.results().get(0).id());
        assertEquals("https://example.test/video.mp4", result.results().get(0).videoUrl());
        assertEquals(700000, result.queryInfo().totalEntries());
    }

    @Test
    void resolvesExactEntryId() throws Exception {
        var film = client.entry("film-id");

        assertEquals("film-id", film.id());
        assertEquals("Folge 1", film.title());
    }

    private static void send(HttpExchange exchange, String json) throws IOException {
        assertEquals("POST", exchange.getRequestMethod());
        assertTrue(exchange.getRequestBody().readAllBytes().length > 0);
        byte[] body = json.getBytes(StandardCharsets.UTF_8);
        exchange.getResponseHeaders().set("Content-Type", "application/json");
        exchange.sendResponseHeaders(200, body.length);
        exchange.getResponseBody().write(body);
        exchange.close();
    }
}
