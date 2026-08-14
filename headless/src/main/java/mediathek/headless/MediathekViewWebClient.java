/*
 * Copyright (c) 2026 MediathekView contributors.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 */
package mediathek.headless;

import com.fasterxml.jackson.core.type.TypeReference;

import java.io.IOException;
import java.net.URI;
import java.net.http.HttpClient;
import java.net.http.HttpRequest;
import java.net.http.HttpResponse;
import java.time.Duration;
import java.util.List;

import static mediathek.headless.Model.ApiEnvelope;
import static mediathek.headless.Model.Film;
import static mediathek.headless.Model.SearchRequest;
import static mediathek.headless.Model.SearchResult;

final class MediathekViewWebClient {
    static final URI DEFAULT_BASE_URI = URI.create("https://mediathekviewweb.de");

    private final URI baseUri;
    private final HttpClient httpClient;
    private final Duration timeout;

    MediathekViewWebClient(URI baseUri, Duration timeout) {
        this(baseUri, timeout, HttpClient.newBuilder()
                .connectTimeout(timeout)
                .followRedirects(HttpClient.Redirect.NORMAL)
                .build());
    }

    MediathekViewWebClient(URI baseUri, Duration timeout, HttpClient httpClient) {
        String normalized = baseUri.toString().replaceFirst("/+$", "");
        this.baseUri = URI.create(normalized);
        this.timeout = timeout;
        this.httpClient = httpClient;
    }

    SearchResult search(SearchRequest request) throws IOException, InterruptedException {
        HttpRequest httpRequest = jsonPost("/api/query", JsonSupport.write(request.normalized()));
        ApiEnvelope<SearchResult> response = send(httpRequest, new TypeReference<>() {
        });
        return requireResult(response);
    }

    List<Film> entries(List<String> ids) throws IOException, InterruptedException {
        if (ids == null || ids.isEmpty()) {
            return List.of();
        }
        HttpRequest httpRequest = jsonPost("/api/entries", JsonSupport.write(ids));
        ApiEnvelope<SearchResult> response = send(httpRequest, new TypeReference<>() {
        });
        return requireResult(response).results();
    }

    Film entry(String id) throws IOException, InterruptedException {
        List<Film> films = entries(List.of(id));
        if (films.isEmpty()) {
            throw new IOException("No MediathekView entry found for id " + id);
        }
        return films.get(0);
    }

    private HttpRequest jsonPost(String path, String json) {
        return HttpRequest.newBuilder(baseUri.resolve(path))
                .timeout(timeout)
                .header("Accept", "application/json")
                .header("Content-Type", "application/json")
                .header("User-Agent", "MediathekView-Headless/0.1")
                .POST(HttpRequest.BodyPublishers.ofString(json))
                .build();
    }

    private <T> T send(HttpRequest request, TypeReference<T> type) throws IOException, InterruptedException {
        HttpResponse<String> response = httpClient.send(request, HttpResponse.BodyHandlers.ofString());
        if (response.statusCode() < 200 || response.statusCode() >= 300) {
            throw new IOException("MediathekViewWeb returned HTTP " + response.statusCode() + ": " + response.body());
        }
        return JsonSupport.MAPPER.readValue(response.body(), type);
    }

    private static <T> T requireResult(ApiEnvelope<T> envelope) throws IOException {
        if (envelope == null) {
            throw new IOException("MediathekViewWeb returned an empty response");
        }
        if (envelope.err() != null && !envelope.err().isEmpty()) {
            throw new IOException("MediathekViewWeb error: " + String.join("; ", envelope.err()));
        }
        if (envelope.result() == null) {
            throw new IOException("MediathekViewWeb response did not contain a result");
        }
        return envelope.result();
    }
}
