/*
 * Copyright (c) 2026 MediathekView contributors.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 */
package mediathek.headless;

import com.fasterxml.jackson.annotation.JsonProperty;

import java.util.List;
import java.util.Locale;

final class Model {
    private Model() {
    }

    record QueryClause(List<String> fields, String query) {
        QueryClause {
            fields = fields == null ? List.of() : List.copyOf(fields);
            query = query == null ? "" : query.trim();
        }
    }

    record SearchRequest(
            List<QueryClause> queries,
            String sortBy,
            String sortOrder,
            Boolean future,
            Integer offset,
            Integer size,
            @JsonProperty("duration_min") Integer durationMin,
            @JsonProperty("duration_max") Integer durationMax) {

        SearchRequest normalized() {
            int normalizedSize = size == null ? 15 : Math.max(1, Math.min(size, 1000));
            int normalizedOffset = offset == null ? 0 : Math.max(0, offset);
            return new SearchRequest(
                    queries == null ? List.of() : List.copyOf(queries),
                    sortBy == null ? "timestamp" : sortBy,
                    sortOrder == null ? "desc" : sortOrder,
                    future != null && future,
                    normalizedOffset,
                    normalizedSize,
                    durationMin,
                    durationMax);
        }
    }

    record Film(
            String id,
            String channel,
            String topic,
            String title,
            String description,
            long timestamp,
            int duration,
            long size,
            @JsonProperty("url_website") String websiteUrl,
            @JsonProperty("url_subtitle") String subtitleUrl,
            @JsonProperty("url_video") String videoUrl,
            @JsonProperty("url_video_low") String lowQualityUrl,
            @JsonProperty("url_video_hd") String highQualityUrl) {
    }

    record QueryInfo(
            long filmlisteTimestamp,
            String searchEngineTime,
            int resultCount,
            long totalResults,
            String totalRelation,
            long totalEntries) {
    }

    record SearchResult(List<Film> results, QueryInfo queryInfo) {
        SearchResult {
            results = results == null ? List.of() : List.copyOf(results);
        }
    }

    record ApiEnvelope<T>(T result, List<String> err) {
    }

    enum Quality {
        HD,
        SD,
        LOW;

        static Quality parse(String value) {
            if (value == null || value.isBlank()) {
                return HD;
            }
            return switch (value.trim().toLowerCase(Locale.ROOT)) {
                case "hd", "high" -> HD;
                case "sd", "normal" -> SD;
                case "low", "lq", "small" -> LOW;
                default -> throw new IllegalArgumentException("Unknown quality: " + value);
            };
        }

        String selectUrl(Film film) {
            return switch (this) {
                case HD -> firstNonBlank(film.highQualityUrl(), film.videoUrl(), film.lowQualityUrl());
                case SD -> firstNonBlank(film.videoUrl(), film.highQualityUrl(), film.lowQualityUrl());
                case LOW -> firstNonBlank(film.lowQualityUrl(), film.videoUrl(), film.highQualityUrl());
            };
        }

        private static String firstNonBlank(String... values) {
            for (String value : values) {
                if (value != null && !value.isBlank()) {
                    return value;
                }
            }
            return "";
        }
    }

    record SubscriptionConfig(String outputDirectory, List<Subscription> subscriptions) {
        SubscriptionConfig {
            subscriptions = subscriptions == null ? List.of() : List.copyOf(subscriptions);
        }
    }

    record Subscription(
            String name,
            List<QueryClause> queries,
            String subdirectory,
            String quality,
            Integer maxResults,
            Integer minDuration,
            Integer maxDuration,
            Boolean includeFuture,
            Boolean subtitles,
            String includeTitleRegex,
            String excludeTitleRegex,
            String includeTopicRegex,
            String excludeTopicRegex) {

        Subscription {
            queries = queries == null ? List.of() : List.copyOf(queries);
        }

        Quality parsedQuality() {
            return Quality.parse(quality);
        }

        int resultLimit() {
            return maxResults == null ? 10 : Math.max(1, Math.min(maxResults, 1000));
        }

        boolean wantsSubtitles() {
            return subtitles == null || subtitles;
        }
    }

    record DownloadRecord(
            String id,
            String status,
            String channel,
            String topic,
            String title,
            String quality,
            String outputPath,
            String sourceUrl,
            String startedAt,
            String completedAt,
            String error) {
    }

    record SyncResult(int subscriptions, int matched, int downloaded, int skipped, List<String> errors) {
        SyncResult {
            errors = errors == null ? List.of() : List.copyOf(errors);
        }
    }

    record SyncSelection(
            String subscription,
            String status,
            String id,
            String channel,
            String topic,
            String title,
            long timestamp,
            int duration) {
    }

    record SyncPlan(int subscriptions, int matched, int skipped,
                    List<SyncSelection> selections, List<String> errors) {
        SyncPlan {
            selections = selections == null ? List.of() : List.copyOf(selections);
            errors = errors == null ? List.of() : List.copyOf(errors);
        }
    }

    record ApiDownloadRequest(String id, String subdirectory, String quality, Boolean subtitles) {
    }
}
