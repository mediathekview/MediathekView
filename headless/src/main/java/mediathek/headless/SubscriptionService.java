/*
 * Copyright (c) 2026 MediathekView contributors.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 */
package mediathek.headless;

import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import static mediathek.headless.Model.Film;
import static mediathek.headless.Model.SearchRequest;
import static mediathek.headless.Model.SearchResult;
import static mediathek.headless.Model.Subscription;
import static mediathek.headless.Model.SubscriptionConfig;
import static mediathek.headless.Model.SyncResult;

final class SubscriptionService {
    private final MediathekViewWebClient client;
    private final DownloadService downloads;
    private final HistoryStore history;

    SubscriptionService(MediathekViewWebClient client, DownloadService downloads, HistoryStore history) {
        this.client = client;
        this.downloads = downloads;
        this.history = history;
    }

    SyncResult sync(SubscriptionConfig config) throws Exception {
        if (config.outputDirectory() == null || config.outputDirectory().isBlank()) {
            throw new IllegalArgumentException("Subscription config requires outputDirectory");
        }
        Path outputRoot = Path.of(config.outputDirectory()).toAbsolutePath().normalize();
        int matched = 0;
        int downloaded = 0;
        int skipped = 0;
        List<String> errors = new ArrayList<>();

        for (Subscription subscription : config.subscriptions()) {
            if (subscription.name() == null || subscription.name().isBlank()) {
                errors.add("Subscription without a name was skipped");
                continue;
            }
            if (subscription.queries().isEmpty()) {
                errors.add(subscription.name() + ": no queries configured");
                continue;
            }

            SearchRequest request = new SearchRequest(
                    subscription.queries(),
                    "timestamp",
                    "desc",
                    subscription.includeFuture() != null && subscription.includeFuture(),
                    0,
                    subscription.resultLimit(),
                    subscription.minDuration(),
                    subscription.maxDuration());

            SearchResult result;
            try {
                result = client.search(request);
            }
            catch (Exception exception) {
                errors.add(subscription.name() + ": search failed: " + compactError(exception));
                continue;
            }
            matched += result.results().size();
            List<Film> films = new ArrayList<>(result.results());
            Collections.reverse(films);

            for (Film film : films) {
                try {
                    if (history.isCompleted(film.id())) {
                        skipped++;
                        continue;
                    }
                    Path path = downloads.download(
                            film,
                            outputRoot,
                            subscription.subdirectory(),
                            subscription.parsedQuality(),
                            subscription.wantsSubtitles(),
                            false);
                    if (path == null) {
                        skipped++;
                    }
                    else {
                        downloaded++;
                    }
                }
                catch (Exception exception) {
                    errors.add(subscription.name() + " / " + film.title() + ": " + compactError(exception));
                }
            }
        }

        return new SyncResult(config.subscriptions().size(), matched, downloaded, skipped, List.copyOf(errors));
    }

    static SubscriptionConfig readConfig(Path path) throws Exception {
        return JsonSupport.read(path.toAbsolutePath().normalize(), SubscriptionConfig.class);
    }

    private static String compactError(Throwable error) {
        String message = error.getMessage();
        if (message == null || message.isBlank()) {
            return error.getClass().getSimpleName();
        }
        return message.length() > 500 ? message.substring(0, 500) : message;
    }
}
