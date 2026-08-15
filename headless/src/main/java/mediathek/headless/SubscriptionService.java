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
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.regex.Pattern;
import java.util.regex.PatternSyntaxException;

import static mediathek.headless.Model.Film;
import static mediathek.headless.Model.SearchRequest;
import static mediathek.headless.Model.SearchResult;
import static mediathek.headless.Model.Subscription;
import static mediathek.headless.Model.SubscriptionConfig;
import static mediathek.headless.Model.SyncPlan;
import static mediathek.headless.Model.SyncResult;
import static mediathek.headless.Model.SyncSelection;

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
        Path outputRoot = outputRoot(config);
        LibraryIndex library = LibraryIndex.scan(outputRoot);
        SyncPlan plan = plan(config, library);
        if (!plan.errors().isEmpty()) {
            return new SyncResult(plan.subscriptions(), plan.matched(), 0, plan.skipped(), plan.errors());
        }

        int downloaded = 0;
        int skipped = plan.skipped();
        List<String> errors = new ArrayList<>();

        for (SyncSelection selection : plan.selections()) {
            if (!selection.status().equals("would-download")) {
                continue;
            }
            Subscription subscription = config.subscriptions().stream()
                    .filter(candidate -> candidate.name().equals(selection.subscription()))
                    .findFirst()
                    .orElseThrow();
            try {
                Film film = client.entry(selection.id());
                Path path = downloads.download(
                        film,
                        outputRoot,
                        subscription.subdirectory(),
                        subscription.parsedQuality(),
                        subscription.wantsSubtitles(),
                        false,
                        library);
                if (path == null) {
                    skipped++;
                }
                else {
                    downloaded++;
                }
            }
            catch (Exception exception) {
                errors.add(subscription.name() + " / " + selection.title() + ": " + compactError(exception));
            }
        }

        return new SyncResult(plan.subscriptions(), plan.matched(), downloaded, skipped, List.copyOf(errors));
    }

    SyncPlan plan(SubscriptionConfig config) throws Exception {
        Path outputRoot = outputRoot(config);
        return plan(config, LibraryIndex.scan(outputRoot));
    }

    private SyncPlan plan(SubscriptionConfig config, LibraryIndex library) throws Exception {
        List<String> duplicateNameErrors = duplicateNameErrors(config.subscriptions());
        if (!duplicateNameErrors.isEmpty()) {
            return new SyncPlan(
                    config.subscriptions().size(), 0, 0,
                    List.of(), duplicateNameErrors);
        }

        int matched = 0;
        int skipped = 0;
        List<SyncSelection> selections = new ArrayList<>();
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

            List<Film> films;
            try {
                films = findMatches(subscription);
            }
            catch (Exception exception) {
                errors.add(subscription.name() + ": search failed: " + compactError(exception));
                continue;
            }
            matched += films.size();
            Collections.reverse(films);

            for (Film film : films) {
                String sourceUrl = subscription.parsedQuality().selectUrl(film);
                boolean downloaded = history.isCompleted(film.id()) || history.isCompletedSource(sourceUrl);
                boolean inLibrary = !downloaded && library.find(film).isPresent();
                if (downloaded || inLibrary) {
                    skipped++;
                }
                selections.add(new SyncSelection(
                        subscription.name(),
                        downloaded ? "already-downloaded" : inLibrary ? "already-in-library" : "would-download",
                        film.id(),
                        film.channel(),
                        film.topic(),
                        film.title(),
                        film.timestamp(),
                        film.duration()));
            }
        }

        return new SyncPlan(
                config.subscriptions().size(), matched, skipped,
                List.copyOf(selections), List.copyOf(errors));
    }

    private List<Film> findMatches(Subscription subscription) throws Exception {
        FilmFilter filter = FilmFilter.compile(subscription);
        int resultLimit = subscription.resultLimit();
        int pageSize = Math.min(1000, Math.max(50, resultLimit * 5));
        int offset = 0;
        List<Film> matches = new ArrayList<>();
        Set<String> sources = new HashSet<>();

        while (matches.size() < resultLimit) {
            SearchRequest request = new SearchRequest(
                    subscription.queries(),
                    "timestamp",
                    "desc",
                    subscription.includeFuture() != null && subscription.includeFuture(),
                    offset,
                    pageSize,
                    subscription.minDuration(),
                    subscription.maxDuration());
            SearchResult result = client.search(request);
            for (Film film : result.results()) {
                if (filter.matches(film) && sources.add(contentKey(subscription, film))) {
                    matches.add(film);
                    if (matches.size() == resultLimit) {
                        break;
                    }
                }
            }

            int received = result.results().size();
            if (received == 0) {
                break;
            }
            offset += received;
            if (received < pageSize
                    || result.queryInfo() != null && offset >= result.queryInfo().totalResults()) {
                break;
            }
        }
        return matches;
    }

    private static String contentKey(Subscription subscription, Film film) {
        String sourceUrl = subscription.parsedQuality().selectUrl(film);
        if (sourceUrl != null && !sourceUrl.isBlank()) {
            return "url:" + sourceUrl;
        }
        return "metadata:" + film.topic() + '\n' + film.title() + '\n'
                + film.timestamp() + '\n' + film.duration();
    }

    private static List<String> duplicateNameErrors(List<Subscription> subscriptions) {
        Set<String> names = new HashSet<>();
        Set<String> duplicates = new HashSet<>();
        List<String> errors = new ArrayList<>();
        for (Subscription subscription : subscriptions) {
            String name = subscription.name();
            if (name != null && !name.isBlank() && !names.add(name) && duplicates.add(name)) {
                errors.add("Duplicate subscription name: " + name);
            }
        }
        return List.copyOf(errors);
    }

    private static Path outputRoot(SubscriptionConfig config) {
        if (config.outputDirectory() == null || config.outputDirectory().isBlank()) {
            throw new IllegalArgumentException("Subscription config requires outputDirectory");
        }
        return Path.of(config.outputDirectory()).toAbsolutePath().normalize();
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

    private record FilmFilter(
            Pattern includeTitle,
            Pattern excludeTitle,
            Pattern includeTopic,
            Pattern excludeTopic) {

        static FilmFilter compile(Subscription subscription) {
            return new FilmFilter(
                    compile("includeTitleRegex", subscription.includeTitleRegex()),
                    compile("excludeTitleRegex", subscription.excludeTitleRegex()),
                    compile("includeTopicRegex", subscription.includeTopicRegex()),
                    compile("excludeTopicRegex", subscription.excludeTopicRegex()));
        }

        boolean matches(Film film) {
            return included(includeTitle, film.title())
                    && excluded(excludeTitle, film.title())
                    && included(includeTopic, film.topic())
                    && excluded(excludeTopic, film.topic());
        }

        private static boolean included(Pattern pattern, String value) {
            return pattern == null || pattern.matcher(value == null ? "" : value).find();
        }

        private static boolean excluded(Pattern pattern, String value) {
            return pattern == null || !pattern.matcher(value == null ? "" : value).find();
        }

        private static Pattern compile(String field, String value) {
            if (value == null || value.isBlank()) {
                return null;
            }
            try {
                return Pattern.compile(value, Pattern.CASE_INSENSITIVE | Pattern.UNICODE_CASE);
            }
            catch (PatternSyntaxException exception) {
                throw new IllegalArgumentException(field + " is invalid: " + exception.getDescription(), exception);
            }
        }
    }
}
