/*
 * Copyright (c) 2026 MediathekView contributors.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 */
package mediathek.headless;

import picocli.CommandLine;

import java.net.URI;
import java.nio.file.Path;
import java.time.Duration;
import java.time.Instant;
import java.time.ZoneId;
import java.time.format.DateTimeFormatter;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.Callable;

import static mediathek.headless.Model.DownloadRecord;
import static mediathek.headless.Model.Film;
import static mediathek.headless.Model.Quality;
import static mediathek.headless.Model.QueryClause;
import static mediathek.headless.Model.SearchRequest;
import static mediathek.headless.Model.SearchResult;
import static mediathek.headless.Model.SubscriptionConfig;
import static mediathek.headless.Model.SyncPlan;
import static mediathek.headless.Model.SyncResult;

@CommandLine.Command(
        name = "mediathekview-headless",
        description = "Search and download public-broadcast media without a graphical session.",
        mixinStandardHelpOptions = true,
        version = "MediathekView Headless 0.1.0",
        subcommands = {
                HeadlessMain.SearchCommand.class,
                HeadlessMain.ShowCommand.class,
                HeadlessMain.DownloadCommand.class,
                HeadlessMain.SyncCommand.class,
                HeadlessMain.HistoryCommand.class,
                HeadlessMain.ServeCommand.class
        })
public final class HeadlessMain implements Runnable {
    @CommandLine.Option(
            names = "--base-url",
            defaultValue = "https://mediathekviewweb.de",
            description = "MediathekViewWeb base URL (default: ${DEFAULT-VALUE}).")
    URI baseUrl;

    @CommandLine.Option(
            names = "--state",
            defaultValue = "${sys:user.home}/.local/state/mediathekview-headless/history.db",
            description = "SQLite download-history path (default: ${DEFAULT-VALUE}).")
    Path statePath;

    @CommandLine.Option(
            names = "--timeout",
            defaultValue = "30",
            description = "HTTP connection/request timeout in seconds (default: ${DEFAULT-VALUE}).")
    int timeoutSeconds;

    public static void main(String[] args) {
        CommandLine commandLine = new CommandLine(new HeadlessMain());
        commandLine.setCaseInsensitiveEnumValuesAllowed(true);
        int exitCode = commandLine.execute(args);
        System.exit(exitCode);
    }

    @Override
    public void run() {
        CommandLine.usage(this, System.out);
    }

    MediathekViewWebClient client() {
        return new MediathekViewWebClient(baseUrl, timeout());
    }

    HistoryStore history() throws Exception {
        return new HistoryStore(statePath);
    }

    Duration timeout() {
        return Duration.ofSeconds(Math.max(1, timeoutSeconds));
    }

    @CommandLine.Command(name = "search", description = "Search the current MediathekView film list.")
    static final class SearchCommand implements Callable<Integer> {
        @CommandLine.ParentCommand
        HeadlessMain parent;

        @CommandLine.Parameters(index = "0", arity = "0..1", paramLabel = "QUERY",
                description = "Text to match in title or topic.")
        String query;

        @CommandLine.Option(names = "--channel", description = "Require text in the channel field.")
        String channel;

        @CommandLine.Option(names = "--topic", description = "Require text in the topic field.")
        String topic;

        @CommandLine.Option(names = "--title", description = "Require text in the title field.")
        String title;

        @CommandLine.Option(names = "--description", description = "Require text in the description field.")
        String description;

        @CommandLine.Option(names = "--min-duration", description = "Minimum duration in seconds.")
        Integer minDuration;

        @CommandLine.Option(names = "--max-duration", description = "Maximum duration in seconds.")
        Integer maxDuration;

        @CommandLine.Option(names = "--limit", defaultValue = "20",
                description = "Maximum results, up to 1000 (default: ${DEFAULT-VALUE}).")
        int limit;

        @CommandLine.Option(names = "--offset", defaultValue = "0", description = "Result offset.")
        int offset;

        @CommandLine.Option(names = "--sort", defaultValue = "timestamp",
                description = "Sort field: timestamp, duration, channel, topic, or title.")
        String sort;

        @CommandLine.Option(names = "--order", defaultValue = "desc",
                description = "Sort order: asc or desc.")
        String order;

        @CommandLine.Option(names = "--future", description = "Include entries scheduled in the future.")
        boolean future;

        @CommandLine.Option(names = "--json", description = "Print machine-readable JSON.")
        boolean json;

        @Override
        public Integer call() throws Exception {
            SearchResult result = parent.client().search(buildRequest());
            if (json) {
                System.out.println(JsonSupport.writePretty(result));
            }
            else {
                printFilms(result.results());
                System.err.printf("%d of %d result(s)%n", result.results().size(), result.queryInfo().totalResults());
            }
            return 0;
        }

        private SearchRequest buildRequest() {
            List<QueryClause> clauses = new ArrayList<>();
            addClause(clauses, List.of("title", "topic"), query);
            addClause(clauses, List.of("channel"), channel);
            addClause(clauses, List.of("topic"), topic);
            addClause(clauses, List.of("title"), title);
            addClause(clauses, List.of("description"), description);
            return new SearchRequest(clauses, sort, order, future, offset, limit, minDuration, maxDuration);
        }
    }

    @CommandLine.Command(name = "show", description = "Resolve one or more exact entry IDs.")
    static final class ShowCommand implements Callable<Integer> {
        @CommandLine.ParentCommand
        HeadlessMain parent;

        @CommandLine.Option(names = "--id", required = true, arity = "1..*", description = "Entry ID(s).")
        List<String> ids;

        @CommandLine.Option(names = "--json", description = "Print machine-readable JSON.")
        boolean json;

        @Override
        public Integer call() throws Exception {
            List<Film> films = parent.client().entries(ids);
            if (json) {
                System.out.println(JsonSupport.writePretty(films));
            }
            else {
                printFilms(films);
            }
            return films.isEmpty() ? 4 : 0;
        }
    }

    @CommandLine.Command(name = "download", description = "Download one exact MediathekView entry.")
    static final class DownloadCommand implements Callable<Integer> {
        @CommandLine.ParentCommand
        HeadlessMain parent;

        @CommandLine.Option(names = "--id", required = true, description = "Exact entry ID from search.")
        String id;

        @CommandLine.Option(names = "--output", required = true,
                description = "Existing writable output root (a subdirectory is created below it).")
        Path output;

        @CommandLine.Option(names = "--subdirectory",
                description = "Folder below the output root (default: entry topic).")
        String subdirectory;

        @CommandLine.Option(names = "--quality", defaultValue = "HD",
                description = "Preferred quality: HD, SD, or LOW (default: ${DEFAULT-VALUE}).")
        Quality quality;

        @CommandLine.Option(names = "--subtitles", negatable = true, defaultValue = "true",
                description = "Download subtitles when available (default: ${DEFAULT-VALUE}).")
        boolean subtitles;

        @CommandLine.Option(names = "--force", description = "Download even when the ID is in completed history.")
        boolean force;

        @Override
        public Integer call() throws Exception {
            Film film = parent.client().entry(id);
            HistoryStore history = parent.history();
            DownloadService service = new DownloadService(history, parent.timeout());
            Path path = service.download(film, output.toAbsolutePath().normalize(), subdirectory, quality, subtitles, force);
            if (path == null) {
                System.out.println("Already downloaded: " + id);
            }
            else {
                System.out.println(path);
            }
            return 0;
        }
    }

    @CommandLine.Command(name = "sync", description = "Run all configured subscriptions once.")
    static final class SyncCommand implements Callable<Integer> {
        @CommandLine.ParentCommand
        HeadlessMain parent;

        @CommandLine.Option(names = "--config", required = true, description = "Subscription JSON file.")
        Path configPath;

        @CommandLine.Option(names = "--json", description = "Print machine-readable JSON.")
        boolean json;

        @CommandLine.Option(names = "--dry-run", description = "Show selected entries without downloading them.")
        boolean dryRun;

        @Override
        public Integer call() throws Exception {
            SubscriptionConfig config = SubscriptionService.readConfig(configPath);
            HistoryStore history = parent.history();
            DownloadService downloads = new DownloadService(history, parent.timeout());
            SubscriptionService subscriptions = new SubscriptionService(parent.client(), downloads, history);
            if (dryRun) {
                SyncPlan plan = subscriptions.plan(config);
                if (json) {
                    System.out.println(JsonSupport.writePretty(plan));
                }
                else {
                    plan.selections().forEach(selection -> System.out.printf(
                            "%-18s  %-28s  %-52s  %s%n",
                            selection.status(),
                            abbreviate(selection.subscription(), 28),
                            abbreviate(selection.title(), 52),
                            selection.id()));
                    System.out.printf("subscriptions=%d matched=%d skipped=%d errors=%d%n",
                            plan.subscriptions(), plan.matched(), plan.skipped(), plan.errors().size());
                    plan.errors().forEach(error -> System.err.println("ERROR: " + error));
                }
                return plan.errors().isEmpty() ? 0 : 2;
            }

            SyncResult result = subscriptions.sync(config);
            if (json) {
                System.out.println(JsonSupport.writePretty(result));
            }
            else {
                System.out.printf("subscriptions=%d matched=%d downloaded=%d skipped=%d errors=%d%n",
                        result.subscriptions(), result.matched(), result.downloaded(), result.skipped(), result.errors().size());
                result.errors().forEach(error -> System.err.println("ERROR: " + error));
            }
            return result.errors().isEmpty() ? 0 : 2;
        }
    }

    @CommandLine.Command(name = "history", description = "Show recent download history.")
    static final class HistoryCommand implements Callable<Integer> {
        @CommandLine.ParentCommand
        HeadlessMain parent;

        @CommandLine.Option(names = "--limit", defaultValue = "100", description = "Maximum rows.")
        int limit;

        @CommandLine.Option(names = "--json", description = "Print machine-readable JSON.")
        boolean json;

        @Override
        public Integer call() throws Exception {
            List<DownloadRecord> records = parent.history().list(limit);
            if (json) {
                System.out.println(JsonSupport.writePretty(records));
            }
            else {
                for (DownloadRecord record : records) {
                    System.out.printf("%-11s  %-20s  %s%n", record.status(), abbreviate(record.id(), 20), record.outputPath());
                }
            }
            return 0;
        }
    }

    @CommandLine.Command(name = "serve", description = "Run the local headless HTTP API.")
    static final class ServeCommand implements Callable<Integer> {
        @CommandLine.ParentCommand
        HeadlessMain parent;

        @CommandLine.Option(names = "--bind", defaultValue = "127.0.0.1",
                description = "Bind address (default: ${DEFAULT-VALUE}). No authentication is provided.")
        String bind;

        @CommandLine.Option(names = "--port", defaultValue = "7070",
                description = "Listen port (default: ${DEFAULT-VALUE}).")
        int port;

        @CommandLine.Option(names = "--config",
                description = "Subscription JSON; required for API downloads and sync.")
        Path configPath;

        @Override
        public Integer call() throws Exception {
            SubscriptionConfig config = configPath == null ? null : SubscriptionService.readConfig(configPath);
            HistoryStore history = parent.history();
            DownloadService downloads = new DownloadService(history, parent.timeout());
            MediathekViewWebClient client = parent.client();
            SubscriptionService subscriptions = new SubscriptionService(client, downloads, history);
            HeadlessHttpServer server = new HeadlessHttpServer(
                    bind, port, client, downloads, history, subscriptions, config);
            Runtime.getRuntime().addShutdownHook(new Thread(server::close, "headless-api-shutdown"));
            server.start();
            System.out.printf("MediathekView Headless API listening on http://%s:%d%n", bind, port);
            server.await();
            return 0;
        }
    }

    private static void addClause(List<QueryClause> clauses, List<String> fields, String value) {
        if (value != null && !value.isBlank()) {
            clauses.add(new QueryClause(fields, value));
        }
    }

    private static void printFilms(List<Film> films) {
        DateTimeFormatter formatter = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm")
                .withZone(ZoneId.systemDefault());
        for (Film film : films) {
            System.out.printf("%s  %-8s  %-52s  %s%n",
                    formatter.format(Instant.ofEpochSecond(film.timestamp())),
                    abbreviate(film.channel(), 8),
                    abbreviate(film.title(), 52),
                    film.id());
        }
    }

    private static String abbreviate(String value, int maxLength) {
        if (value == null) {
            return "";
        }
        if (value.length() <= maxLength) {
            return value;
        }
        return value.substring(0, Math.max(1, maxLength - 1)) + "…";
    }
}
