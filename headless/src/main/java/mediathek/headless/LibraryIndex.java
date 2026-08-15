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
import java.nio.file.Files;
import java.nio.file.Path;
import java.text.Normalizer;
import java.util.HashMap;
import java.util.Locale;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.regex.Pattern;
import java.util.stream.Stream;

import static mediathek.headless.Model.Film;

final class LibraryIndex {
    private static final Set<String> VIDEO_EXTENSIONS = Set.of(
            ".avi", ".m4v", ".mkv", ".mov", ".mp4", ".ts", ".webm");
    private static final Pattern LEGACY_ID = Pattern.compile("-(-?\\d{10})$");
    private static final Pattern SHORT_ID = Pattern.compile("\\s+\\[[0-9a-fA-F]{8}]$");
    private static final Pattern SOURCE_FINGERPRINT = Pattern.compile(
            "\\s+\\[src-([0-9a-fA-F]{32})]$");
    private static final Pattern DATE_PREFIX = Pattern.compile("^\\d{4}-\\d{2}-\\d{2}\\s+-\\s+");
    private static final Pattern CONTENT_PREFIX = Pattern.compile(
            "^(?:Sachgeschichte|Lachgeschichte|MausSpezial)\\s*[_:·-]\\s*",
            Pattern.CASE_INSENSITIVE | Pattern.UNICODE_CASE);
    private static final Pattern SPECIAL_PREFIX = Pattern.compile(
            "^Spezial\\s*[-_:]\\s*", Pattern.CASE_INSENSITIVE | Pattern.UNICODE_CASE);
    private static final Pattern NON_ALPHANUMERIC = Pattern.compile("[^\\p{L}\\p{N}]+");

    private final Path root;
    private final Map<EpisodeKey, Path> videosByEpisode = new HashMap<>();

    private LibraryIndex(Path root) {
        this.root = root.toAbsolutePath().normalize();
    }

    static LibraryIndex scan(Path root) throws IOException {
        LibraryIndex index = new LibraryIndex(root);
        try (Stream<Path> paths = Files.walk(index.root)) {
            paths.filter(Files::isRegularFile).forEach(index::add);
        }
        return index;
    }

    Optional<Path> find(Film film) {
        String title = normalize(film.title());
        for (String sourceUrl : new String[]{film.videoUrl(), film.lowQualityUrl(), film.highQualityUrl()}) {
            if (sourceUrl == null || sourceUrl.isBlank()) {
                continue;
            }
            Path sourceMatch = videosByEpisode.get(new EpisodeKey(
                    title, "source:" + EpisodeIdentity.sourceFingerprint(sourceUrl)));
            if (sourceMatch != null) {
                return Optional.of(sourceMatch);
            }
            Path legacyMatch = videosByEpisode.get(new EpisodeKey(
                    title, "legacy:" + EpisodeIdentity.legacySourceHash(sourceUrl)));
            if (legacyMatch != null) {
                return Optional.of(legacyMatch);
            }
        }
        return Optional.empty();
    }

    void add(Path path) {
        Path absolute = path.toAbsolutePath().normalize();
        if (!absolute.startsWith(root) || !isCompletedVideo(absolute)) {
            return;
        }

        String stem = withoutExtension(absolute.getFileName().toString());
        stem = SHORT_ID.matcher(stem).replaceFirst("");

        String identity;
        var sourceMatcher = SOURCE_FINGERPRINT.matcher(stem);
        if (sourceMatcher.find()) {
            identity = "source:" + sourceMatcher.group(1).toLowerCase(Locale.ROOT);
            stem = sourceMatcher.replaceFirst("");
        }
        else {
            var legacyMatcher = LEGACY_ID.matcher(stem);
            if (!legacyMatcher.find()) {
                return;
            }
            identity = "legacy:" + legacyMatcher.group(1);
            stem = legacyMatcher.replaceFirst("");
        }

        stem = DATE_PREFIX.matcher(stem).replaceFirst("");
        addTitle(stem, identity, absolute);
        addTitle(stripContentPrefix(stem), identity, absolute);

        Path relative = root.relativize(absolute);
        Path parent = relative.getParent();
        if (parent != null) {
            String directory = parent.getFileName().toString();
            String prefix = directory + "-";
            if (stem.regionMatches(true, 0, prefix, 0, prefix.length())) {
                String title = stem.substring(prefix.length());
                addTitle(title, identity, absolute);
                addTitle(stripContentPrefix(title), identity, absolute);
            }
        }
    }

    private void addTitle(String title, String identity, Path path) {
        String normalizedTitle = normalize(title);
        if (!normalizedTitle.isBlank()) {
            videosByEpisode.putIfAbsent(new EpisodeKey(normalizedTitle, identity), path);
        }
    }

    private static String stripContentPrefix(String value) {
        String stripped = CONTENT_PREFIX.matcher(value).replaceFirst("");
        return SPECIAL_PREFIX.matcher(stripped).replaceFirst("");
    }

    private static boolean isCompletedVideo(Path path) {
        String name = path.getFileName().toString().toLowerCase(Locale.ROOT);
        if (name.contains(".part.") || name.endsWith(".part")) {
            return false;
        }
        return VIDEO_EXTENSIONS.stream().anyMatch(name::endsWith);
    }

    private static String withoutExtension(String filename) {
        int dot = filename.lastIndexOf('.');
        return dot > 0 ? filename.substring(0, dot) : filename;
    }

    static String normalize(String value) {
        String normalized = Normalizer.normalize(value == null ? "" : value, Normalizer.Form.NFKC)
                .toLowerCase(Locale.ROOT);
        return NON_ALPHANUMERIC.matcher(normalized).replaceAll(" ").trim();
    }

    private record EpisodeKey(String title, String identity) {
    }
}
