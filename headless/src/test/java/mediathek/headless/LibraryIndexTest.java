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

import java.nio.file.Files;
import java.nio.file.Path;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

class LibraryIndexTest {
    @TempDir
    Path tempDirectory;

    @Test
    void recognizesLegacyProgramPrefixesAndIds() throws Exception {
        String sourceUrl = "https://example.test/video.mp4";
        Path directory = Files.createDirectories(
                tempDirectory.resolve("Anna, Nina, Pia und die wilden Tiere"));
        Path video = Files.writeString(directory.resolve(
                "Anna, Nina, Pia und die wilden Tiere-Die Raubkatzen von Brasilien-0557530028.mp4"),
                "video");

        LibraryIndex index = LibraryIndex.scan(tempDirectory);

        assertEquals(video, index.find(film("Die Raubkatzen von Brasilien", sourceUrl)).orElseThrow());
    }

    @Test
    void recognizesDatedAndContentCategoryFilenames() throws Exception {
        String datedUrl = "https://example.test/heilige-birma.mp4";
        Path datedDirectory = Files.createDirectories(tempDirectory.resolve("Anna und die Haustiere"));
        Path dated = Files.writeString(
                datedDirectory.resolve("2026-08-08 - Heilige Birma [src-"
                        + EpisodeIdentity.sourceFingerprint(datedUrl) + "].mp4"), "video");
        String categoryUrl = "https://example.test/sonnenmilch.mp4";
        Path mausDirectory = Files.createDirectories(tempDirectory.resolve("Die Sendung mit der Maus"));
        Path category = Files.writeString(mausDirectory.resolve(
                "Die Sendung mit der Maus-Sachgeschichte_ Sonnenmilch-"
                        + EpisodeIdentity.legacySourceHash(categoryUrl) + ".mp4"), "video");

        LibraryIndex index = LibraryIndex.scan(tempDirectory);

        assertEquals(dated, index.find(film("Heilige Birma", datedUrl)).orElseThrow());
        assertEquals(category, index.find(film("Sonnenmilch", categoryUrl)).orElseThrow());
    }

    @Test
    void ignoresPartialFilesAndKeepsAccessibilityEditionsDistinct() throws Exception {
        String partialUrl = "https://example.test/new-episode.mp4";
        Path directory = Files.createDirectories(tempDirectory.resolve("Die Sendung mit der Maus"));
        Files.writeString(directory.resolve("2026-08-09 - Neue Folge [src-"
                + EpisodeIdentity.sourceFingerprint(partialUrl) + "].mp4.part.mp4"), "partial");
        String accessibleUrl = "https://example.test/frankreich-ad.mp4";
        Files.writeString(directory.resolve(
                "Die Sendung mit der Maus-MausSpezial_ Frankreich-Maus - Audiodeskription-"
                        + EpisodeIdentity.legacySourceHash(accessibleUrl) + ".mp4"),
                "video");

        LibraryIndex index = LibraryIndex.scan(tempDirectory);

        assertTrue(index.find(film("Neue Folge", partialUrl)).isEmpty());
        assertTrue(index.find(film("Frankreich-Maus", accessibleUrl)).isEmpty());
        assertTrue(index.find(film("Frankreich-Maus - Audiodeskription", accessibleUrl)).isPresent());
    }

    @Test
    void keepsRecurringTitleOnlyEpisodesDistinct() throws Exception {
        String existingUrl = "https://example.test/news/first.mp4";
        Path directory = Files.createDirectories(tempDirectory.resolve("News"));
        Path existing = Files.writeString(directory.resolve("2026-08-14 - Bulletin [src-"
                + EpisodeIdentity.sourceFingerprint(existingUrl) + "].mp4"), "video");
        Files.writeString(directory.resolve("2026-08-13 - Ambiguous Bulletin.mp4"), "ambiguous");

        LibraryIndex index = LibraryIndex.scan(tempDirectory);

        assertEquals(existing, index.find(film("Bulletin", existingUrl)).orElseThrow());
        assertTrue(index.find(film("Bulletin", "https://example.test/news/second.mp4")).isEmpty());
        assertTrue(index.find(film("Ambiguous Bulletin", "https://example.test/news/ambiguous.mp4")).isEmpty());
    }

    @Test
    void usesImmediateParentForNestedLegacyNames() throws Exception {
        String sourceUrl = "https://example.test/show/episode.mp4";
        Path directory = Files.createDirectories(tempDirectory.resolve("category/Show"));
        Path video = Files.writeString(directory.resolve("Show-Episode-"
                + EpisodeIdentity.legacySourceHash(sourceUrl) + ".mp4"), "video");

        LibraryIndex index = LibraryIndex.scan(tempDirectory);

        assertEquals(video, index.find(film("Episode", sourceUrl)).orElseThrow());
    }

    private static Model.Film film(String title, String sourceUrl) {
        return new Model.Film(
                "id", "BR", "Topic", title, "", 1700000000, 1200, 1,
                "", "", sourceUrl, "", "");
    }
}
