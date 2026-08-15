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
        Path directory = Files.createDirectories(
                tempDirectory.resolve("Anna, Nina, Pia und die wilden Tiere"));
        Path video = Files.writeString(directory.resolve(
                "Anna, Nina, Pia und die wilden Tiere-Die Raubkatzen von Brasilien-1011316568.mp4"),
                "video");

        LibraryIndex index = LibraryIndex.scan(tempDirectory);

        assertEquals(video, index.find(film("Die Raubkatzen von Brasilien")).orElseThrow());
    }

    @Test
    void recognizesDatedAndContentCategoryFilenames() throws Exception {
        Path datedDirectory = Files.createDirectories(tempDirectory.resolve("Anna und die Haustiere"));
        Path dated = Files.writeString(
                datedDirectory.resolve("2026-08-08 - Heilige Birma.mp4"), "video");
        Path mausDirectory = Files.createDirectories(tempDirectory.resolve("Die Sendung mit der Maus"));
        Path category = Files.writeString(mausDirectory.resolve(
                "Die Sendung mit der Maus-Sachgeschichte_ Sonnenmilch-1316812142.mp4"), "video");

        LibraryIndex index = LibraryIndex.scan(tempDirectory);

        assertEquals(dated, index.find(film("Heilige Birma")).orElseThrow());
        assertEquals(category, index.find(film("Sonnenmilch")).orElseThrow());
    }

    @Test
    void ignoresPartialFilesAndKeepsAccessibilityEditionsDistinct() throws Exception {
        Path directory = Files.createDirectories(tempDirectory.resolve("Die Sendung mit der Maus"));
        Files.writeString(directory.resolve("2026-08-09 - Neue Folge.mp4.part.mp4"), "partial");
        Files.writeString(directory.resolve(
                "Die Sendung mit der Maus-MausSpezial_ Frankreich-Maus - Audiodeskription-0096140127.mp4"),
                "video");

        LibraryIndex index = LibraryIndex.scan(tempDirectory);

        assertTrue(index.find(film("Neue Folge")).isEmpty());
        assertTrue(index.find(film("Frankreich-Maus")).isEmpty());
        assertTrue(index.find(film("Frankreich-Maus - Audiodeskription")).isPresent());
    }

    private static Model.Film film(String title) {
        return new Model.Film(
                "id", "BR", "Topic", title, "", 1700000000, 1200, 1,
                "", "", "https://example.test/video.mp4", "", "");
    }
}
