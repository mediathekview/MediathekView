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

import java.util.List;

import static mediathek.headless.Model.Film;
import static mediathek.headless.Model.Quality;
import static mediathek.headless.Model.SearchRequest;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

class ModelTest {
    @Test
    void serializesMediathekViewWebDurationFieldNames() {
        SearchRequest request = new SearchRequest(List.of(), "timestamp", "desc", false, 0, 20, 60, 3600);

        String json = JsonSupport.write(request);

        assertTrue(json.contains("\"duration_min\":60"));
        assertTrue(json.contains("\"duration_max\":3600"));
    }

    @Test
    void qualityFallsBackWhenPreferredUrlIsMissing() {
        Film film = new Film("id", "WDR", "topic", "title", "", 0, 0, 0,
                "", "", "https://example.test/sd.mp4", "", "");

        assertEquals("https://example.test/sd.mp4", Quality.HD.selectUrl(film));
        assertEquals(Quality.LOW, Quality.parse("lq"));
    }
}
