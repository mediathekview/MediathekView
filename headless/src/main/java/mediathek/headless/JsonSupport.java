/*
 * Copyright (c) 2026 MediathekView contributors.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 */
package mediathek.headless;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.DeserializationFeature;
import com.fasterxml.jackson.databind.ObjectMapper;

import java.io.IOException;
import java.io.InputStream;
import java.nio.file.Files;
import java.nio.file.Path;

final class JsonSupport {
    static final ObjectMapper MAPPER = new ObjectMapper()
            .disable(DeserializationFeature.FAIL_ON_UNKNOWN_PROPERTIES);

    private JsonSupport() {
    }

    static String write(Object value) {
        try {
            return MAPPER.writeValueAsString(value);
        }
        catch (JsonProcessingException exception) {
            throw new IllegalStateException("Could not serialize JSON", exception);
        }
    }

    static String writePretty(Object value) {
        try {
            return MAPPER.writerWithDefaultPrettyPrinter().writeValueAsString(value);
        }
        catch (JsonProcessingException exception) {
            throw new IllegalStateException("Could not serialize JSON", exception);
        }
    }

    static <T> T read(Path path, Class<T> type) throws IOException {
        try (InputStream input = Files.newInputStream(path)) {
            return MAPPER.readValue(input, type);
        }
    }
}
