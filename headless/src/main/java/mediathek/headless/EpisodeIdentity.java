/*
 * Copyright (c) 2026 MediathekView contributors.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 */
package mediathek.headless;

import java.nio.charset.StandardCharsets;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.util.HexFormat;

final class EpisodeIdentity {
    private static final int SOURCE_FINGERPRINT_BYTES = 16;

    private EpisodeIdentity() {
    }

    static String sourceFingerprint(String sourceUrl) {
        try {
            byte[] digest = MessageDigest.getInstance("SHA-256")
                    .digest(sourceUrl.getBytes(StandardCharsets.UTF_8));
            return HexFormat.of().formatHex(digest, 0, SOURCE_FINGERPRINT_BYTES);
        }
        catch (NoSuchAlgorithmException exception) {
            throw new IllegalStateException("SHA-256 is unavailable", exception);
        }
    }

    static String legacySourceHash(String sourceUrl) {
        int hash = Math.abs(sourceUrl.hashCode());
        StringBuilder padded = new StringBuilder(Integer.toString(hash));
        while (padded.length() < 10) {
            padded.insert(0, '0');
        }
        return padded.toString();
    }
}
