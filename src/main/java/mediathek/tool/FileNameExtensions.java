/*
 * Copyright (c) 2026 derreisende77.
 * This code was developed as part of the MediathekView project https://github.com/mediathekview/MediathekView
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package mediathek.tool;

public final class FileNameExtensions {
    private FileNameExtensions() {
    }

    public static int getLikelyExtensionDotIndex(String fileName) {
        final int suffixEnd = getSuffixEnd(fileName);
        final int lastSeparator = getLastPathSeparator(fileName, suffixEnd);
        final int lastDot = fileName.lastIndexOf('.', suffixEnd - 1);
        if (lastDot < 0 || lastDot <= lastSeparator) {
            return -1;
        }

        return looksLikeExtension(fileName.substring(lastDot + 1, suffixEnd))
                ? lastDot
                : -1;
    }

    public static int getLastPathSeparator(String fileName) {
        return getLastPathSeparator(fileName, fileName.length());
    }

    private static int getLastPathSeparator(String fileName, int upperBoundExclusive) {
        return Math.max(
                fileName.lastIndexOf('/', upperBoundExclusive - 1),
                fileName.lastIndexOf('\\', upperBoundExclusive - 1)
        );
    }

    private static int getSuffixEnd(String fileName) {
        final int queryIndex = fileName.indexOf('?');
        final int fragmentIndex = fileName.indexOf('#');
        int suffixEnd = fileName.length();
        if (queryIndex >= 0) {
            suffixEnd = Math.min(suffixEnd, queryIndex);
        }
        if (fragmentIndex >= 0) {
            suffixEnd = Math.min(suffixEnd, fragmentIndex);
        }
        return suffixEnd;
    }

    public static boolean looksLikeExtension(String suffix) {
        return !suffix.isEmpty()
                && suffix.length() <= 10
                && suffix.indexOf(' ') < 0
                && suffix.indexOf('/') < 0
                && suffix.indexOf('\\') < 0;
    }
}
