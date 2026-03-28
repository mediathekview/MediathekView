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

import java.nio.file.Path;

public final class PathExtensions {

    private PathExtensions() {
    }

    public static Path withExtension(Path path, String newExtension) {
        if (path == null) {
            throw new IllegalArgumentException("path must not be null");
        }
        if (newExtension == null || newExtension.isBlank()) {
            throw new IllegalArgumentException("extension must not be blank");
        }

        String normalizedExt = newExtension.startsWith(".")
                ? newExtension
                : "." + newExtension;

        Path fileName = path.getFileName();
        if (fileName == null) {
            throw new IllegalArgumentException("Path has no filename: " + path);
        }

        String name = fileName.toString();
        int lastDot = FileNameExtensions.getLikelyExtensionDotIndex(name);

        String baseName;
        if (lastDot > 0) {
            // Replace existing extension only if the suffix looks like one.
            baseName = name.substring(0, lastDot);
        }
        else {
            // No extension present
            baseName = name;
        }

        String newName = baseName + normalizedExt;

        Path parent = path.getParent();
        return (parent == null)
                ? Path.of(newName)
                : parent.resolve(newName);
    }
}
