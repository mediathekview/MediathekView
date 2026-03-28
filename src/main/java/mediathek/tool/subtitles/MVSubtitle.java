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

package mediathek.tool.subtitles;

import mediathek.daten.DatenDownload;
import mediathek.tool.FileUtils;
import mediathek.tool.PathExtensions;
import mediathek.tool.subtitles.detector.TimedTextFormatDetector;
import mediathek.tool.subtitles.ttml2.AssExporter;
import mediathek.tool.subtitles.ttml2.SubRipHtmlExporter;
import mediathek.tool.subtitles.ttml2.Ttml2Parser;
import mediathek.tool.subtitles.vtt.WebVttToTtml2Converter;
import org.apache.logging.log4j.LogManager;
import org.jetbrains.annotations.NotNull;

import java.io.IOException;
import java.nio.file.*;

public class MVSubtitle {
    /**
     * Move {@code source} to {@code target}, preferring ATOMIC_MOVE and falling back to non-atomic move/copy-delete.
     * This is intended for files that may cross filesystems or be stored on network shares.
     */
    public static void moveWithFallback(@NotNull Path source, @NotNull Path target) throws IOException {
        try {
            // Atomic move is preferred but may fail across filesystems.
            Files.move(source, target, StandardCopyOption.REPLACE_EXISTING, StandardCopyOption.ATOMIC_MOVE);
            return;
        } catch (AtomicMoveNotSupportedException ignored) {
            // Retry below without atomic semantics.
        }

        //fall back to copy/delete...
        moveNonAtomicOrCopyDelete(source, target);
    }

    private static void moveNonAtomicOrCopyDelete(@NotNull Path source, @NotNull Path target) throws IOException {
        try {
            Files.move(source, target, StandardCopyOption.REPLACE_EXISTING);
        } catch (FileSystemException e) {
            if (isCrossDeviceMoveError(source, target, e)) {
                Files.copy(source, target, StandardCopyOption.REPLACE_EXISTING);
                Files.delete(source);
            } else {
                throw e;
            }
        }
    }

    private static boolean isCrossDeviceMoveError(@NotNull Path source, @NotNull Path target, @NotNull FileSystemException e) {
        try {
            final Path targetProbe = Files.exists(target) ? target : target.getParent();
            if (targetProbe != null && !Files.getFileStore(source).equals(Files.getFileStore(targetProbe))) {
                return true;
            }
        } catch (IOException ignored) {
            // Fall back to reason parsing below.
        }

        final String reason = e.getReason();
        if (reason == null) {
            return false;
        }

        final String normalized = reason.toLowerCase();
        return normalized.contains("cross-device")
                || normalized.contains("exdev")
                || (normalized.contains("link") && normalized.contains("device"));
    }

    public static Path addFileExtension(@NotNull Path selectedFilePath, @NotNull TimedTextFormatDetector.Format format) throws IOException {
        Path path;
        switch (format) {
            case WEBVTT -> {
                path = PathExtensions.withExtension(selectedFilePath, ".vtt");
                moveWithFallback(selectedFilePath, path);
            }
            case TTML1, TTML2 -> {
                path = PathExtensions.withExtension(selectedFilePath, ".ttml");
                moveWithFallback(selectedFilePath, path);
            }
            default -> throw new IOException("Unknown subtitle format: " + format);
        }
        return path;
    }

    private void downloadAndConvertSubtitleFile(@NotNull String subtitleUrl, @NotNull Path selectedFilePath) throws Exception {
        Path tempSubtitleFile = null;
        try {
            tempSubtitleFile = FileUtils.downloadToTempFile(subtitleUrl);
            moveWithFallback(tempSubtitleFile, selectedFilePath);

            var res = TimedTextFormatDetector.detect(selectedFilePath, true);
            if (!res.valid()) {
                throw new IOException("Invalid subtitle format: " + res.format());
            } else {
                //valid result
                if (res.format() == TimedTextFormatDetector.Format.UNKNOWN) {
                    throw new IOException("Unknown subtitle format: " + res.format());
                }

                selectedFilePath = addFileExtension(selectedFilePath, res.format());

                if (res.format() == TimedTextFormatDetector.Format.WEBVTT) {
                    //convert WebVTT to TTML2
                    WebVttToTtml2Converter converter = new WebVttToTtml2Converter();
                    var newPath = PathExtensions.withExtension(selectedFilePath, ".ttml");
                    converter.convert(selectedFilePath, newPath);
                    selectedFilePath = newPath;
                }

                Ttml2Parser parser = new Ttml2Parser();
                var ttmlDoc = parser.parse(selectedFilePath);

                //SRT conversion
                var srtPath = PathExtensions.withExtension(selectedFilePath, ".srt");
                var srtStr = new SubRipHtmlExporter().export(ttmlDoc);
                Files.writeString(srtPath, srtStr);

                //ASS conversion
                var assPath = PathExtensions.withExtension(selectedFilePath, ".ass");
                var assOptions = new AssExporter.Options(384, 288, false);
                var assStr = new AssExporter(assOptions).export(ttmlDoc);
                Files.writeString(assPath, assStr);
            }
        } finally {
            try {
                if (tempSubtitleFile != null)
                    Files.deleteIfExists(tempSubtitleFile);
            } catch (IOException ignored) {
            }
        }
    }

    public void writeSubtitle(@NotNull DatenDownload datenDownload) {
        final String urlSubtitle = datenDownload.arr[DatenDownload.DOWNLOAD_URL_SUBTITLE];
        if (urlSubtitle.isEmpty())
            return;

        Path destinationPath = Paths.get(datenDownload.getFileNameWithoutSuffix());
        try {
            downloadAndConvertSubtitleFile(urlSubtitle, destinationPath);
        } catch (Exception e) {
            LogManager.getLogger().error("Error writing subtitle.", e);
        }
    }
}
