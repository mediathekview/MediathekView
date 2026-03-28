/*
 * Copyright (c) 2025-2026 derreisende77.
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

package mediathek.mac;

import mediathek.config.Konstanten;
import mediathek.mainwindow.MediathekGui;
import mediathek.tool.MVMessageDialog;

import javax.swing.*;
import java.io.IOException;
import java.util.concurrent.atomic.AtomicBoolean;

public class SingleIinaPlayer {
    private static final AtomicBoolean ACCESSIBILITY_WARNING_SHOWN = new AtomicBoolean(false);

    public synchronized void play(String url) throws IOException {
        var escapedUrl = escapeAppleScriptString(url);
        var hasAccessibilityPermission = MacAccessibilityPermission.isTrusted();
        var script = hasAccessibilityPermission
                ? "tell application id \"com.colliderli.iina\" to activate\n" +
                "delay 0.1\n" +
                "tell application \"System Events\"\n" +
                "tell process \"IINA\"\n" +
                "try\n" +
                "keystroke \"w\" using command down\n" +
                "end try\n" +
                "end tell\n" +
                "end tell\n" +
                "delay 0.1\n" +
                "tell application id \"com.colliderli.iina\" to open location \"" + escapedUrl + "\""
                : "tell application id \"com.colliderli.iina\" to open location \"" + escapedUrl + "\"";

        if (!hasAccessibilityPermission) {
            maybeShowAccessibilityWarning();
        }

        try {
            new ProcessBuilder("/usr/bin/osascript", "-e", script).start();
        }
        catch (IOException ex) {
            MacMultimediaPlayerLocator.findIinaPlayer().ifPresent(path -> {
                var appBundlePath = path.getParent().getParent().getParent().toAbsolutePath().toString();
                var pb = new ProcessBuilder("open", "-a", appBundlePath, url);
                try {
                    pb.start();
                }
                catch (IOException e) {
                    throw new RuntimeException(e);
                }
            });
            if (MacMultimediaPlayerLocator.findIinaPlayer().isEmpty()) {
                throw ex;
            }
        }
    }

    private static String escapeAppleScriptString(String value) {
        return value.replace("\\", "\\\\").replace("\"", "\\\"");
    }

    private static void maybeShowAccessibilityWarning() {
        if (!ACCESSIBILITY_WARNING_SHOWN.compareAndSet(false, true)) {
            return;
        }

        MVMessageDialog.showMessageDialog(
                MediathekGui.ui(),
                """
                        MediathekView hat keine macOS-Bedienungshilfen-Berechtigung.
                        Der Livestream kann trotzdem in IINA geoeffnet werden, aber das vorherige Fenster kann nicht automatisch geschlossen werden.

                        Aktivieren Sie MediathekView unter:
                        Systemeinstellungen -> Datenschutz und Sicherheit -> Bedienungshilfen
                        """,
                Konstanten.PROGRAMMNAME,
                JOptionPane.INFORMATION_MESSAGE
        );
    }
}
