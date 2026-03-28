/*
 * Copyright (c) 2024-2026 derreisende77.
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

import org.apache.commons.lang3.SystemUtils;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.StringWriter;
import java.util.concurrent.TimeUnit;


/**
 * Dark mode detector for macOS and Windows.
 * Based on java code from <a href="https://gist.github.com/HanSolo/7cf10b86efff8ca2845bf5ec2dd0fe1d">this gist</a>.
 */
public class DarkModeDetector {
    private static final String REGDWORD_TOKEN = "REG_DWORD";
    private static final String[] DARK_THEME_CMD = {
            "reg", "query", "HKEY_CURRENT_USER\\Software\\Microsoft\\Windows\\CurrentVersion\\Themes\\Personalize",
            "/v", "AppsUseLightTheme"
    };

    /**
     * Detect whether the running OS is in dark mode.
     * Works only on windows and macOS.
     *
     * @return true if in dark mode, false if otherwise.
     */
    public static boolean isDarkMode() {
        if (SystemUtils.IS_OS_MAC_OSX)
            return isMacOsDarkMode();
        else if (SystemUtils.IS_OS_WINDOWS)
            return isWindowsDarkMode();
        else if (SystemUtils.IS_OS_LINUX && isGnome())
            return isGnomeDarkMode();
        else
            return false;
    }

    private static boolean isGnome() {
        var currentDesktop = System.getenv("XDG_CURRENT_DESKTOP");
        return currentDesktop != null && (currentDesktop.equals("GNOME") || currentDesktop.equals("ubuntu:GNOME"));
    }

    /**
     * Indicate if dark mode detection is supported on the current platform.
     * @return true if supported, false otherwise.
     */
    public static boolean hasDarkModeDetectionSupport() {
        return SystemUtils.IS_OS_WINDOWS || SystemUtils.IS_OS_MAC_OSX ||
                (SystemUtils.IS_OS_LINUX && isGnome());
    }

    private static boolean isGnomeDarkMode() {
        try {
            var process = new ProcessBuilder("gsettings", "get", "org.gnome.desktop.interface", "color-scheme").start();
            String result;
            try (var source = new InputStreamReader(process.getInputStream());
                 var buffer = new StringWriter()) {
                source.transferTo(buffer);
                result = buffer.toString().trim();
            }
            boolean finished = process.waitFor(5, TimeUnit.SECONDS);
            if (!finished) {
                process.destroyForcibly();
                return false;
            }
            if (process.exitValue() != 0) {
                return false;
            }
            return result.equals("'prefer-dark'");
        } catch (IOException | InterruptedException e) {
            return false;
        }
    }

    private static boolean isMacOsDarkMode() {
        try {
            boolean isDarkMode = false;
            var process = new ProcessBuilder("defaults", "read", "-g", "AppleInterfaceStyle").start();
            try (var isr = new InputStreamReader(process.getInputStream());
                 var rdr = new BufferedReader(isr)) {
                String line;
                while ((line = rdr.readLine()) != null) {
                    if (line.equals("Dark")) {
                        isDarkMode = true;
                    }
                }
            }
            int rc = process.waitFor();  // Wait for the process to complete
            return 0 == rc && isDarkMode;
        }
        catch (IOException | InterruptedException e) {
            return false;
        }
    }

    private static boolean isWindowsDarkMode() {
        try {
            Process process = new ProcessBuilder(DARK_THEME_CMD).start();
            String result;
            try (var reader = new InputStreamReader(process.getInputStream());
                 var buffer = new StringWriter()) {
                reader.transferTo(buffer);
                int rc = process.waitFor();
                if (rc != 0) {
                    return false;
                }
                result = buffer.toString();
            }
            int p = result.indexOf(REGDWORD_TOKEN);

            if (p == -1) { return false; }

            // 1 == Light Mode, 0 == Dark Mode
            String temp = result.substring(p + REGDWORD_TOKEN.length()).trim();
            return ((Integer.parseInt(temp.substring("0x".length()), 16))) == 0;
        }
        catch (Exception e) {
            return false;
        }
    }
}
