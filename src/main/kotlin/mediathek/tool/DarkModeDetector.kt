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

package mediathek.tool

import org.apache.commons.lang3.SystemUtils
import java.io.IOException
import java.io.InputStreamReader
import java.io.StringWriter
import java.nio.charset.StandardCharsets
import java.util.concurrent.TimeUnit

/**
 * Dark mode detector for macOS and Windows.
 * Based on java code from [this gist](https://gist.github.com/HanSolo/7cf10b86efff8ca2845bf5ec2dd0fe1d).
 */
object DarkModeDetector {
    private const val REGDWORD_TOKEN = "REG_DWORD"
    private const val GNOME_DARK_MODE = "'prefer-dark'"
    private const val MACOS_DARK_MODE = "Dark"
    private val darkThemeCommand = arrayOf(
        "reg",
        "query",
        "HKEY_CURRENT_USER\\Software\\Microsoft\\Windows\\CurrentVersion\\Themes\\Personalize",
        "/v",
        "AppsUseLightTheme",
    )

    /**
     * Detect whether the running OS is in dark mode.
     * Works only on windows and macOS.
     *
     * @return true if in dark mode, false if otherwise.
     */
    @JvmStatic
    fun isDarkMode(): Boolean = when {
        SystemUtils.IS_OS_MAC_OSX -> isMacOsDarkMode()
        SystemUtils.IS_OS_WINDOWS -> isWindowsDarkMode()
        SystemUtils.IS_OS_LINUX && isGnome() -> isGnomeDarkMode()
        else -> false
    }

    private fun isGnome(): Boolean {
        val currentDesktop = System.getenv("XDG_CURRENT_DESKTOP")
        return currentDesktop == "GNOME" || currentDesktop == "ubuntu:GNOME"
    }

    /**
     * Indicate if dark mode detection is supported on the current platform.
     * @return true if supported, false otherwise.
     */
    @JvmStatic
    fun hasDarkModeDetectionSupport(): Boolean =
        SystemUtils.IS_OS_WINDOWS || SystemUtils.IS_OS_MAC_OSX || (SystemUtils.IS_OS_LINUX && isGnome())

    private fun isGnomeDarkMode(): Boolean =
        readCommandOutput(
            "gsettings",
            "get",
            "org.gnome.desktop.interface",
            "color-scheme",
            timeoutSeconds = 5,
        ) == GNOME_DARK_MODE

    private fun isMacOsDarkMode(): Boolean =
        readCommandOutput("defaults", "read", "-g", "AppleInterfaceStyle")
            ?.lineSequence()
            ?.any { it == MACOS_DARK_MODE }
            ?: false

    private fun isWindowsDarkMode(): Boolean {
        val result = readCommandOutput(*darkThemeCommand) ?: return false
        val tokenPosition = result.indexOf(REGDWORD_TOKEN)
        if (tokenPosition == -1) {
            return false
        }

        return parseWindowsThemeValue(result, tokenPosition)
    }

    private fun parseWindowsThemeValue(result: String, tokenPosition: Int): Boolean {
        val registryValue = result.substring(tokenPosition + REGDWORD_TOKEN.length).trim()

        return try {
            // 1 == Light Mode, 0 == Dark Mode
            registryValue.substring("0x".length).toInt(16) == 0
        } catch (_: NumberFormatException) {
            false
        } catch (_: StringIndexOutOfBoundsException) {
            false
        }
    }

    private fun readCommandOutput(
        vararg command: String,
        timeoutSeconds: Long? = null,
    ): String? {
        try {
            val process = ProcessBuilder(*command).start()
            val result = InputStreamReader(process.inputStream, StandardCharsets.UTF_8).use { reader ->
                StringWriter().use { buffer ->
                    reader.transferTo(buffer)
                    buffer.toString()
                }
            }

            val finished = timeoutSeconds?.let { process.waitFor(it, TimeUnit.SECONDS) } ?: run {
                process.waitFor()
                true
            }
            if (!finished) {
                process.destroyForcibly()
                return null
            }

            if (process.exitValue() != 0) {
                return null
            }

            return result.trim()
        } catch (_: InterruptedException) {
            Thread.currentThread().interrupt()
            return null
        } catch (_: IOException) {
            return null
        }
    }
}
