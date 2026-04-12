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

package mediathek.x11

import kotlinx.coroutines.*
import org.apache.commons.lang3.SystemUtils
import java.io.IOException
import java.util.*

object DesktopEnvDetector {
    enum class DesktopEnvironment {
        KDE,
        GNOME,
        UNITY,
        UNKNOWN,
    }

    @JvmStatic
    fun trayIconSupported(): Boolean {
        if (!SystemUtils.IS_OS_LINUX) {
            return true
        }

        return detect() != DesktopEnvironment.KDE
    }

    @JvmStatic
    fun detect(): DesktopEnvironment = runBlocking {
        detectDesktopEnvironment()
    }

    @JvmSynthetic
    suspend fun detectDesktopEnvironment(): DesktopEnvironment {
        val envValue = getDesktopEnvFromVariables().uppercase(Locale.ROOT)

        return when {
            "KDE" in envValue -> DesktopEnvironment.KDE
            "GNOME" in envValue -> DesktopEnvironment.GNOME
            "UNITY" in envValue -> DesktopEnvironment.UNITY
            else -> detectDesktopEnvironmentFromProcesses()
        }
    }

    private fun getDesktopEnvFromVariables(): String {
        val env = System.getenv()

        return env["XDG_CURRENT_DESKTOP"]
            ?: env["DESKTOP_SESSION"]
            ?: env["GDMSESSION"]
            ?: env["GNOME_DESKTOP_SESSION_ID"]?.let { "GNOME" }
            ?: "unknown"
    }

    private suspend fun detectDesktopEnvironmentFromProcesses(): DesktopEnvironment = coroutineScope {
        val processChecks = listOf(
            DesktopEnvironment.KDE to async { isProcessRunning("plasmashell") },
            DesktopEnvironment.GNOME to async { isProcessRunning("gnome-shell") },
            DesktopEnvironment.UNITY to async { isProcessRunning("unity-panel-service") },
        )

        processChecks.firstOrNull { (_, isRunning) -> isRunning.await() }?.first
            ?: DesktopEnvironment.UNKNOWN
    }

    private suspend fun isProcessRunning(process: String): Boolean = withContext(Dispatchers.IO) {
        try {
            val p = ProcessBuilder("pgrep", "-x", process)
                .redirectErrorStream(true)
                .start()
            p.waitFor() == 0
        } catch (_: IOException) {
            false
        } catch (_: InterruptedException) {
            false
        }
    }
}
