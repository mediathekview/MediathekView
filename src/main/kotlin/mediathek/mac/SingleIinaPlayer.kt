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

package mediathek.mac

import mediathek.config.Konstanten
import mediathek.tool.MVMessageDialog
import java.awt.Component
import java.io.IOException
import java.util.concurrent.atomic.AtomicBoolean
import javax.swing.JOptionPane

class SingleIinaPlayer(
    private val ownerProvider: () -> Component? = { null },
) {
    @Synchronized
    @Throws(IOException::class)
    fun play(url: String) {
        val escapedUrl = escapeAppleScriptString(url)
        val hasAccessibilityPermission = MacAccessibilityPermission.isTrusted()
        val script = if (hasAccessibilityPermission) {
            """
            tell application id "com.colliderli.iina" to activate
            delay 0.1
            tell application "System Events"
            tell process "IINA"
            try
            keystroke "w" using command down
            end try
            end tell
            end tell
            delay 0.1
            tell application id "com.colliderli.iina" to open location "$escapedUrl"
            """.trimIndent()
        } else {
            "tell application id \"com.colliderli.iina\" to open location \"$escapedUrl\""
        }

        if (!hasAccessibilityPermission) {
            maybeShowAccessibilityWarning(ownerProvider())
        }

        try {
            ProcessBuilder("/usr/bin/osascript", "-e", script).start()
        } catch (ex: IOException) {
            MacMultimediaPlayerLocator.findIinaPlayer().ifPresent { path ->
                val appBundlePath = path.parent.parent.parent.toAbsolutePath().toString()
                val processBuilder = ProcessBuilder("open", "-a", appBundlePath, url)
                try {
                    processBuilder.start()
                } catch (e: IOException) {
                    throw RuntimeException(e)
                }
            }
            if (MacMultimediaPlayerLocator.findIinaPlayer().isEmpty) {
                throw ex
            }
        }
    }

    companion object {
        private val ACCESSIBILITY_WARNING_SHOWN = AtomicBoolean(false)

        private fun maybeShowAccessibilityWarning(owner: Component?) {
            if (!ACCESSIBILITY_WARNING_SHOWN.compareAndSet(false, true)) {
                return
            }

            MVMessageDialog.showMessageDialog(
                owner,
                """
                MediathekView hat keine macOS-Bedienungshilfen-Berechtigung.
                Der Livestream kann trotzdem in IINA geoeffnet werden, aber das vorherige Fenster kann nicht automatisch geschlossen werden.

                Aktivieren Sie MediathekView unter:
                Systemeinstellungen -> Datenschutz und Sicherheit -> Bedienungshilfen
                """.trimIndent(),
                Konstanten.PROGRAMMNAME,
                JOptionPane.INFORMATION_MESSAGE,
            )
        }
    }
}
