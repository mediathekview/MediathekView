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

package mediathek

import mediathek.tool.UIProgressState
import javax.swing.SwingUtilities

object SplashScreenLifecycle {
    private var splashScreen: SplashScreen? = null

    fun set(splashScreen: SplashScreen?) {
        this.splashScreen = splashScreen
    }

    fun show() {
        runOnEdt {
            splashScreen?.isVisible = true
        }
    }

    fun hide() {
        runOnEdt {
            splashScreen?.isVisible = false
        }
    }

    @JvmStatic
    fun update(state: UIProgressState) {
        splashScreen?.update(state)
    }

    @JvmStatic
    fun close() {
        splashScreen?.close()
    }

    fun clearIfCurrent(splashScreen: SplashScreen) {
        if (this.splashScreen === splashScreen) {
            this.splashScreen = null
        }
    }

    private fun runOnEdt(block: () -> Unit) {
        if (SwingUtilities.isEventDispatchThread()) {
            block()
        } else {
            SwingUtilities.invokeLater(block)
        }
    }
}
