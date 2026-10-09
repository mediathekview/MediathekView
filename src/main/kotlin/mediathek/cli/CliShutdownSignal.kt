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

@file:Suppress("JAVA_MODULE_DOES_NOT_EXPORT_PACKAGE")

package mediathek.cli

import org.apache.logging.log4j.LogManager
import sun.misc.Signal

import java.util.concurrent.atomic.AtomicBoolean

object CliShutdownSignal {
    private val logger = LogManager.getLogger()

    fun install(onShutdownRequested: () -> Unit): AutoCloseable =
        try {
            val signal = Signal("INT")
            val firstSignal = AtomicBoolean(true)
            val previousHandler = Signal.handle(signal) {
                if (firstSignal.compareAndSet(true, false)) {
                    onShutdownRequested()
                } else {
                    logger.warn("Second Ctrl-C received. Forcing shutdown.")
                    Runtime.getRuntime().halt(DownloadAndQuitRunner.INTERRUPTED_EXIT_CODE)
                }
            }
            AutoCloseable {
                Signal.handle(signal, previousHandler)
            }
        } catch (ex: SecurityException) {
            logger.warn("Could not install Ctrl-C handler for CLI shutdown.", ex)
            AutoCloseable {}
        } catch (ex: IllegalArgumentException) {
            logger.warn("Could not install Ctrl-C handler for CLI shutdown.", ex)
            AutoCloseable {}
        } catch (ex: LinkageError) {
            logger.warn("Could not install Ctrl-C handler for CLI shutdown.", ex)
            AutoCloseable {}
        }
}
