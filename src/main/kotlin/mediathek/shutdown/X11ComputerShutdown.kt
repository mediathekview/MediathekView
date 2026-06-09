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

package mediathek.shutdown

import mediathek.config.application.ApplicationConfiguration
import mediathek.tool.ProcessCommandUtils
import org.apache.commons.lang3.SystemUtils
import org.apache.logging.log4j.LogManager
import java.io.IOException

class X11ComputerShutdown : ComputerShutdown {
    private val logger = LogManager.getLogger()

    override fun requestShutdown() {
        val shutdownCommand = when {
            SystemUtils.IS_OS_LINUX || SystemUtils.IS_OS_FREE_BSD -> configuredShutdownCommand()
            else -> {
                logger.error("shutdown command is unknown for this operating system")
                return
            }
        }

        try {
            logger.info("Shutdown: {}", shutdownCommand)
            ProcessBuilder(*ProcessCommandUtils.tokenizeCommand(shutdownCommand)).start()
        } catch (ex: IOException) {
            logger.error(ex)
        }
    }

    private fun configuredShutdownCommand(): String {
        return ApplicationConfiguration.getInstance().linuxShutdownCommand
    }
}
