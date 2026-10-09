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

import mediathek.tool.GuiFunktionenProgramme
import org.apache.logging.log4j.LogManager
import java.nio.file.Path
import kotlin.io.path.absolutePathString

private const val SHUTDOWN_HELPER = "MVShutdownHelper"
private const val LEGACY_SHUTDOWN_HELPER = "mv_shutdown_helper"

class MacComputerShutdown : ComputerShutdown {
    private val logger = LogManager.getLogger()

    override fun requestShutdown() {
        var exePath: Path? = null
        try {
            exePath = GuiFunktionenProgramme.findExecutableOnPath(SHUTDOWN_HELPER)
        } catch (_: Exception) {
            logger.warn("Could not find {} executable", SHUTDOWN_HELPER)
            try {
                exePath = GuiFunktionenProgramme.findExecutableOnPath(LEGACY_SHUTDOWN_HELPER)
            } catch (_: Exception) {
                logger.error("Could not find old {} executable", LEGACY_SHUTDOWN_HELPER)
            }
        }

        if (exePath != null) {
            Runtime.getRuntime().exec(arrayOf("nohup", exePath.absolutePathString()))
        } else {
            logger.error("Could not shutdown mac as executable path is null")
        }
    }
}
