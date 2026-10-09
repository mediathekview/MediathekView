/*
 * Copyright (c) 2025 derreisende77.
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

import org.apache.logging.log4j.LogManager
import java.io.IOException

/**
 * @author emil
 */
object GetFile {
    private val logger = LogManager.getLogger()

    @JvmStatic
    fun getHilfeSuchen(pfad: String?): String {
        if (pfad.isNullOrBlank()) {
            return ""
        }

        return try {
            GetFile::class.java.getResourceAsStream(pfad).use { input ->
                if (input == null) {
                    logger.warn("getHilfeSuchen(): resource not found: {}", pfad)
                    ""
                } else {
                    input.readAllBytes().toString(Charsets.UTF_8)
                }
            }
        } catch (ex: IOException) {
            logger.error("getHilfeSuchen()", ex)
            ""
        }
    }
}
