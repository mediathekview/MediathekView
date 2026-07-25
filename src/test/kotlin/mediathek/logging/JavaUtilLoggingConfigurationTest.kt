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

package mediathek.logging

import org.junit.jupiter.api.Assertions.*
import org.junit.jupiter.api.Test
import java.util.logging.Handler
import java.util.logging.Level

internal class JavaUtilLoggingConfigurationTest {
    @Test
    fun redirectsLucenePanamaVectorizationInfoLoggerToLog4jTrace() {
        val logger = JavaUtilLoggingConfiguration.lucenePanamaVectorizationLoggerForTest()
        val previousLevel = logger.level
        val previousUseParentHandlers = logger.useParentHandlers
        val previousHandlers = logger.handlers.toList()
        previousHandlers.forEach(logger::removeHandler)
        logger.level = Level.WARNING
        logger.useParentHandlers = true

        try {
            JavaUtilLoggingConfiguration.install()

            assertEquals(Level.INFO, logger.level)
            assertFalse(logger.useParentHandlers)
            assertTrue(logger.handlers.any { handler -> handler.javaClass.name.contains("JulTraceForwardingHandler") })
        } finally {
            logger.handlers.toList().forEach(logger::removeHandler)
            previousHandlers.forEach { handler: Handler -> logger.addHandler(handler) }
            logger.level = previousLevel
            logger.useParentHandlers = previousUseParentHandlers
            JavaUtilLoggingConfiguration.install()
        }
    }
}
