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

import org.apache.logging.log4j.LogManager
import java.util.logging.Handler
import java.util.logging.Level
import java.util.logging.LogRecord
import java.util.logging.Logger

object JavaUtilLoggingConfiguration {
    private const val LUCENE_PANAMA_VECTORIZATION_LOGGER =
        "org.apache.lucene.internal.vectorization.PanamaVectorizationProvider"
    private val lucenePanamaVectorizationLogger: Logger = Logger.getLogger(LUCENE_PANAMA_VECTORIZATION_LOGGER)
    private val lucenePanamaVectorizationHandler = JulTraceForwardingHandler()

    fun install() {
        lucenePanamaVectorizationLogger.level = Level.INFO
        lucenePanamaVectorizationLogger.useParentHandlers = false
        if (lucenePanamaVectorizationHandler !in lucenePanamaVectorizationLogger.handlers) {
            lucenePanamaVectorizationLogger.addHandler(lucenePanamaVectorizationHandler)
        }
    }

    internal fun lucenePanamaVectorizationLoggerForTest(): Logger = lucenePanamaVectorizationLogger

    private class JulTraceForwardingHandler : Handler() {
        init {
            level = Level.INFO
        }

        override fun publish(record: LogRecord) {
            if (!isLoggable(record)) {
                return
            }

            val logger = LogManager.getLogger(record.loggerName)
            val thrown = record.thrown
            if (thrown == null) {
                logger.trace(record.message)
            } else {
                logger.trace(record.message, thrown)
            }
        }

        override fun flush() {
            // Log4j2 appenders handle their own flushing.
        }

        override fun close() {
            // Shared singleton handler; nothing to close.
        }
    }
}
