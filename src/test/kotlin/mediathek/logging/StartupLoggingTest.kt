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

import org.apache.logging.log4j.Level
import org.apache.logging.log4j.core.LogEvent
import org.apache.logging.log4j.core.LoggerContext
import org.apache.logging.log4j.core.appender.AbstractAppender
import org.apache.logging.log4j.core.appender.ConsoleAppender
import org.apache.logging.log4j.core.config.DefaultConfiguration
import org.junit.jupiter.api.Assertions.*
import org.junit.jupiter.api.Test
import org.junit.jupiter.api.io.TempDir
import org.junit.jupiter.params.ParameterizedTest
import org.junit.jupiter.params.provider.ValueSource
import java.nio.file.Files
import java.nio.file.Path

internal class StartupLoggingTest {
    @TempDir
    lateinit var directory: Path

    @Test
    fun `inaccessible log file leaves console logging available and reports its path`() {
        // A directory at the file path fails reliably on every platform, even with elevated privileges.
        val logFile = Files.createDirectory(directory.resolve("mediathekview.log"))
        LoggerContext("file-logging-failure-test").use { context ->
            context.start(DefaultConfiguration().apply { rootLogger.level = Level.TRACE })
            context.rootLogger.appenders.values.toList().forEach(context.rootLogger::removeAppender)
            val events = mutableListOf<LogEvent>()
            val recorder = object : AbstractAppender("recorder", null, null, false, emptyArray()) {
                override fun append(event: LogEvent) {
                    events.add(event.toImmutable())
                }
            }
            recorder.start()
            context.configuration.addAppender(recorder)
            context.rootLogger.addAppender(recorder)
            val warning = assertDoesNotThrow<String?> {
                StartupLogging.configure(context, false, false) { logFile }
            }

            assertNotNull(warning)
            assertTrue(warning!!.contains(logFile.toString()))
            assertTrue(warning.contains("ohne Dateiprotokollierung"))
            assertTrue(context.rootLogger.appenders.values.any { it is ConsoleAppender && it.isStarted })
            assertFalse(context.configuration.appenders.containsKey("LogFile"))
            assertFalse(context.rootLogger.appenders.containsKey("Async"))
            context.getLogger("startup").info("Startup continues")
            assertTrue(events.any { it.level == Level.WARN && it.message.formattedMessage == warning })
            assertTrue(events.any { it.message.formattedMessage == "Startup continues" })
        }
    }

    @Test
    fun `writable log file still receives messages`() {
        val logFile = directory.resolve("mediathekview.log")
        LoggerContext("file-logging-success-test").use { context ->
            context.start(DefaultConfiguration().apply { rootLogger.level = Level.TRACE })
            assertNull(StartupLogging.configure(context, false, false) { logFile })
            context.getLogger("startup").info("Startup log message")
        }
        assertTrue(Files.readString(logFile).contains("Startup log message"))
    }

    @ParameterizedTest
    @ValueSource(strings = ["TRACE", "INFO", "WARN", "OFF"])
    fun `custom root levels are preserved`(levelName: String) {
        val level = Level.valueOf(levelName)
        for (fileLoggingDisabled in listOf(false, true)) {
            LoggerContext("custom-root-$levelName-$fileLoggingDisabled").use { context ->
                context.start(DefaultConfiguration().apply { rootLogger.level = level })
                // Include a logger created before startup updates the configuration.
                val logger = context.getLogger("startup")
                assertNull(StartupLogging.configure(context, false, fileLoggingDisabled) {
                    directory.resolve("$levelName-$fileLoggingDisabled.log")
                })

                assertEquals(level, context.configuration.rootLogger.level)
                assertEquals(level, context.rootLogger.level)
                assertEquals(level, logger.level)
                assertEquals(level.isLessSpecificThan(Level.INFO), logger.isInfoEnabled)
                assertEquals(level.isLessSpecificThan(Level.WARN), logger.isWarnEnabled)
            }
        }
    }

    @Test
    fun `disabled file logging does not resolve or open log file`() {
        LoggerContext("file-logging-disabled-test").use { context ->
            context.start(DefaultConfiguration())
            assertNull(StartupLogging.configure(context, false, true) { error("Must not resolve log path") })
            assertFalse(context.configuration.appenders.containsKey("LogFile"))
        }
    }
}
