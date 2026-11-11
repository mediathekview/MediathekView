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
import org.apache.logging.log4j.core.Filter
import org.apache.logging.log4j.core.LoggerContext
import org.apache.logging.log4j.core.appender.AsyncAppender
import org.apache.logging.log4j.core.appender.ConsoleAppender
import org.apache.logging.log4j.core.appender.FileAppender
import org.apache.logging.log4j.core.config.AppenderRef
import org.apache.logging.log4j.core.filter.ThresholdFilter
import org.apache.logging.log4j.core.layout.PatternLayout
import java.nio.file.Path

internal object StartupLogging {
    /** Returns a user-facing warning when optional file logging could not be initialized. */
    fun configure(
        context: LoggerContext,
        detailedConsoleLogging: Boolean,
        fileLoggingDisabled: Boolean,
        logFilePath: () -> Path,
    ): String? {
        val config = context.configuration
        val consolePattern = if (detailedConsoleLogging) "[%-5level] [%t] %c - %msg%n" else ". %msg%n"
        val consoleAppender = ConsoleAppender.createDefaultAppenderForLayout(
            PatternLayout.newBuilder().withPattern(consolePattern).build()
        )
        if (!detailedConsoleLogging) {
            consoleAppender.addFilter(ThresholdFilter.createFilter(Level.INFO, Filter.Result.ACCEPT, Filter.Result.DENY))
        }
        consoleAppender.start()
        val rootLogger = context.rootLogger
        rootLogger.addAppender(consoleAppender)
        context.updateLoggers()

        if (fileLoggingDisabled) return null

        var path: Path? = null
        var fileAppender: FileAppender? = null
        var asyncAppender: AsyncAppender? = null
        try {
            path = logFilePath()
            val builder = FileAppender.newBuilder()
                .setName("LogFile")
                .withAppend(false)
                .withFileName(path.toString())
                .setLayout(PatternLayout.newBuilder().withPattern("%-5p %d  [%t] %C{2} (%F:%L) - %m%n").build())
                .setConfiguration(config)
            if (!detailedConsoleLogging) {
                builder.filter = ThresholdFilter.createFilter(Level.DEBUG, Filter.Result.ACCEPT, Filter.Result.DENY)
            }
            fileAppender = checkNotNull(builder.build()) { "Log file appender could not be created." }
            fileAppender.start()
            check(fileAppender.isStarted) { "Log file appender could not be started." }
            config.addAppender(fileAppender)

            asyncAppender = checkNotNull(AsyncAppender.newBuilder()
                .setName("Async")
                .setAppenderRefs(arrayOf(AppenderRef.createAppenderRef(fileAppender.name, null, null)))
                .setConfiguration(config)
                .setIncludeLocation(true)
                .setBlocking(false)
                .build()) { "Async log appender could not be created." }
            asyncAppender.start()
            check(asyncAppender.isStarted) { "Async log appender could not be started." }
            config.addAppender(asyncAppender)
            rootLogger.addAppender(asyncAppender)
            context.updateLoggers()
            return null
        } catch (ex: Exception) {
            asyncAppender?.let {
                rootLogger.removeAppender(it)
                config.appenders.remove(it.name)
                it.stop()
            }
            fileAppender?.let {
                config.appenders.remove(it.name)
                it.stop()
            }
            val warning = if (path != null) {
                "Die Logdatei \"$path\" konnte nicht geöffnet werden."
            } else {
                "Der Pfad zur Logdatei konnte nicht ermittelt werden."
            } + "\nMediathekView wird ohne Dateiprotokollierung fortgesetzt. Bitte prüfen Sie den Pfad und die Dateirechte."
            context.getLogger(StartupLogging::class.java).warn(warning, ex)
            return warning
        }
    }
}
