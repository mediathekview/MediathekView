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

import org.apache.logging.log4j.core.*
import org.apache.logging.log4j.core.appender.AbstractAppender
import org.apache.logging.log4j.core.config.Property
import org.apache.logging.log4j.core.config.plugins.Plugin
import org.apache.logging.log4j.core.config.plugins.PluginAttribute
import org.apache.logging.log4j.core.config.plugins.PluginElement
import org.apache.logging.log4j.core.config.plugins.PluginFactory
import java.io.Serializable
import java.io.StringWriter
import java.time.Instant

@Plugin(
    name = "SwingAppender",
    category = Core.CATEGORY_NAME,
    elementType = Appender.ELEMENT_TYPE,
    printObject = true
)
class SwingAppender private constructor(
    name: String,
    filter: Filter?
) : AbstractAppender(name, filter, null as Layout<out Serializable>?, true, Property.EMPTY_ARRAY) {

    override fun append(event: LogEvent) {
        val e = event.toImmutable()

        val thrownText = e.thrown?.let { t ->
            val sw = StringWriter()
            sw.appendLine(t.toString())
            t.stackTrace?.forEach { sw.appendLine("\tat $it") }
            sw.toString()
        }

        val entry = LogEntry(
            instant = Instant.ofEpochMilli(e.timeMillis),
            level = e.level,
            logger = e.loggerName ?: "",
            thread = e.threadName ?: "",
            message = e.message?.formattedMessage ?: "",
            thrown = thrownText
        )

        LogRepository.emit(entry)
    }

    companion object {
        @JvmStatic
        @PluginFactory
        fun createAppender(
            @PluginAttribute("name") name: String?,
            @PluginElement("Filter") filter: Filter?
        ): SwingAppender = SwingAppender(name ?: "SwingAppender", filter)
    }
}