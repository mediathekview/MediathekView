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

package mediathek.tool.notification

import org.apache.logging.log4j.LogManager
import java.io.Closeable
import java.util.function.Supplier

/**
 * Owns the active notification center and hides platform-specific lifecycle management from application data.
 */
object NotificationService : Closeable {
    private val logger = LogManager.getLogger()
    private var notificationCenter: INotificationCenter = NullNotificationCenter()

    fun configure(notificationCenterFactory: Supplier<INotificationCenter>, enabled: Boolean) {
        closeCurrentNotificationCenter()
        notificationCenter = if (enabled) notificationCenterFactory.get() else NullNotificationCenter()
    }

    fun displayNotification(msg: NotificationMessage) {
        notificationCenter.displayNotification(msg)
    }

    override fun close() {
        closeCurrentNotificationCenter()
        notificationCenter = NullNotificationCenter()
    }

    private fun closeCurrentNotificationCenter() {
        try {
            notificationCenter.close()
        } catch (e: Exception) {
            logger.error("Failed to close notification center", e)
        }
    }
}
