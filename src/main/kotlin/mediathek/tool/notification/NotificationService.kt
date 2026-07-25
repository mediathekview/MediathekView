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

/**
 * Owns the active notification backend and serializes publication, replacement, and shutdown.
 */
class NotificationService : NotificationPublisher, AutoCloseable {
    private val logger = LogManager.getLogger()
    private val lifecycleLock = Any()
    private var notificationBackend: NotificationBackend = DisabledNotificationBackend
    private var closed = false

    fun configure(notificationBackendFactory: () -> NotificationBackend, enabled: Boolean) {
        synchronized(lifecycleLock) {
            if (closed) {
                return
            }
            val previousBackend = notificationBackend
            notificationBackend = DisabledNotificationBackend
            closeNotificationBackend(previousBackend)
            notificationBackend =
                if (enabled) createNotificationBackend(notificationBackendFactory) else DisabledNotificationBackend
        }
    }

    override fun publish(notification: NotificationMessage) {
        synchronized(lifecycleLock) {
            if (closed) {
                return
            }
            try {
                notificationBackend.publish(notification)
            } catch (exception: Exception) {
                logger.error("Failed to display notification", exception)
            } catch (error: LinkageError) {
                logger.error("Failed to load notification backend", error)
            }
        }
    }

    override fun close() {
        synchronized(lifecycleLock) {
            if (closed) {
                return
            }
            closed = true
            val previousBackend = notificationBackend
            notificationBackend = DisabledNotificationBackend
            closeNotificationBackend(previousBackend)
        }
    }

    private fun closeNotificationBackend(notificationBackend: NotificationBackend) {
        try {
            notificationBackend.close()
        } catch (exception: Exception) {
            logger.error("Failed to close notification backend", exception)
        } catch (error: LinkageError) {
            logger.error("Failed to unload notification backend", error)
        }
    }

    private fun createNotificationBackend(notificationBackendFactory: () -> NotificationBackend): NotificationBackend {
        return try {
            notificationBackendFactory()
        } catch (exception: Exception) {
            logger.error("Failed to create notification backend", exception)
            DisabledNotificationBackend
        } catch (error: LinkageError) {
            logger.error("Failed to load notification backend", error)
            DisabledNotificationBackend
        }
    }
}
