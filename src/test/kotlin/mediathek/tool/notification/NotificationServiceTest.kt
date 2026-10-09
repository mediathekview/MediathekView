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

import org.junit.jupiter.api.AfterEach
import org.junit.jupiter.api.Assertions.*
import org.junit.jupiter.api.Test
import java.util.concurrent.CountDownLatch
import java.util.concurrent.Executors
import java.util.concurrent.TimeUnit
import java.util.concurrent.atomic.AtomicInteger

class NotificationServiceTest {
    private val notificationService = NotificationService()

    @AfterEach
    fun closeNotificationService() {
        notificationService.close()
    }

    @Test
    fun `reconfiguration waits for notification delivery to finish`() {
        val deliveryStarted = CountDownLatch(1)
        val releaseDelivery = CountDownLatch(1)
        val previousCenterClosed = CountDownLatch(1)
        val reconfigurationStarted = CountDownLatch(1)
        val executor = Executors.newFixedThreadPool(2)
        val previousCenter = object : NotificationBackend {
            override fun publish(notification: NotificationMessage) {
                deliveryStarted.countDown()
                check(releaseDelivery.await(5, TimeUnit.SECONDS)) { "Notification delivery was not released" }
            }

            override fun close() {
                previousCenterClosed.countDown()
            }
        }

        try {
            notificationService.configure({ previousCenter }, true)

            val delivery = executor.submit {
                notificationService.publish(notificationMessage())
            }
            assertTrue(deliveryStarted.await(5, TimeUnit.SECONDS), "Notification delivery did not start")

            val reconfiguration = executor.submit {
                reconfigurationStarted.countDown()
                notificationService.configure({ DisabledNotificationBackend }, true)
            }

            assertTrue(reconfigurationStarted.await(5, TimeUnit.SECONDS), "Reconfiguration did not start")
            assertFalse(
                previousCenterClosed.await(250, TimeUnit.MILLISECONDS),
                "The active notification center was closed during delivery",
            )

            releaseDelivery.countDown()
            delivery.get(5, TimeUnit.SECONDS)
            reconfiguration.get(5, TimeUnit.SECONDS)

            assertTrue(previousCenterClosed.await(5, TimeUnit.SECONDS), "Previous notification center was not closed")
        } finally {
            releaseDelivery.countDown()
            executor.shutdownNow()
        }
    }

    @Test
    fun `close is terminal and idempotent`() {
        val closes = AtomicInteger()
        val replacementCreations = AtomicInteger()
        val backend = object : NotificationBackend {
            override fun publish(notification: NotificationMessage) {}
            override fun close() {
                closes.incrementAndGet()
            }
        }
        notificationService.configure({ backend }, true)

        notificationService.close()
        notificationService.close()
        notificationService.configure(
            {
                replacementCreations.incrementAndGet()
                DisabledNotificationBackend
            },
            true,
        )

        assertEquals(1, closes.get())
        assertEquals(0, replacementCreations.get())
    }

    @Test
    fun `backend construction failure leaves notifications disabled`() {
        notificationService.configure({ error("Backend failed") }, true)

        notificationService.publish(notificationMessage())
    }

    @Test
    fun `backend linkage failure leaves notifications disabled`() {
        notificationService.configure({ throw UnsatisfiedLinkError("Backend unavailable") }, true)

        notificationService.publish(notificationMessage())
    }

    private fun notificationMessage(): NotificationMessage =
        NotificationMessage("Title", "Message", MessageType.INFO)
}
