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

package mediathek.tool.timer

import mediathek.gui.messages.TimerEvent
import mediathek.tool.MessageBus
import org.apache.logging.log4j.LogManager
import org.apache.logging.log4j.Logger
import java.util.concurrent.ScheduledFuture
import java.util.concurrent.ScheduledThreadPoolExecutor
import java.util.concurrent.ThreadFactory
import java.util.concurrent.TimeUnit
import java.util.concurrent.atomic.AtomicLong

object TimerPool {
    private val logger: Logger = LogManager.getLogger()
    private val executor = ScheduledThreadPoolExecutor(2, TimerPoolThreadFactory()).apply {
        removeOnCancelPolicy = true
        executeExistingDelayedTasksAfterShutdownPolicy = false
        continueExistingPeriodicTasksAfterShutdownPolicy = false
    }
    private val timerEventFuture: ScheduledFuture<*>

    init {
        logger.trace("Initializing timer pool...")
        timerEventFuture = scheduleWithFixedDelay(
            { MessageBus.messageBus.publishAsync(TimerEvent()) },
            4,
            1,
            TimeUnit.SECONDS
        )
    }

    @JvmStatic
    fun execute(command: Runnable) {
        executor.execute(command)
    }

    @JvmStatic
    fun schedule(command: Runnable, delay: Long, unit: TimeUnit): ScheduledFuture<*> =
        executor.schedule(command, delay, unit)

    @JvmStatic
    fun scheduleAtFixedRate(
        command: Runnable,
        initialDelay: Long,
        period: Long,
        unit: TimeUnit
    ): ScheduledFuture<*> = executor.scheduleAtFixedRate(command, initialDelay, period, unit)

    @JvmStatic
    fun scheduleWithFixedDelay(
        command: Runnable,
        initialDelay: Long,
        delay: Long,
        unit: TimeUnit
    ): ScheduledFuture<*> = executor.scheduleWithFixedDelay(command, initialDelay, delay, unit)

    @JvmStatic
    @Throws(InterruptedException::class)
    fun shutdown(timeout: Long, unit: TimeUnit): List<Runnable> {
        timerEventFuture.cancel(true)
        executor.shutdown()
        if (!executor.awaitTermination(timeout, unit)) {
            logger.warn("Time out occurred before timer pool termination")
        }

        return executor.shutdownNow()
    }

    private class TimerPoolThreadFactory : ThreadFactory {
        private val threadNumber = AtomicLong(1)

        override fun newThread(runnable: Runnable): Thread =
            Thread.ofVirtual()
                .name("TimerPool-virtual-thread-${threadNumber.getAndIncrement()}")
                .unstarted(runnable)
    }
}
