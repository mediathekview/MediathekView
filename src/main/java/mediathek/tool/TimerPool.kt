package mediathek.tool

import mediathek.gui.messages.TimerEvent
import mediathek.tool.MessageBus.messageBus
import org.apache.logging.log4j.LogManager
import org.apache.logging.log4j.Logger
import java.util.concurrent.ScheduledThreadPoolExecutor
import java.util.concurrent.ThreadFactory
import java.util.concurrent.TimeUnit
import java.util.concurrent.atomic.AtomicInteger

object TimerPool {
    private val logger: Logger = LogManager.getLogger()

    @JvmStatic
    val timerPool = ScheduledThreadPoolExecutor(2, TimerPoolThreadFactory())

    init {
        logger.trace("Initializing timer pool...")
        //get rid of cancelled tasks immediately...
        timerPool.removeOnCancelPolicy = true
        timerPool.allowCoreThreadTimeOut(true)
        timerPool.setKeepAliveTime(30, TimeUnit.SECONDS)
        timerPool.scheduleWithFixedDelay({ messageBus.publishAsync(TimerEvent()) }, 4, 1, TimeUnit.SECONDS)
    }

    /**
     * Thread factory to give timer pool threads a recognizable name.
     * Follows the java.util.concurrent.Executors.DefaultThreadFactory implementation for
     * setting up the threads.
     */
    private class TimerPoolThreadFactory : ThreadFactory {
        private val threadNumber = AtomicInteger(1)

        override fun newThread(r: Runnable): Thread {
            val t = Thread(Thread.currentThread().threadGroup, r, "TimerPool-thread-${threadNumber.getAndIncrement()}", 0)
            t.isDaemon = false
            t.priority = Thread.NORM_PRIORITY

            return t
        }
    }
}