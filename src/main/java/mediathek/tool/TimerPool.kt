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
    val timerPool =
        ScheduledThreadPoolExecutor(Runtime.getRuntime().availableProcessors() / 2, TimerPoolThreadFactory())

    init {
        logger.trace("Initializing timer pool...")
        //get rid of cancelled tasks immediately...
        timerPool.removeOnCancelPolicy = true
        timerPool.allowCoreThreadTimeOut(true)
        timerPool.setKeepAliveTime(1, TimeUnit.MINUTES)
        timerPool.scheduleWithFixedDelay({ messageBus.publishAsync(TimerEvent()) }, 4, 1, TimeUnit.SECONDS)
    }

    /**
     * Thread factory to give timer pool threads a recognizable name.
     * Follows the java.util.concurrent.Executors.DefaultThreadFactory implementation for
     * setting up the threads.
     */
    internal class TimerPoolThreadFactory internal constructor() : ThreadFactory {
        private val group: ThreadGroup
        private val threadNumber = AtomicInteger(1)
        private val namePrefix: String
        override fun newThread(r: Runnable): Thread {
            val t = Thread(
                group, r,
                namePrefix + threadNumber.getAndIncrement(),
                0
            )
            if (t.isDaemon) t.isDaemon = false
            if (t.priority != Thread.NORM_PRIORITY) t.priority = Thread.NORM_PRIORITY
            return t
        }

        init {
            val s = System.getSecurityManager()
            group = if (s != null) s.threadGroup else Thread.currentThread().threadGroup
            namePrefix = "TimerPool-thread-"
        }
    }
}