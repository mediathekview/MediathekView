package mediathek.tool.sender_icon_cache

import com.github.benmanes.caffeine.cache.Caffeine
import com.github.benmanes.caffeine.cache.LoadingCache
import mediathek.config.application.ApplicationConfiguration
import mediathek.gui.messages.SenderIconStyleChangedEvent
import mediathek.tool.MessageBus
import mediathek.tool.timer.TimerPool
import net.engio.mbassy.listener.Handler
import org.apache.logging.log4j.LogManager
import java.util.*
import java.util.concurrent.atomic.AtomicBoolean
import javax.swing.ImageIcon
import kotlin.time.Duration.Companion.hours
import kotlin.time.Duration.Companion.minutes
import kotlin.time.toJavaDuration

/**
 * This class will load only one instance for all used sender icons.
 */
object MVSenderIconCache {
    private val useLocalIcons = AtomicBoolean(false)
    private val senderCache: LoadingCache<String, Optional<ImageIcon>>
    private val logger = LogManager.getLogger()

    @Handler
    @Suppress("UNUSED_PARAMETER")
    private fun handleSenderIconStyleChangedEvent(e: SenderIconStyleChangedEvent) {
        logger.trace("invalidating caches due to sender icon style change")
        useLocalIcons.set(ApplicationConfiguration.getInstance().localSenderIcons)
        senderCache.invalidateAll()
    }

    private fun setupCleanupScheduler() {
        TimerPool.scheduleAtFixedRate({
            logger.trace("Cleaning sender icon caches")
            senderCache.cleanUp()
        }, 5.minutes, 5.minutes)
    }

    /**
     * Get the icon for a specific sender.
     *
     * @param sender The name of the supported sender.
     * @return The [javax.swing.ImageIcon] for the sender or null.
     */
    operator fun get(sender: String): Optional<ImageIcon> {
        if (sender.isBlank()) {
            return Optional.empty()
        }

        return try {
            val cached = senderCache[sender]
            if (!useLocalIcons.get() && cached.isEmpty) {
                // In wiki mode, transient network failures should not stay cached for hours.
                senderCache.invalidate(sender)
                senderCache[sender]
            } else {
                cached
            }
        } catch (_: RuntimeException) {
            Optional.empty()
        }
    }

    init {
        logger.trace("Initializing sender icon cache...")
        setupCleanupScheduler()
        val senderIconLoader = SenderIconCacheLoader(useLocalIcons)

        senderCache = Caffeine.newBuilder()
            .expireAfterAccess(2.hours.toJavaDuration())
            .build { sender -> senderIconLoader.load(sender) }

        MessageBus.messageBus.subscribe(this)
        useLocalIcons.set(ApplicationConfiguration.getInstance().localSenderIcons)
    }
}
