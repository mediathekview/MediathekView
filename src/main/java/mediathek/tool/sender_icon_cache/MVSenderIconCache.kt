package mediathek.tool.sender_icon_cache

import com.google.common.cache.CacheBuilder
import com.google.common.cache.CacheLoader.InvalidCacheLoadException
import com.google.common.cache.LoadingCache
import mediathek.gui.messages.SenderIconStyleChangedEvent
import mediathek.tool.ApplicationConfiguration
import mediathek.tool.MessageBus
import mediathek.tool.TimerPool
import net.engio.mbassy.listener.Handler
import org.apache.logging.log4j.LogManager
import java.util.*
import java.util.concurrent.ExecutionException
import java.util.concurrent.TimeUnit
import java.util.concurrent.atomic.AtomicBoolean
import javax.swing.ImageIcon

/**
 * This class will load only one instance for all used sender icons.
 */
object MVSenderIconCache {
    private val useLocalIcons = AtomicBoolean(false)
    private val senderCache: LoadingCache<String, Optional<ImageIcon>>
    private val logger = LogManager.getLogger()
    const val CONFIG_USE_LOCAL_SENDER_ICONS = "application.sender_icons.use_local"


    @Handler
    @Suppress("UNUSED_PARAMETER")
    private fun handleSenderIconStyleChangedEvent(e: SenderIconStyleChangedEvent) {
        logger.trace("invalidating caches due to sender icon style change")
        useLocalIcons.set(ApplicationConfiguration.getConfiguration().getBoolean(CONFIG_USE_LOCAL_SENDER_ICONS, false))
        senderCache.invalidateAll()
    }

    private fun setupCleanupScheduler() {
        TimerPool.timerPool.scheduleAtFixedRate({
            logger.trace("Cleaning sender icon caches")
            senderCache.cleanUp()
        }, 5, 5, TimeUnit.MINUTES)
    }

    /**
     * Get the icon for a specific sender.
     *
     * @param sender The name of the supported sender.
     * @return The [javax.swing.ImageIcon] for the sender or null.
     */
    @JvmStatic
    operator fun get(sender: String): Optional<ImageIcon> {
        return try {
                senderCache[sender]
        } catch (ex: InvalidCacheLoadException) {
            Optional.empty()
        } catch (ex: ExecutionException) {
            Optional.empty()
        }
    }

    init {
        logger.trace("Initializing sender icon cache...")
        setupCleanupScheduler()

        senderCache = CacheBuilder.newBuilder()
            .expireAfterAccess(2, TimeUnit.HOURS)
            .build(SenderIconCacheLoader(useLocalIcons))

        MessageBus.messageBus.subscribe(this)
        useLocalIcons.set(ApplicationConfiguration.getConfiguration().getBoolean(CONFIG_USE_LOCAL_SENDER_ICONS, false))
    }
}