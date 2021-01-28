package mediathek.tool.sender_icon_cache

import com.google.common.cache.CacheBuilder
import com.google.common.cache.CacheLoader.InvalidCacheLoadException
import com.google.common.cache.LoadingCache
import mediathek.gui.messages.SenderIconStyleChangedEvent
import mediathek.tool.ApplicationConfiguration
import mediathek.tool.MessageBus
import net.engio.mbassy.listener.Handler
import org.apache.logging.log4j.LogManager
import java.util.*
import java.util.concurrent.ExecutionException
import java.util.concurrent.Executors
import java.util.concurrent.TimeUnit
import java.util.concurrent.atomic.AtomicBoolean
import javax.swing.ImageIcon

/**
 * This class will load only one instance for all used sender icons.
 */
class MVSenderIconCache {
    private val useLocalIcons = AtomicBoolean(false)
    private val smallSenderCache: LoadingCache<String, Optional<ImageIcon>>
    private val largeSenderCache: LoadingCache<String, Optional<ImageIcon>>
    private val cacheCleanupScheduler = Executors.newScheduledThreadPool(1)

    @Handler
    private fun handleSenderIconStyleChangedEvent(e: SenderIconStyleChangedEvent) {
        logger.trace("invalidating caches due to sender icon style change")
        useLocalIcons.set(ApplicationConfiguration.getConfiguration().getBoolean(CONFIG_USE_LOCAL_SENDER_ICONS, false))
        smallSenderCache.invalidateAll()
        largeSenderCache.invalidateAll()
    }

    private fun setupCleanupScheduler() {
        cacheCleanupScheduler.scheduleAtFixedRate({
            largeSenderCache.cleanUp()
            smallSenderCache.cleanUp()
        }, 5, 5, TimeUnit.MINUTES)
    }

    /**
     * Get the icon for a specific sender.
     *
     * @param sender The name of the supported sender.
     * @param small  large or small icon requested.
     * @return The [javax.swing.ImageIcon] for the sender or null.
     */
    operator fun get(sender: String, small: Boolean): Optional<ImageIcon> {
        return try {
            if (small)
                smallSenderCache[sender]
            else
                largeSenderCache[sender]
        } catch (ex: InvalidCacheLoadException) {
            Optional.empty()
        } catch (ex: ExecutionException) {
            Optional.empty()
        }
    }

    companion object {
        const val CONFIG_USE_LOCAL_SENDER_ICONS = "application.sender_icons.use_local"
        private val logger = LogManager.getLogger()
    }

    init {
        setupCleanupScheduler()

        largeSenderCache = CacheBuilder.newBuilder()
            .expireAfterAccess(2, TimeUnit.HOURS)
            .build(SenderIconCacheLoader(SenderIconSize.LARGE, useLocalIcons))
        smallSenderCache = CacheBuilder.newBuilder()
            .expireAfterAccess(2, TimeUnit.HOURS)
            .build(SenderIconCacheLoader(SenderIconSize.SMALL, useLocalIcons))

        MessageBus.messageBus.subscribe(this)
        useLocalIcons.set(ApplicationConfiguration.getConfiguration().getBoolean(CONFIG_USE_LOCAL_SENDER_ICONS, false))
    }
}