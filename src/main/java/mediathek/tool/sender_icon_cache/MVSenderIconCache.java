package mediathek.tool.sender_icon_cache;

import com.google.common.cache.CacheBuilder;
import com.google.common.cache.CacheLoader;
import com.google.common.cache.LoadingCache;
import mediathek.config.Daten;
import mediathek.gui.messages.SenderIconStyleChangedEvent;
import mediathek.tool.ApplicationConfiguration;
import net.engio.mbassy.listener.Handler;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import javax.swing.*;
import java.util.Optional;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.Executors;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicBoolean;

/**
 * This class will load only one instance for all used sender icons.
 */
public class MVSenderIconCache {
    public static final String CONFIG_USE_LOCAL_SENDER_ICONS = "application.sender_icons.use_local";
    private static final Logger logger = LogManager.getLogger();
    private final AtomicBoolean useLocalIcons = new AtomicBoolean(false);
    private final LoadingCache<String, Optional<ImageIcon>> senderCache_small;
    private final LoadingCache<String, Optional<ImageIcon>> senderCache;
    private final ScheduledExecutorService cacheCleanupScheduler = Executors.newScheduledThreadPool(1);

    public MVSenderIconCache(Daten d) {
        setupCleanupScheduler();

        senderCache = CacheBuilder.newBuilder()
                .expireAfterAccess(2, TimeUnit.HOURS)
                .build(new SenderIconCacheLoader(SenderIconSize.LARGE, useLocalIcons));

        senderCache_small = CacheBuilder.newBuilder()
                .expireAfterAccess(2, TimeUnit.HOURS)
                .build(new SenderIconCacheLoader(SenderIconSize.SMALL, useLocalIcons));

        d.getMessageBus().subscribe(this);
        useLocalIcons.set(ApplicationConfiguration.getConfiguration().getBoolean(CONFIG_USE_LOCAL_SENDER_ICONS, false));
    }

    @Handler
    private void handleSenderIconStyleChangedEvent(SenderIconStyleChangedEvent e) {
        logger.trace("invalidating caches due to sender icon style change");
        useLocalIcons.set(ApplicationConfiguration.getConfiguration().getBoolean(CONFIG_USE_LOCAL_SENDER_ICONS, false));
        senderCache_small.invalidateAll();
        senderCache.invalidateAll();
    }

    private void setupCleanupScheduler() {
        cacheCleanupScheduler.scheduleAtFixedRate(() -> {
            senderCache.cleanUp();
            senderCache_small.cleanUp();
        }, 5, 5, TimeUnit.MINUTES);
    }

    /**
     * Get the icon for a specific sender.
     *
     * @param sender The name of the supported sender.
     * @param small  large or small icon requested.
     * @return The {@link javax.swing.ImageIcon} for the sender or null.
     */
    public Optional<ImageIcon> get(String sender, boolean small) {
        Optional<ImageIcon> icon;
        try {
            if (small)
                icon = senderCache_small.get(sender);
            else
                icon = senderCache.get(sender);
        } catch (CacheLoader.InvalidCacheLoadException | ExecutionException ex) {
            icon = Optional.empty();
        }

        return icon;
    }

}
