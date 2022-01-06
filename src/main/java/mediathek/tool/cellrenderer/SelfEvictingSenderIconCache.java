package mediathek.tool.cellrenderer;

import mediathek.tool.TimerPool;

import javax.swing.*;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.TimeUnit;

/**
 * A cache for sender icons at a specific cell dimension.
 * Will get cleared periodically.
 */
public class SelfEvictingSenderIconCache extends ConcurrentHashMap<SenderCacheKey, Icon> {
    public SelfEvictingSenderIconCache() {
        TimerPool.getTimerPool().scheduleAtFixedRate(this::clear, 5, 5, TimeUnit.MINUTES);
    }
}
