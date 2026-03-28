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

package mediathek.tool.cellrenderer;

import mediathek.tool.timer.TimerPool;

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
