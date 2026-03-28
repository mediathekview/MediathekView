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

package mediathek.controller;

import java.util.concurrent.locks.LockSupport;

/**
 * Lightweight byte-based rate limiter using a monotonic clock.
 * Replaces Google Guava's RateLimiter.
 */
public final class ByteRateLimiter {
    private static final long NANOS_PER_SECOND = 1_000_000_000L;
    private static final long UNLIMITED_THRESHOLD = Long.MAX_VALUE / 2;

    private volatile long bytesPerSecond;
    private volatile boolean unlimited;
    private long nextAvailableNanos;

    public ByteRateLimiter(long bytesPerSecond) {
        setRate(bytesPerSecond);
    }

    private static long nanosForPermits(int permits, long rateBytesPerSecond) {
        final double nanos = (permits * (double) NANOS_PER_SECOND) / rateBytesPerSecond;
        if (!Double.isFinite(nanos) || nanos >= Long.MAX_VALUE) {
            return Long.MAX_VALUE;
        }
        return Math.max(1L, (long) Math.ceil(nanos));
    }

    private static long safeAdd(long a, long b) {
        if (Long.MAX_VALUE - a < b) {
            return Long.MAX_VALUE;
        }
        return a + b;
    }

    public void setRate(long bytesPerSecond) {
        synchronized (this) {
            this.unlimited = bytesPerSecond >= UNLIMITED_THRESHOLD;
            this.bytesPerSecond = bytesPerSecond <= 0 ? 1 : bytesPerSecond;
            if (unlimited) {
                nextAvailableNanos = 0L;
            }
            else {
                // Apply rate changes quickly and avoid carrying stale delay debt.
                nextAvailableNanos = Math.min(nextAvailableNanos, System.nanoTime());
            }
        }
    }

    public void acquire(int permits) {
        if (permits <= 0) {
            return;
        }

        final long waitNanos;
        synchronized (this) {
            if (unlimited) {
                return;
            }
            final long now = System.nanoTime();
            final long reservationStart = Math.max(now, nextAvailableNanos);
            final long intervalNanos = nanosForPermits(permits, bytesPerSecond);
            nextAvailableNanos = safeAdd(reservationStart, intervalNanos);
            waitNanos = reservationStart - now;
        }

        if (waitNanos > 0) {
            LockSupport.parkNanos(waitNanos);
        }
    }
}
