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

package mediathek.tool.time;

import java.time.Duration;
import java.time.Instant;
import java.util.Locale;
import java.util.concurrent.TimeUnit;

/**
 * Lightweight stopwatch replacement for Guava Stopwatch.
 */
public final class Stopwatch {
    private Duration elapsed = Duration.ZERO;
    private Instant startedAt;
    private boolean running;

    private Stopwatch() {
    }

    public static Stopwatch createStarted() {
        return new Stopwatch().start();
    }

    public static Stopwatch createUnstarted() {
        return new Stopwatch();
    }

    public synchronized Stopwatch start() {
        if (running) {
            throw new IllegalStateException("This stopwatch is already running.");
        }
        startedAt = Instant.now();
        running = true;
        return this;
    }

    public synchronized Stopwatch stop() {
        if (!running) {
            throw new IllegalStateException("This stopwatch is already stopped.");
        }
        elapsed = elapsed.plus(Duration.between(startedAt, Instant.now()));
        startedAt = null;
        running = false;
        return this;
    }

    public synchronized Stopwatch reset() {
        elapsed = Duration.ZERO;
        startedAt = null;
        running = false;
        return this;
    }

    public synchronized boolean isRunning() {
        return running;
    }

    public synchronized Duration elapsed() {
        if (!running) {
            return elapsed;
        }
        return elapsed.plus(Duration.between(startedAt, Instant.now()));
    }

    public synchronized long elapsed(TimeUnit desiredUnit) {
        return desiredUnit.convert(elapsed().toNanos(), TimeUnit.NANOSECONDS);
    }

    @Override
    public synchronized String toString() {
        long nanos = elapsed().toNanos();
        TimeUnit unit = chooseUnit(nanos);
        double value = nanos / (double) TimeUnit.NANOSECONDS.convert(1, unit);
        return String.format(Locale.ROOT, "%.4g %s", value, abbreviate(unit));
    }

    private static TimeUnit chooseUnit(long nanos) {
        if (TimeUnit.NANOSECONDS.toDays(nanos) > 0) {
            return TimeUnit.DAYS;
        }
        if (TimeUnit.NANOSECONDS.toHours(nanos) > 0) {
            return TimeUnit.HOURS;
        }
        if (TimeUnit.NANOSECONDS.toMinutes(nanos) > 0) {
            return TimeUnit.MINUTES;
        }
        if (TimeUnit.NANOSECONDS.toSeconds(nanos) > 0) {
            return TimeUnit.SECONDS;
        }
        if (TimeUnit.NANOSECONDS.toMillis(nanos) > 0) {
            return TimeUnit.MILLISECONDS;
        }
        if (TimeUnit.NANOSECONDS.toMicros(nanos) > 0) {
            return TimeUnit.MICROSECONDS;
        }
        return TimeUnit.NANOSECONDS;
    }

    private static String abbreviate(TimeUnit unit) {
        return switch (unit) {
            case NANOSECONDS -> "ns";
            case MICROSECONDS -> "us";
            case MILLISECONDS -> "ms";
            case SECONDS -> "s";
            case MINUTES -> "min";
            case HOURS -> "h";
            case DAYS -> "d";
        };
    }
}
