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

import mediathek.tool.BandwidthFormatter;
import mediathek.tool.FileUtils;
import org.jetbrains.annotations.NotNull;

import java.io.IOException;
import java.io.InputStream;

public class MVBandwidthCountingInputStream extends InputStream {

    private static final long NANOS_PER_SECOND = 1_000_000_000L;
    private final InputStream iStream;
    private final Object lock = new Object();
    private final long startedAtNanos = System.nanoTime();
    private long currentWindowStartNanos = startedAtNanos;
    private long totalBytesRead;
    private long currentWindowBytesRead;
    private long currentBandwidth;

    public MVBandwidthCountingInputStream(InputStream in) {
        iStream = in;
    }

    @Override
    public void close() throws IOException {
        iStream.close();
        super.close();
    }

    @Override
    public int read() throws IOException {
        final int bytesRead = iStream.read();
        if (bytesRead != -1) {
            incrementBytesRead(1);
        }

        return bytesRead;
    }

    @Override
    public int read(byte @NotNull [] b) throws IOException {
        final int bytesRead = iStream.read(b);
        if (bytesRead != -1) {
            incrementBytesRead(bytesRead);
        }

        return bytesRead;
    }

    /**
     * Return the akt bandwidth used by this InputStream.
     *
     * @return akt Bandwidth in bytes per second.
     */
    public long getBandwidth() {
        synchronized (lock) {
            refreshCurrentBandwidth(System.nanoTime());
            return currentBandwidth;
        }
    }

    public long getSumByte() {
        synchronized (lock) {
            return totalBytesRead;
        }
    }

    /**
     * Return the bandwidth used by this InputStream.
     *
     * @return Bandwidth in bytes per second for the complete download.
     */
    public long getSumBandwidth() {
        synchronized (lock) {
            if (totalBytesRead <= 0) {
                return 0;
            }

            final long elapsedNanos = System.nanoTime() - startedAtNanos;
            if (elapsedNanos <= 0) {
                return 0;
            }

            return totalBytesRead * NANOS_PER_SECOND / elapsedNanos;
        }
    }

    @Override
    public String toString() {
        final long bytesRead = getSumByte();
        final long b = getSumBandwidth();

        String s = FileUtils.humanReadableByteCountBinary(bytesRead);
        return "Download: Bytes gelesen: " + s + "  Bandbreite: " + BandwidthFormatter.format(b);
    }

    private void incrementBytesRead(int value) {
        synchronized (lock) {
            final long nowNanos = System.nanoTime();
            totalBytesRead += value;
            currentWindowBytesRead += value;
            refreshCurrentBandwidth(nowNanos);
        }
    }

    private void refreshCurrentBandwidth(long nowNanos) {
        final long elapsedNanos = nowNanos - currentWindowStartNanos;
        if (elapsedNanos >= NANOS_PER_SECOND) {
            if (currentWindowBytesRead > 0) {
                currentBandwidth = currentWindowBytesRead * NANOS_PER_SECOND / elapsedNanos;
            } else {
                currentBandwidth = 0;
            }

            currentWindowBytesRead = 0;
            currentWindowStartNanos = nowNanos;
        }
    }
}
