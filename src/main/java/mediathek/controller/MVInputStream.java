/*    
 *    MediathekView
 *    Copyright (C) 2013   W. Xaver
 *    W.Xaver[at]googlemail.com
 *    http://zdfmediathk.sourceforge.net/
 * 
 *    org.apache.hadoop.tools.util.ThrottledInputStream
 *    unter http://www.apache.org/licenses/LICENSE-2.0
 *    diente als Vorlage
 *    
 *    This program is free software: you can redistribute it and/or modify
 *    it under the terms of the GNU General Public License as published by
 *    the Free Software Foundation, either version 3 of the License, or
 *    any later version.
 *
 *    This program is distributed in the hope that it will be useful,
 *    but WITHOUT ANY WARRANTY; without even the implied warranty of
 *    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *    GNU General Public License for more details.
 *
 *    You should have received a copy of the GNU General Public License
 *    along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */
package mediathek.controller;

import mediathek.daten.DatenDownload;
import mediathek.tool.MVFilmSize;

import java.io.IOException;
import java.io.InputStream;
import java.util.TimerTask;
import java.util.concurrent.locks.ReentrantReadWriteLock;

public class MVInputStream extends InputStream {

    private final InputStream iStream;
    private MVBandwidthTokenBucket bucket = null;
    private final BandwidthCalculationTask calculationTask;

    public MVInputStream(InputStream in, java.util.Timer calculationTimer) {
        iStream = in;
        bucket = new MVBandwidthTokenBucket();
        bucket.ensureBucketThreadIsRunning();

        //start bandwidth calculation
        calculationTask = new BandwidthCalculationTask();
        calculationTimer.scheduleAtFixedRate(calculationTask, 0, 1000);
    }

    @Override
    public void close() throws IOException {
        iStream.close();
        super.close();
        //stop bandwidth calculation
        calculationTask.cancel();
    }

    @Override
    public int read() throws IOException {
        bucket.takeBlocking();
        final int bytesRead = iStream.read();
        if (bytesRead != -1) {
            calculationTask.incrementBytesRead(1);
        }

        return bytesRead;
    }

    @Override
    public int read(byte[] b) throws IOException {
        bucket.takeBlocking(b.length);
        final int bytesRead = iStream.read(b);
        if (bytesRead != -1) {
            calculationTask.incrementBytesRead(bytesRead);
        }

        return bytesRead;
    }

    /**
     * Return the akt bandwidth used by this InputStream.
     *
     * @return akt Bandwidth in bytes per second.
     */
    public long getBandwidth() {
        return calculationTask.getBandwidth();
    }

    /**
     * Return the sum time used by this InputStream.
     *
     * @return time in second.
     */
    public long getSumTime() {
        return calculationTask.getSumTime();
    }

    public long getSumByte() {
        return calculationTask.getTotalBytesRead();
    }

    /**
     * Return the bandwidth used by this InputStream.
     *
     * @return Bandwidth in bytes per second for the complete download.
     */
    public long getSumBandwidth() {
        final long bytesRead = calculationTask.getTotalBytesRead();
        final long time = calculationTask.getSumTime();
        return bytesRead <= 0 ? 0 : bytesRead / time;
    }

    @Override
    public String toString() {
        final long bytesRead = calculationTask.getTotalBytesRead();
        final long b = getSumBandwidth();
        String s = MVFilmSize.humanReadableByteCount(bytesRead, true);
        return "Download: Bytes gelesen: " + s + "  Bandbreite: " + DatenDownload.getTextBandbreite(b);
    }

    public String[] getMsg() {
        final long bytesRead = calculationTask.getTotalBytesRead();
        final long b = getSumBandwidth();
        String s = MVFilmSize.humanReadableByteCount(bytesRead, true);
        return new String[]{"Download", "Bytes gelesen: " + s, "Bandbreite: " + DatenDownload.getTextBandbreite(b)};
    }

    /**
     * This TimerTask calculates the bandwidth (bytes per seconds) and records the overall bytes read
     * until termination.
     */
    private class BandwidthCalculationTask extends TimerTask {

        private long oldTotalBytes = 0;
        private long totalBytesRead = 0;
        private long bandwidth = 0;
        private long sumTime = 0;
        private final ReentrantReadWriteLock lock = new ReentrantReadWriteLock();

        @Override
        public void run() {
            lock.writeLock().lock();
            bandwidth = totalBytesRead - oldTotalBytes;
            oldTotalBytes = totalBytesRead;
            ++sumTime;
            lock.writeLock().unlock();
        }

        public void incrementBytesRead(int value) {
            lock.writeLock().lock();
            totalBytesRead += value;
            lock.writeLock().unlock();
        }

        /**
         * Return the total number of bytes read.
         *
         * @return Total number of bytes read.
         */
        public long getTotalBytesRead() {
            lock.readLock().lock();
            final long res = totalBytesRead;
            lock.readLock().unlock();
            return res;
        }

        /**
         * Return the bandwidth used by this stream.
         *
         * @return Bandwidth in bytes per second.
         */
        public long getBandwidth() {
            lock.readLock().lock();
            final long bw = bandwidth;
            lock.readLock().unlock();
            return bw;
        }

        /**
         * Return the sum of time used by this stream.
         *
         * @return Time in s
         */
        public long getSumTime() {
            lock.readLock().lock();
            final long t = sumTime;
            lock.readLock().unlock();
            return t;
        }
    }
}
