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
package mediathek.tool;

import mediathek.controller.starter.MVBandwidthTokenBucket;

import java.io.IOException;
import java.io.InputStream;
import java.util.TimerTask;
import java.util.concurrent.locks.ReentrantReadWriteLock;

public class MVInputStream extends InputStream {

    private final InputStream iStream;
    private MVBandwidthTokenBucket bucket = null;
    private BandwidthCalculationTask calculationTask;

    public MVInputStream(InputStream in, java.util.Timer calculationTimer) {
        iStream = in;
        bucket = MVBandwidthTokenBucket.getInstance();
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
        if (bytesRead != -1)
            calculationTask.incrementBytesRead(1);

        return bytesRead;
    }

    @Override
    public int read(byte[] b) throws IOException {
        bucket.takeBlocking(b.length);
        final int bytesRead = iStream.read(b);
        if (bytesRead != -1)
            calculationTask.incrementBytesRead(bytesRead);

        return bytesRead;
    }

    /**
     * Return the bandwidth used by this InputStream.
     *
     * @return Bandwidth in bytes per second.
     */
    public long getBandwidth() {
        return calculationTask.getBandwidth();
    }

    @Override
    public String toString() {
        final long bytesRead = calculationTask.getTotalBytesRead();
        final long bandwidth = calculationTask.getBandwidth();

        return "Download: "
                + "gelesen: " + (bytesRead > 0 ? bytesRead / 1024 : 0) + " KiB, "
                + "Bandbreite: " + (bandwidth > 0 ? bandwidth / 1024 : 0) + " KiB/s ";
    }

    /**
     * This TimerTask calculates the bandwidth (bytes per seconds) and records the overall bytes read
     * until termination.
     */
    private class BandwidthCalculationTask extends TimerTask {
        private long oldTotalBytes = 0;
        private long totalBytesRead = 0;
        private long bandwidth = 0;
        private ReentrantReadWriteLock lock = new ReentrantReadWriteLock();

        @Override
        public void run() {
            lock.writeLock().lock();
            bandwidth = totalBytesRead - oldTotalBytes;
            oldTotalBytes = totalBytesRead;
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
    }
}
