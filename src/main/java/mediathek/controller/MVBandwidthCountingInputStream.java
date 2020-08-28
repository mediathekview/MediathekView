package mediathek.controller;

import mediathek.config.Daten;
import mediathek.daten.DatenDownload;
import org.apache.commons.io.FileUtils;
import org.jetbrains.annotations.NotNull;

import java.io.IOException;
import java.io.InputStream;
import java.util.concurrent.ScheduledFuture;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicLong;

public class MVBandwidthCountingInputStream extends InputStream {

    private final InputStream iStream;
    private final BandwidthCalculationTask calculationTask = new BandwidthCalculationTask();
    private final ScheduledFuture<?> calculationTaskFuture;

    public MVBandwidthCountingInputStream(InputStream in) {
        iStream = in;

        //start bandwidth calculation
        calculationTaskFuture = Daten.getInstance().getTimerPool().scheduleAtFixedRate(calculationTask,0,1, TimeUnit.SECONDS);
    }

    @Override
    public void close() throws IOException {
        iStream.close();
        super.close();

        //stop bandwidth calculation
        if (calculationTaskFuture != null)
            calculationTaskFuture.cancel(true);
    }

    @Override
    public int read() throws IOException {
        final int bytesRead = iStream.read();
        if (bytesRead != -1) {
            calculationTask.incrementBytesRead(1);
        }

        return bytesRead;
    }

    @Override
    public int read(@NotNull byte[] b) throws IOException {
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
        String s = FileUtils.byteCountToDisplaySize(bytesRead);
        return "Download: Bytes gelesen: " + s + "  Bandbreite: " + DatenDownload.getTextBandbreite(b);
    }

    /**
     * This TimerTask calculates the bandwidth (bytes per seconds) and records the overall bytes read
     * until termination.
     */
    private static class BandwidthCalculationTask implements Runnable {

        private final AtomicLong _oldTotalBytes = new AtomicLong(0);
        private final AtomicLong _totalBytesRead = new AtomicLong(0);
        private final AtomicLong _bandwidth = new AtomicLong(0);
        private final AtomicLong _sumTime = new AtomicLong(0);

        @Override
        public void run() {
            _sumTime.incrementAndGet();
            final long totalBytesRead = _totalBytesRead.get();
            _bandwidth.set(totalBytesRead - _oldTotalBytes.getAndSet(totalBytesRead));
        }

        public void incrementBytesRead(int value) {
            _totalBytesRead.addAndGet(value);
        }

        /**
         * Return the total number of bytes read.
         *
         * @return Total number of bytes read.
         */
        public long getTotalBytesRead() {
            return _totalBytesRead.get();
        }

        /**
         * Return the bandwidth used by this stream.
         *
         * @return Bandwidth in bytes per second.
         */
        public long getBandwidth() {
            return _bandwidth.get();
        }

        /**
         * Return the sum of time used by this stream.
         *
         * @return Time in s
         */
        public long getSumTime() {
            return _sumTime.get();
        }
    }
}
