package mediathek.controller.starter;

import java.util.concurrent.Semaphore;
import mediathek.daten.Daten;
import mediathek.tool.ListenerMediathekView;
import mediathek.tool.MVConfig;

/**
 * This singleton class provides the necessary tokens for direct file downloads.
 * It ensures that selected bandwidth limit will not be exceeded for all concurrent direct downloads.
 * Bandwidth throttling based on http://en.wikipedia.org/wiki/Token_bucket
 */
public class MVBandwidthTokenBucket {

    public static final int DEFAULT_BUFFER_SIZE = 64 * 1024; // default byte buffer size
    public static final int BANDWIDTH_MAX_RED_KBYTE = 500; // 500 kByte
    public static final int BANDWIDTH_MAX_BYTE = 1_000_000; // 1.000 kByte

    private static final MVBandwidthTokenBucket ourInstance = new MVBandwidthTokenBucket();
    private volatile int bucketCapacity = BANDWIDTH_MAX_RED_KBYTE; // 500kByte
    private final Semaphore bucketSize = new Semaphore(0, false);

    private MVBandwidthTokenBucketFillerThread fillerThread = null;

    private MVBandwidthTokenBucket() {
        setBucketCapacity(getBandwidth());
        ListenerMediathekView.addListener(new ListenerMediathekView(ListenerMediathekView.EREIGNIS_BANDBREITE, MVBandwidthTokenBucket.class.getSimpleName()) {
            @Override
            public void ping() {
                setBucketCapacity(getBandwidth());
            }
        });

    }

    public static MVBandwidthTokenBucket getInstance() {
        return ourInstance;
    }

    /**
     * Ensure that bucket filler thread is running.
     * If it running, nothing will happen.
     */
    public synchronized void ensureBucketThreadIsRunning() {
        if (fillerThread == null) {
            fillerThread = new MVBandwidthTokenBucketFillerThread();
            fillerThread.start();
        }
    }

    /**
     * Take number of byte tickets from bucket.
     *
     * @param howMany The number of bytes to acquire.
     */
    public void takeBlocking(final int howMany) {
        //if bucket size equals 0 then unlimited speed...
        if (getBucketCapacity() > 0) {
            try {
                bucketSize.acquire(howMany);
            } catch (Exception ignored) {
            }
        }
    }

    /**
     * Acquire one byte ticket from bucket.
     */
    public void takeBlocking() {
        takeBlocking(1);
    }

    /**
     * Get the capacity of the Token Bucket.
     *
     * @return Maximum number of tokens in the bucket.
     */
    public synchronized int getBucketCapacity() {
        return bucketCapacity;
    }

    /**
     * Kill the semaphore filling thread.
     */
    private void terminateFillerThread() {
        if (fillerThread != null) {
            fillerThread.interrupt();
            fillerThread = null;
        }
    }

    public synchronized void setBucketCapacity(int bucketCapacity) {
        this.bucketCapacity = bucketCapacity;
        if (bucketCapacity == BANDWIDTH_MAX_BYTE) {
            terminateFillerThread();

            //if we have waiting callers, release them by releasing buckets in the semaphore...
            while (bucketSize.hasQueuedThreads()) {
                bucketSize.release();
            }

            //reset semaphore
            bucketSize.drainPermits();
        } else {
            terminateFillerThread();

            bucketSize.drainPermits();

            //restart filler thread with new settings...
            ensureBucketThreadIsRunning();
        }
    }

    /**
     * Read bandwidth settings from config.
     *
     * @return The maximum bandwidth in bytes set or zero for unlimited speed.
     */
    private int getBandwidth() {
        int bytesPerSecond;

        try {
            final int maxKBytePerSec = (int) Long.parseLong(Daten.mVConfig.get(MVConfig.SYSTEM_BANDBREITE_KBYTE));
            bytesPerSecond = maxKBytePerSec * 1000;

        } catch (Exception ex) {
            bytesPerSecond = BANDWIDTH_MAX_RED_KBYTE;
            Daten.mVConfig.add(MVConfig.SYSTEM_BANDBREITE_KBYTE, BANDWIDTH_MAX_RED_KBYTE + "");
        }
        return bytesPerSecond;
    }

    /**
     * Fills the bucket semaphore with available download buckets for speed management.
     */
    private class MVBandwidthTokenBucketFillerThread extends Thread {

        public MVBandwidthTokenBucketFillerThread() {
            setName("MVBandwidthTokenBucket Filler Thread");
        }

        @Override
        public void run() {
            try {
                while (!isInterrupted()) {
                    final int bucketCapacity = getBucketCapacity();
                    //for unlimited speed we dont need the thread
                    if (bucketCapacity == MVBandwidthTokenBucket.BANDWIDTH_MAX_BYTE) {
                        break;
                    }

                    final int releaseCount = bucketCapacity - bucketSize.availablePermits();
                    if (releaseCount > 0) {
                        bucketSize.release(releaseCount);
                    }

                    sleep(1000);
                }
            } catch (Exception ignored) {
            }
        }
    }
}
