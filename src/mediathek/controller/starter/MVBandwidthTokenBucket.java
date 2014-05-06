package mediathek.controller.starter;

import mediathek.daten.Daten;
import mediathek.tool.ListenerMediathekView;
import mediathek.tool.MVConfig;

import java.util.concurrent.Semaphore;

/**
 * This singleton class provides the necessary tokens for direct file downloads.
 * It ensures that selected bandwidth limit will not be exceeded for all concurrent direct downloads.
 * Bandwidth throttling based on http://en.wikipedia.org/wiki/Token_bucket
 */
public class MVBandwidthTokenBucket {
    public static final int TOKEN_PERMIT_SIZE = 1000;
    private static MVBandwidthTokenBucket ourInstance = new MVBandwidthTokenBucket();
    private volatile int bucketCapacity = 1000; // 1000 KByte = 1 MByte
    private Semaphore bucketSize = new Semaphore(0, false);


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
     * Take one TOKEN_PERMIT_SIZE Byte block out of the queue.
     * If no token is available the caller will block until one gets available.
     */
    public void takeBlocking() {
        //if bucket size equals 0 then unlimited speed...
        if (getBucketCapacity() > 0) {
            try {
                bucketSize.acquire();
            } catch (Exception ignored) {
            }
        }
    }

    public synchronized int getBucketCapacity() {
        return bucketCapacity;
    }

    /**
     * Kill the semaphore filling thread.
     */
    private void terminateFillerThread()
    {
        if (fillerThread != null) {
            fillerThread.interrupt();
            fillerThread = null;
        }
    }

    public synchronized void setBucketCapacity(int bucketCapacity) {
        this.bucketCapacity = bucketCapacity;
        if (bucketCapacity == 0) {
            terminateFillerThread();

            //if we have waiting callers, release them by releasing buckets in the semaphore...
            while (bucketSize.hasQueuedThreads())
                bucketSize.release();

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
     * @return The maximum bandwidth set or zero for unlimited speed.
     */
    private int getBandwidth() {
        int maxKBytePerSec;
        try {
            maxKBytePerSec = (int) Long.parseLong(Daten.mVConfig.get(MVConfig.SYSTEM_BANDBREITE_KBYTE));
        } catch (Exception ex) {
            maxKBytePerSec = 0;
            Daten.mVConfig.add(MVConfig.SYSTEM_BANDBREITE_KBYTE, "0");
        }
        return maxKBytePerSec;
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
                    if (bucketCapacity == 0)
                        break;

                    final int releaseCount = bucketCapacity - bucketSize.availablePermits();
                    if (releaseCount > 0)
                        bucketSize.release(releaseCount);

                    sleep(1000);
                }
            } catch (Exception ignored) {
            }
        }
    }
}
