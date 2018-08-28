package mediathek.mac;

import mediathek.tool.threads.IndicatorThread;

import java.awt.*;
import java.util.concurrent.TimeUnit;

/**
 * This thread will update the percentage drawn on the dock icon on OS X.
 */
class OsxIndicatorThread extends IndicatorThread {

    private int oldPercentage;

    public OsxIndicatorThread() {
        super();
        setName("OsxIndicatorThread");
    }

    @Override
    public void run() {
        Taskbar.getTaskbar().setProgressValue(0);

        try {
            while (!isInterrupted()) {
                final int percentage = (int) calculateOverallPercentage();

                //update in 1pct steps...
                //if icon was already drawn, don´ do it again
                if (oldPercentage != percentage) {
                    Taskbar.getTaskbar().setProgressValue(percentage);
                }

                oldPercentage = percentage;

                TimeUnit.MILLISECONDS.sleep(500);
            }
        } catch (Exception ignored) {
        } finally {
            //reset the application dock icon
            Taskbar.getTaskbar().setProgressValue(-1);
            oldPercentage = 0;
        }
    }
}
