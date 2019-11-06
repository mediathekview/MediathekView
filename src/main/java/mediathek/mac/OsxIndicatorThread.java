package mediathek.mac;

import mediathek.tool.threads.IndicatorThread;

import java.awt.*;
import java.util.concurrent.TimeUnit;

/**
 * This thread will update the percentage drawn on the dock icon on macOS.
 */
class OsxIndicatorThread extends IndicatorThread {

    private int oldPercentage;

    public OsxIndicatorThread() {
        super();
        setName("OsxIndicatorThread");
    }

    @Override
    public void run() {
        final Taskbar taskbar = Taskbar.getTaskbar();
        taskbar.setProgressValue(0);

        try {
            while (!isInterrupted()) {
                final int percentage = (int) calculateOverallPercentage();

                //update in 1pct steps...
                //if icon was already drawn, don´ do it again
                if (oldPercentage != percentage) {
                    taskbar.setProgressValue(percentage);
                }

                oldPercentage = percentage;

                TimeUnit.MILLISECONDS.sleep(500);
            }
        } catch (Exception ignored) {
        } finally {
            //reset the application dock icon
            taskbar.setProgressValue(-1);
            oldPercentage = 0;
        }
    }
}
