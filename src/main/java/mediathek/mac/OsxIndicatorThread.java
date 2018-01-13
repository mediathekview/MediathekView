package mediathek.mac;

import com.apple.eawt.Application;
import mediathek.tool.threads.IndicatorThread;

import java.awt.*;
import java.awt.image.BufferedImage;
import java.util.concurrent.TimeUnit;

/**
 * This thread will update the percentage drawn on the dock icon on OS X.
 */
class OsxIndicatorThread extends IndicatorThread {

    /**
     * The Image of the OS X application icon.
     */
    private Image OsxApplicationIconImage = null;
    /**
     * Stores the application image with the progress drawn on it
     */
    private BufferedImage newApplicationIcon = null;
    private final int appIconWidth;
    private final int appIconHeight;
    private boolean bFirstUpdate = true;
    private final Application application = Application.getApplication();
    private double oldPercentage = 0.0;


    public OsxIndicatorThread() {
        super();
        setName("OsxIndicatorThread");

        OsxApplicationIconImage = application.getDockIconImage();
        appIconWidth = OsxApplicationIconImage.getWidth(null);
        appIconHeight = OsxApplicationIconImage.getHeight(null);
        newApplicationIcon = new BufferedImage(appIconWidth, appIconHeight, BufferedImage.TYPE_INT_ARGB);
    }

    /**
     * Draw the progress bar into the application icon and set dock icon.
     *
     * @param progressBarWidth width of the bar.
     */
    private void drawAndSetApplicationIconWithProgress(int progressBarWidth) {
        Graphics g = newApplicationIcon.getGraphics();
        g.drawImage(OsxApplicationIconImage, 0, 0, null);
        g.setColor(Color.RED);
        g.fillRect(0, appIconHeight - 20, appIconWidth, 20);
        g.setColor(Color.GREEN);
        g.fillRect(0, appIconHeight - 20, progressBarWidth, 20);
        g.dispose();
        application.setDockIconImage(newApplicationIcon);
    }

    @Override
    public void run() {
        try {
            while (!isInterrupted()) {
                final double percentage = calculateOverallPercentage();
                final int progressBarWidth = (int) ((appIconWidth / 100.0) * percentage);

                if (bFirstUpdate) {
                    drawAndSetApplicationIconWithProgress(progressBarWidth);
                    bFirstUpdate = false;
                }

                //update in 1pct steps...
                if (percentage % 1 == 0) {
                    //if icon was already drawn, don´ do it again
                    if (oldPercentage != percentage) {
                        drawAndSetApplicationIconWithProgress(progressBarWidth);
                    }

                    oldPercentage = percentage;
                }
                TimeUnit.MILLISECONDS.sleep(500);
            }
        } catch (Exception ignored) {
        } finally {
            //reset the application dock icon
            application.setDockIconImage(OsxApplicationIconImage);
            oldPercentage = 0.0;
        }
    }
}
