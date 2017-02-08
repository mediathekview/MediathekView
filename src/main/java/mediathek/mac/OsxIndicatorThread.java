package mediathek.mac;

import com.apple.eawt.Application;
import mediathek.config.Daten;
import mediathek.controller.starter.Start;
import mediathek.daten.DatenDownload;

import java.awt.*;
import java.awt.image.BufferedImage;
import java.util.LinkedList;

/**
 * This thread will update the percentage drawn on the dock icon on OS X.
 */
class OsxIndicatorThread extends Thread {

    private final Daten daten;
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
    private double oldPercentage = 0.0;
    private boolean bFirstUpdate = true;
    private final Application application = Application.getApplication();

    public OsxIndicatorThread() {
        setName("OSX dock icon update thread");
        daten = Daten.getInstance();

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
                int numOfDownloadsActive = 0;
                double accumPercentage = 0.0;

                //only count running/active downloads and calc accumulated progress..
                LinkedList<DatenDownload> activeDownloadList = daten.getListeDownloads().getListOfStartsNotFinished(DatenDownload.QUELLE_ALLE);
                for (DatenDownload download : activeDownloadList) {
                    if (download.start.status == Start.STATUS_RUN) {
                        numOfDownloadsActive++;
                        accumPercentage += download.start.percent / 10.0;
                    }
                }

                final double percentage = accumPercentage / numOfDownloadsActive;
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
                sleep(500);
            }
        } catch (Exception ignored) {
        } finally {
            //reset the application dock icon
            application.setDockIconImage(OsxApplicationIconImage);
            oldPercentage = 0.0;
        }
    }
}
