package mediathek.tool.threads;

import mediathek.config.Daten;
import mediathek.controller.starter.Start;
import mediathek.daten.DatenDownload;

/**
 * Base class for platform-specific progress indicator threads
 */
public class IndicatorThread extends Thread {
    protected final Daten daten;


    public IndicatorThread() {
        setName("IndicatorThread");
        daten = Daten.getInstance();
    }

    protected double calculateOverallPercentage() {
        int numOfDownloadsActive = 0;
        double accumPercentage = 0.0;
        //only count running/active downloads and calc accumulated progress..
        var activeDownloadList = daten.getListeDownloads().getListOfStartsNotFinished(DatenDownload.QUELLE_ALLE);
        for (DatenDownload download : activeDownloadList) {
            if (download.start.status == Start.STATUS_RUN) {
                numOfDownloadsActive++;
                accumPercentage += download.start.percent / 10.0;
            }
        }
        activeDownloadList.clear();

        return accumPercentage / numOfDownloadsActive;
    }
}
