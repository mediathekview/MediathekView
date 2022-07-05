package mediathek.daten;

import mediathek.config.Daten;
import mediathek.controller.starter.Start;
import mediathek.gui.messages.DownloadInfoUpdateAvailableEvent;
import mediathek.gui.messages.TimerEvent;
import mediathek.tool.MessageBus;
import net.engio.mbassy.listener.Handler;

import java.text.DecimalFormat;

public class DownloadInfos {
    private static final DecimalFormat formatter = new DecimalFormat("####0.00");
    /**
     * Bandbreite: bytes per second
     */
    private long bandwidth;
    /**
     * Restzeit aller gestarteten Downloads
     */
    private long timeRestAllDownloads;
    /**
     * Restzeit für die gerade ladenden/laufenden Downloads
     */
    private long timeRestAktDownloads;
    /**
     * Anzahl Bytes bereits geladen für die gerade ladenden/laufenden Downloads
     */
    private long byteAktDownloads;
    /**
     * Anzahl Bytes für alle gestarteten Downloads
     */
    private long byteAlleDownloads;
    /**
     * Anzahl gestarteter Downloads
     */
    private int anzDownloadsRun;

    private String bandwidthStr = "";

    public DownloadInfos() {
        MessageBus.getMessageBus().subscribe(this);
    }

    public long getBandwidth() {
        return bandwidth;
    }

    public long getTimeRestAllDownloads() {
        return timeRestAllDownloads;
    }

    public long getTimeRestAktDownloads() {
        return timeRestAktDownloads;
    }

    public long getByteAktDownloads() {
        return byteAktDownloads;
    }

    public long getByteAlleDownloads() {
        return byteAlleDownloads;
    }

    public String getBandwidthStr() {
        return bandwidthStr;
    }

    public void formatBandwidthString() {
        if (bandwidth > 1_000_000.0) {
            bandwidthStr = formatter.format(bandwidth / 1_000_000.0) + " MByte/s";
        } else if (bandwidth > 1_000.0) {
            bandwidthStr = Math.round(bandwidth / 1_000.0) + " kByte/s";
        } else {
            bandwidthStr = bandwidth + " Byte/s";
        }
    }

    public String getGesamtRestzeit() {
        if (timeRestAllDownloads > 0) {
            if (timeRestAllDownloads < 60) {
                return "< 1 Min";
            } else {
                return timeRestAllDownloads / 60 + " Min";
            }
        }
        return "";
    }

    public String getRestzeit() {
        if (timeRestAktDownloads > 0) {
            if (timeRestAktDownloads < 60) {
                return "< 1 Min";
            } else {
                return timeRestAktDownloads / 60 + " Min";
            }
        }
        return "";
    }

    @Handler
    private void handleTimerEvent(TimerEvent e) {
        makeDownloadInfos();
    }

    private void makeDownloadInfos() {
        resetData();

        final var aktivDownloads = Daten.getInstance()
                .getListeDownloads().getListOfStartsNotFinished(DatenDownload.QUELLE_ALLE);

        // Liste gestarteter Downloads
        for (DatenDownload download : aktivDownloads) {
            anzDownloadsRun++;
            byteAlleDownloads += (download.mVFilmSize.getSize() > 0 ? download.mVFilmSize.getSize() : 0);
            if (download.start != null && download.start.status == Start.STATUS_RUN) {
                // die Downlaods laufen gerade
                bandwidth += download.start.bandbreite; // bytes per second
                byteAktDownloads += (download.mVFilmSize.getAktSize() > 0 ? download.mVFilmSize.getAktSize() : 0);
                if (download.start.restSekunden > timeRestAktDownloads) {
                    // der längeste gibt die aktuelle Restzeit vor
                    timeRestAktDownloads = download.start.restSekunden;
                }
            }
        }
        aktivDownloads.clear();

        if (bandwidth < 0) {
            bandwidth = 0;
        }

        if (bandwidth > 0) {
            // sonst macht die Restzeit keinen Sinn
            final long b = byteAlleDownloads - byteAktDownloads;
            if (b <= 0) {
                timeRestAllDownloads = 0;
            } else {
                timeRestAllDownloads = b / bandwidth;
            }
            if (timeRestAllDownloads < timeRestAktDownloads) {
                timeRestAllDownloads = timeRestAktDownloads; // falsch geraten oder es gibt nur einen
            }

            if (anzDownloadsRun == 1) {
                timeRestAllDownloads = 0; // gibt ja nur noch einen
            }
        }

        formatBandwidthString();

        //TODO put status values in Info Event message
        MessageBus.getMessageBus().publishAsync(new DownloadInfoUpdateAvailableEvent());
    }

    private void resetData() {
        anzDownloadsRun = 0;
        byteAlleDownloads = 0;
        byteAktDownloads = 0;
        timeRestAktDownloads = 0;
        timeRestAllDownloads = 0;
        bandwidth = 0;
    }

}
