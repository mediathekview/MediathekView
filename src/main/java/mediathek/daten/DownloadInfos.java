/*
 * MediathekView
 * Copyright (C) 2014 W. Xaver
 * W.Xaver[at]googlemail.com
 * http://zdfmediathk.sourceforge.net/
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package mediathek.daten;

import mSearch.tool.Log;
import mediathek.config.Daten;
import mediathek.config.MVConfig;
import mediathek.controller.starter.Start;
import mediathek.gui.messages.BaseEvent;
import mediathek.gui.messages.DownloadInfoUpdateAvailableEvent;
import mediathek.gui.messages.TimerEvent;
import mediathek.tool.MVFilmSize;
import net.engio.mbassy.bus.MBassador;
import net.engio.mbassy.listener.Handler;

import java.text.DecimalFormat;

public class DownloadInfos {
    private static final DecimalFormat formatter = new DecimalFormat("####0.00");
    private final MBassador<BaseEvent> messageBus;
    /**
     * Bandbreite: bytes per second
     */
    private long bandwidth = 0;
    /**
     * Restzeit aller gestarteten Downloads
     */
    private long timeRestAllDownloads = 0;
    /**
     * Restzeit für die gerade ladenden/laufenden Downloads
     */
    private long timeRestAktDownloads = 0;
    /**
     * Anzahl Bytes bereits geladen für die gerade ladenden/laufenden Downloads
     */
    private long byteAktDownloads = 0;
    /**
     * Anzahl Bytes für alle gestarteten Downloads
     */
    private long byteAlleDownloads = 0;
    /**
     * Anzahl gestarteter Downloads
     */
    private int anzDownloadsRun = 0;
    // Prozent fertig (alle)
    private int percent = -1;
    private String bandwidthStr = "";

    public DownloadInfos(MBassador<BaseEvent> messageBus) {
        this.messageBus = messageBus;
        messageBus.subscribe(this);
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

    public String roundBandwidth() {
        if (bandwidth > 1_000_000.0) {
            bandwidthStr = formatter.format(bandwidth / 1_000_000.0) + " MByte/s";
        } else if (bandwidth > 1_000.0) {
            bandwidthStr = Math.round(bandwidth / 1_000.0) + " kByte/s";
        } else {
            bandwidthStr = Math.round(bandwidth) + " Byte/s";
        }
        return bandwidthStr;
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

        final var listeDownloads = Daten.getInstance().getListeDownloads();
        final var aktivDownloads = listeDownloads.getListOfStartsNotFinished(DatenDownload.QUELLE_ALLE);

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
        if (byteAlleDownloads > 0) {
            percent = (int) (byteAktDownloads * 100 / byteAlleDownloads);
            progressMsg();
        }
        roundBandwidth();

        messageBus.publishAsync(new DownloadInfoUpdateAvailableEvent());
    }

    private void progressMsg() {
        if (!MVConfig.getBool(MVConfig.Configs.SYSTEM_PARAMETER_DOWNLOAD_PROGRESS)) {
            return;
        }
        final int progress = percent;
        if (progress >= 0) {
            StringBuilder text = new StringBuilder("  [ ");
            final int a = progress / 10;
            for (int i = 0; i < a; ++i) {
                text.append("#");
            }
            for (int i = 0; i < (10 - a); ++i) {
                text.append("-");
            }
            text.append(" ]  ").append(MVFilmSize.getGroesse(byteAktDownloads)).append(" von ").append(MVFilmSize.getGroesse(byteAlleDownloads)).append(" MByte /");
            text.append(" Downloads: ").append(anzDownloadsRun).append(" /");
            text.append(" Bandbreite: ").append(roundBandwidth());
            Log.progress(text.toString());
        }
    }

    private void resetData() {
        anzDownloadsRun = 0;
        byteAlleDownloads = 0;
        byteAktDownloads = 0;
        timeRestAktDownloads = 0;
        timeRestAllDownloads = 0;
        bandwidth = 0;
        percent = -1;
    }

}
