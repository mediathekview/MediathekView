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

import mediathek.config.Daten;
import java.text.DecimalFormat;
import java.util.LinkedList;
import mediathek.controller.starter.Start;

public class DownloadInfos {

    // Anzahl
    public int anzDownloadsRun = 0; //Anzahl gestarteter Downloads
    // Größe
    public long byteAlleDownloads = 0; //anz. Bytes für alle gestarteten Downloads
    public long byteAktDownloads = 0; //anz. Bytes bereits geladen für die gerade ladenden/laufenden Downloads
    // Zeit
    public long timeRestAktDownloads = 0; //Restzeit für die gerade ladenden/laufenden Downloads
    public long timeRestAllDownloads = 0; // Restzeit aller gestarteten Downloads
    // Bandbreite
    public long bandwidth = 0; //Bandbreite: bytes per second
    public String bandwidthStr = "";

    // Anzahl, Anz-Abo, Anz-Down, nicht gestarted, laufen, fertig OK, fertig fehler
    public int[] downloadStarts = new int[]{0, 0, 0, 0, 0, 0, 0};

    private LinkedList<DatenDownload> aktivDownloads; // Liste gestarteter Downloads

    public String roundBandwidth(long time) {
        roundBandwidth();
        if (bandwidth > 1_000_000.0) {
            return time / 60 + ":" + (time % 60 < 10 ? "0" + time % 60 : time % 60) + " Minuten / " + bandwidthStr;
        } else if (bandwidth > 1_000.0) {
            return time / 60 + ":" + (time % 60 < 10 ? "0" + time % 60 : time % 60) + " Minuten / " + bandwidthStr;
        } else {
            return time / 60 + ":" + (time % 60 < 10 ? "0" + time % 60 : time % 60) + " Minuten / " + bandwidthStr;
        }
    }

    public String roundBandwidth() {
        if (bandwidth > 1_000_000.0) {
            bandwidthStr = new DecimalFormat("####0.00").format(bandwidth / 1_000_000.0) + " MByte/s";
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
                return Long.toString(timeRestAllDownloads / 60) + " Min";
            }
        }
        return "";
    }

    public String getRestzeit() {
        if (timeRestAktDownloads > 0) {
            if (timeRestAktDownloads < 60) {
                return "< 1 Min";
            } else {
                return Long.toString(timeRestAktDownloads / 60) + " Min";
            }
        }
        return "";
    }

    public synchronized void makeDownloadInfos() {
        clean();

        downloadStarts = Daten.listeDownloads.getStarts();

        aktivDownloads = Daten.listeDownloads.getListOfStartsNotFinished(DatenDownload.QUELLE_ALLE);
        for (DatenDownload download : aktivDownloads) {
            ++Daten.downloadInfos.anzDownloadsRun;
            Daten.downloadInfos.byteAlleDownloads += (download.mVFilmSize.getSize() > 0 ? download.mVFilmSize.getSize() : 0);
            if (download.start != null && download.start.status == Start.STATUS_RUN) {
                // die Downlaods laufen gerade
                Daten.downloadInfos.bandwidth += download.start.bandbreite; // bytes per second
                Daten.downloadInfos.byteAktDownloads += (download.mVFilmSize.getAktSize() > 0 ? download.mVFilmSize.getAktSize() : 0);
                if (download.start.restSekunden > Daten.downloadInfos.timeRestAktDownloads) {
                    // der längeste gibt die aktuelle Restzeit vor
                    Daten.downloadInfos.timeRestAktDownloads = download.start.restSekunden;
                }
            }
        }

        if (Daten.downloadInfos.bandwidth < 0) {
            Daten.downloadInfos.bandwidth = 0;
        }

        if (Daten.downloadInfos.bandwidth > 0) {
            // sonst macht die Restzeit keinen Sinn
            final long b = Daten.downloadInfos.byteAlleDownloads - Daten.downloadInfos.byteAktDownloads;
            if (b <= 0) {
                Daten.downloadInfos.timeRestAllDownloads = 0;
            } else {
                Daten.downloadInfos.timeRestAllDownloads = b / Daten.downloadInfos.bandwidth;
            }
            if (Daten.downloadInfos.timeRestAllDownloads < Daten.downloadInfos.timeRestAktDownloads) {
                Daten.downloadInfos.timeRestAllDownloads = Daten.downloadInfos.timeRestAktDownloads; // falsch geraten oder es gibt nur einen
            }
            if (Daten.downloadInfos.anzDownloadsRun == 1) {
                Daten.downloadInfos.timeRestAllDownloads = 0; // gibt ja nur noch einen
            }
        }
        Daten.downloadInfos.roundBandwidth();
    }

    private void clean() {
        anzDownloadsRun = 0;
        byteAlleDownloads = 0;
        byteAktDownloads = 0;
        timeRestAktDownloads = 0;
        timeRestAllDownloads = 0;
        bandwidth = 0;
    }

}
