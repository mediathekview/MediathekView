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

import java.text.DecimalFormat;
import java.util.LinkedList;
import mSearch.tool.Log;
import mediathek.config.Daten;
import mediathek.config.MVConfig;
import mediathek.controller.starter.Start;
import mediathek.tool.MVFilmSize;

public class DownloadInfos {

    private final Daten daten;
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
    // Prozent fertig (alle)
    public int percent = -1;

    // Anzahl, Anz-Abo, Anz-Down, nicht gestarted, laufen, fertig OK, fertig fehler
    public int[] downloadStarts = new int[]{0, 0, 0, 0, 0, 0, 0};

    private LinkedList<DatenDownload> aktivDownloads; // Liste gestarteter Downloads

    public DownloadInfos(Daten aDaten)
    {
        daten = aDaten;
    }

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

        downloadStarts = daten.getListeDownloads().getStarts();

        aktivDownloads = daten.getListeDownloads().getListOfStartsNotFinished(DatenDownload.QUELLE_ALLE);
        for (DatenDownload download : aktivDownloads) {
            ++daten.getDownloadInfos().anzDownloadsRun;
            daten.getDownloadInfos().byteAlleDownloads += (download.mVFilmSize.getSize() > 0 ? download.mVFilmSize.getSize() : 0);
            if (download.start != null && download.start.status == Start.STATUS_RUN) {
                // die Downlaods laufen gerade
                daten.getDownloadInfos().bandwidth += download.start.bandbreite; // bytes per second
                daten.getDownloadInfos().byteAktDownloads += (download.mVFilmSize.getAktSize() > 0 ? download.mVFilmSize.getAktSize() : 0);
                if (download.start.restSekunden > daten.getDownloadInfos().timeRestAktDownloads) {
                    // der längeste gibt die aktuelle Restzeit vor
                    daten.getDownloadInfos().timeRestAktDownloads = download.start.restSekunden;
                }
            }
        }

        if (daten.getDownloadInfos().bandwidth < 0) {
            daten.getDownloadInfos().bandwidth = 0;
        }

        if (daten.getDownloadInfos().bandwidth > 0) {
            // sonst macht die Restzeit keinen Sinn
            final long b = daten.getDownloadInfos().byteAlleDownloads - daten.getDownloadInfos().byteAktDownloads;
            if (b <= 0) {
                daten.getDownloadInfos().timeRestAllDownloads = 0;
            } else {
                daten.getDownloadInfos().timeRestAllDownloads = b / daten.getDownloadInfos().bandwidth;
            }
            if (daten.getDownloadInfos().timeRestAllDownloads < daten.getDownloadInfos().timeRestAktDownloads) {
                daten.getDownloadInfos().timeRestAllDownloads = daten.getDownloadInfos().timeRestAktDownloads; // falsch geraten oder es gibt nur einen
            }
            if (daten.getDownloadInfos().anzDownloadsRun == 1) {
                daten.getDownloadInfos().timeRestAllDownloads = 0; // gibt ja nur noch einen
            }
        }
        if (byteAlleDownloads > 0) {
            percent = (int) (byteAktDownloads * 100 / byteAlleDownloads);
            progressMsg();
        }
        roundBandwidth();
    }

    private void progressMsg() {
        if (!MVConfig.getBool(MVConfig.Configs.SYSTEM_PARAMETER_DOWNLOAD_PROGRESS)) {
            return;
        }
        int progress = daten.getDownloadInfos().percent;
        if (progress >= 0) {
            String text = "  [ ";
            int a = progress / 10;
            for (int i = 0; i < a; ++i) {
                text += "#";
            }
            for (int i = 0; i < (10 - a); ++i) {
                text += "-";
            }
            text += " ]  " + MVFilmSize.getGroesse(byteAktDownloads) + " von " + MVFilmSize.getGroesse(byteAlleDownloads) + " MByte /";
            text += " Downloads: " + daten.getDownloadInfos().anzDownloadsRun + " /";
            text += " Bandbreite: " + roundBandwidth();
            Log.progress(text);
        }
    }

    private void clean() {
        anzDownloadsRun = 0;
        byteAlleDownloads = 0;
        byteAktDownloads = 0;
        timeRestAktDownloads = 0;
        timeRestAllDownloads = 0;
        bandwidth = 0;
        percent = -1;
    }

}
