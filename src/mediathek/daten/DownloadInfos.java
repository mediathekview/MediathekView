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

    public void clean() {
        anzDownloadsRun = 0;
        byteAlleDownloads = 0;
        byteAktDownloads = 0;
        timeRestAktDownloads = 0;
        timeRestAllDownloads = 0;
        bandwidth = 0;
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

}
