/*    
 *    MediathekView
 *    Copyright (C) 2008   W. Xaver
 *    W.Xaver[at]googlemail.com
 *    http://zdfmediathk.sourceforge.net/
 *    
 *    This program is free software: you can redistribute it and/or modify
 *    it under the terms of the GNU General Public License as published by
 *    the Free Software Foundation, either version 3 of the License, or
 *    any later version.
 *
 *    This program is distributed in the hope that it will be useful,
 *    but WITHOUT ANY WARRANTY; without even the implied warranty of
 *    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *    GNU General Public License for more details.
 *
 *    You should have received a copy of the GNU General Public License
 *    along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */
package mediathek.controller.starter;

import mSearch.tool.Datum;
import mediathek.controller.MVBandwidthCountingInputStream;

public class Start {

    public static final int PROGRESS_NICHT_GESTARTET = -1;
    public static final int PROGRESS_WARTEN = 0;
    public static final int PROGRESS_GESTARTET = 1;
    public static final int PROGRESS_FERTIG = 1000;
    public byte status = STATUS_INIT;
    public int startcounter = 0;
    public Process process = null; //Prozess des Download
    public int percent = -1; // Prozent fertiggestellt: -1=nix, 999=99,9%
    public long bandbreite = -1; // Downloadbandbreite: bytes per second
    public boolean stoppen = false;
    public boolean beginnAnschauen = false;
    public int countRestarted = 0;

    public Datum startZeit = null;
    public long restSekunden = -1;
    public MVBandwidthCountingInputStream mVBandwidthCountingInputStream = null;
    // Stati
    public static final byte STATUS_INIT = 1;
    public static final byte STATUS_RUN = 2;
    public static final byte STATUS_FERTIG = 3;
    public static final byte STATUS_ERR = 4;
    //Download wird so oft gestartet, falls er beim ersten Mal nicht anspringt
    public static final int STARTCOUNTER_MAX = 3;

    public Start() {
    }

    public static String getTextProgress(boolean dManager, Start s) {
        String ret = "";
//        boolean dManager = download.isDownloadManager();
//        Start s = download.start;
        if (s == null) {
            return ret;
        }

        switch (s.percent) {
            case PROGRESS_NICHT_GESTARTET:
                break;

            case PROGRESS_WARTEN:
                ret = dManager ? "extern" : "warten";
                break;

            case PROGRESS_GESTARTET:
                ret = dManager ? "extern:gesendet" : "gestartet";
                break;

            case PROGRESS_FERTIG:
                if (s.status == Start.STATUS_ERR) {
                    ret = dManager ? "extern:fehler" : "fehlerhaft";
                } else {
                    ret = dManager ? "extern:fertig" : "fertig";
                }
                break;

            default:
                if (dManager) {
                    ret = "extern";
                } else if (1 < s.percent && s.percent < PROGRESS_FERTIG) {
                    double d = s.percent / 10.0;
                    ret = Double.toString(d) + '%';
                }
                break;
        }

        return ret;
    }
}
