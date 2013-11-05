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

import mediathek.tool.Datum;
import mediathek.tool.MVInputStream;

public class Start {

    public static final int PROGRESS_NICHT_GESTARTET = -1;
    public static final int PROGRESS_WARTEN = 0;
    public static final int PROGRESS_GESTARTET = 1;
    public static final int PROGRESS_FERTIG = 1000;
    public int status = STATUS_INIT;
    public int startcounter = 0;
    public Process process = null; //Prozess des Download
    public int percent = -1; // Prozent fertiggestellt: -1=nix, 999=99,9%
    public long bandbreite = -1; // Downloadbandbreite
    public boolean stoppen = false;
    public boolean beginnAnschauen = false;
    public Datum startZeit = null;
    public long restSekunden = -1;
    public MVInputStream mVInputStream = null;
    // Quelle - start über einen Button - Download - Abo
    public static final int QUELLE_ALLE = -1;
    public static final int QUELLE_BUTTON = 1;
    public static final int QUELLE_DOWNLOAD = 2;
    public static final int QUELLE_ABO = 3;
    public static final String QUELLE_ALLE_TXT = "Alle";
    public static final String QUELLE_BUTTON_TXT = "Button";
    public static final String QUELLE_DOWNLOAD_TXT = "Download";
    public static final String QUELLE_ABO_TXT = "Abo";
    public static final int ART_DOWNLOAD = 1; // direkter Download
    public static final int ART_PROGRAMM = 2; // Download über ein Programm
    public static final String ART_DOWNLOAD_TXT = "direkter Download";
    public static final String ART_PROGRAMM_TXT = "Programm";
    // Stati
    public static final int STATUS_INIT = 1;
    public static final int STATUS_RUN = 2;
    public static final int STATUS_FERTIG = 3;
    public static final int STATUS_ERR = 4;
    //Download wird so oft gestartet, falls er beim ersten Mal nicht anspringt
    public static final int STARTCOUNTER_MAX = 3;

    public Start() {
    }

    public static String getTextProgress(Start s) {
        String ret = "";
        if (s == null) {
            return "";
        }
        if (s.percent == PROGRESS_NICHT_GESTARTET) {
            // noch nicht gestartet
        } else if (s.percent == PROGRESS_WARTEN) {
            ret = "warten";
        } else if (s.percent == PROGRESS_GESTARTET) {
            ret = "gestartet";
        } else if (1 < s.percent && s.percent < PROGRESS_FERTIG) {
            double d = s.percent / 10.0;
            ret = Double.toString(d) + "%";
        } else if (s.percent == PROGRESS_FERTIG) {
            if (s.status == Start.STATUS_ERR) {
                ret = "fehlerhaft";
            } else {
                ret = "fertig";
            }
        }
        return ret;
    }
}
