/*
 * MediathekView
 * Copyright (C) 2008 W. Xaver
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
package mediathek.controller.io.starter;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import mediathek.Log;
import mediathek.daten.DatenDownload;

class RuntimeExec {

    private static final int INPUT = 1;
    private static final int ERROR = 2;
    private String prog;
    Thread clearIn;
    Thread clearOut;
    private Process process = null;
    Starts s;
    private Pattern pattern = Pattern.compile("([0-9.]*%)");

    /**
     * Neue Klasse instanzieren
     */
    public RuntimeExec(Starts st) {
        s = st;
        prog = s.datenDownload.arr[DatenDownload.DOWNLOAD_PROGRAMM_AUFRUF_NR];
    }

    /**
     * Neue Klasse instanzieren
     */
    public RuntimeExec(String p) {
        prog = p;
    }

    //===================================
    // Public
    //===================================
    /**
     * Download starten
     */
    public Process exec() {
        try {
            Log.systemMeldung("----------------------------------------------------------------------------");
            Log.systemMeldung("| Programm starten");
            Log.systemMeldung("| Programmset: " + s.datenDownload.arr[DatenDownload.DOWNLOAD_PROGRAMMSET_NR]);
            Log.systemMeldung("| " + prog);
            Log.systemMeldung("----------------------------------------------------------------------------");
            process = Runtime.getRuntime().exec(prog);
            clearIn = new Thread(new ClearInOut(INPUT, process));
            clearOut = new Thread(new ClearInOut(ERROR, process));
            clearIn.start();
            clearOut.start();
        } catch (Exception ex) {
            //bescheid geben
            if (process == null) {
            }
            Log.fehlerMeldung(450028932, "RuntimeExec.exec", ex, "Fehler beim Starten");
        }
        return process;
    }

    //===================================
    // Private
    //===================================
    private class ClearInOut implements Runnable {

        private int art;
        private BufferedReader buff;
        private InputStream in;
        private Process process;
        int percent = 0;

        public ClearInOut(int a, Process p) {
            art = a;
            process = p;
        }

        @Override
        public void run() {
            String titel = "";
            try {
                switch (art) {
                    case INPUT:
                        in = process.getInputStream();
                        titel = "INPUTSTREAM";
                        break;
                    case ERROR:
                        in = process.getErrorStream();
                        titel = "ERRORSTREAM";
                        break;
                }
                buff = new BufferedReader(new InputStreamReader(in));
                String inStr;
                while ((inStr = buff.readLine()) != null) {
                    GetPercentageFromErrorStream(inStr);
                    Log.playerMeldung(titel + ": " + inStr);
                }
            } catch (IOException ex) {
            } finally {
                try {
                    buff.close();
                } catch (IOException ex) {
                }
            }
        }

        private void GetPercentageFromErrorStream(String input) {
            // by: siedlerchr
            Matcher matcher = pattern.matcher(input);
            if (matcher.find()) {
                String percentage = matcher.group();
                percentage = percentage.substring(0, percentage.length() - 1);
                try {
                    // nur ganze Int speichern, damit nur 100 Schritte
                    Double d = Double.valueOf(percentage);
                    int pNeu;
                    if (d > 0 && d <= 2) {
                        // damit der Progressbar gleich startet
                        pNeu = 2;
                    } else {
                        pNeu = d.intValue();
                    }
                    if (pNeu != percent) {
                        percent = pNeu;
                        s.datenDownload.startMelden(percent);
                    }
                } catch (Exception ex) {
                    s.datenDownload.startMelden(1);
                    Log.fehlerMeldung(912036780, "RuntimeExec.GetPercentageFromErrorStream", input);
                }
            }
        }
    }
}
