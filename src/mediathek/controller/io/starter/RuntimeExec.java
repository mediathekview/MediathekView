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
import javax.swing.JOptionPane;
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
        } catch (IOException ex) {
            //bescheid geben
            if (process == null) {
            }
            Log.fehlerMeldung("RuntimeExec.exec", ex, "Fehler beim Starten");
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
    }
}
