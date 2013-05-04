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
import java.util.ArrayList;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import mediathek.daten.DatenDownload;
import mediathek.tool.Log;

public class RuntimeExec {

    private static final int INPUT = 1;
    private static final int ERROR = 2;
    private String prog;
    Thread clearIn;
    Thread clearOut;
    private Process process = null;
    Start s;
    private static int procnr = 0; //TH
    private Pattern patternFlvstreamer = Pattern.compile("([0-9.]*%)");
    private Pattern patternFfmpeg = Pattern.compile("(?<=Duration: )[^,]*");
    private Pattern patternZeit = Pattern.compile("(?<=time=)[\\d.]+");
    private double totalSecs = 0;
    private String zeit, prozent;

    public RuntimeExec(Start st) {
        s = st;
        prog = s.datenDownload.arr[DatenDownload.DOWNLOAD_PROGRAMM_AUFRUF_NR];
    }

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
////            String p1 = prog.substring(0, prog.indexOf(" "));
////            String p2 = prog.substring(prog.indexOf(" ") + 1);

////            String[] arr = prog.split(" ");
////            arr = split(prog);
////            process = new ProcessBuilder(arr).start();

            process = Runtime.getRuntime().exec(prog);
            clearIn = new Thread(new ClearInOut(INPUT, process));
            clearOut = new Thread(new ClearInOut(ERROR, process));
            clearIn.start();
            clearOut.start();
        } catch (Exception ex) {
            //bescheid geben
            if (process == null) {
            }
            Log.fehlerMeldung(450028932, Log.FEHLER_ART_PROG, "RuntimeExec.exec", ex, "Fehler beim Starten");
        }
        return process;
    }

    //===================================
    // Private
    //===================================
    
////        private String[] split(String s) {
////            ArrayList<String> list = new ArrayList<String>();
////            String p = "";
////            boolean STUECK = false;
////            for (int i = 0; i < s.length(); ++i) {
////                String ss = s.substring(i, i + 1);
////                if (STUECK) {
////                    if (ss.equals("\"")) {
////                        STUECK = false;
////                        list.add(p);
////                        p = "";
////                    } else {
////                        p += ss;
////                    }
////                } else {
////                    if (!ss.equals(" ")) {
////                        if (ss.equals("\"")) {
////                            STUECK = true;
////                            continue;
////                        }
////                        p += ss;
////                    } else {
////                        list.add(p);
////                        p = "";
////                    }
////                }
////            }
////            if (!p.equals("")) {
////                list.add(p);
////            }
////            return list.toArray(new String[]{});
////        }
    private class ClearInOut implements Runnable {

        private int art;
        private BufferedReader buff;
        private InputStream in;
        private Process process;
        private int percent = 0;
        private int percent_start = -1;

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
                        //TH
                        synchronized (this) {
                            titel = "ERRORSTREAM [" + (++procnr) + "]";
                        }
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
            // für den flvstreamer und rtmpdump
            Matcher matcher = patternFlvstreamer.matcher(input);
            if (matcher.find()) {
                try {
                    prozent = matcher.group();
                    prozent = prozent.substring(0, prozent.length() - 1);
                    double d = Double.parseDouble(prozent);
                    meldenDouble(d);
                } catch (Exception ex) {
                    s.datenDownload.statusMelden(DatenDownload.PROGRESS_GESTARTET);
                    Log.fehlerMeldung(912036780, Log.FEHLER_ART_PROG, "RuntimeExec.GetPercentageFromErrorStream-1", input);
                }
            } else {
                // für ffmpeg
                // ffmpeg muss dazu mit dem Parameter -i gestartet werden:
                // -i %f -acodec copy -vcodec copy -y **
                try {
                    matcher = patternFfmpeg.matcher(input);
                    if (matcher.find()) {
                        // Find duration
                        String dauer = matcher.group();
                        String[] hms = dauer.split(":");
                        totalSecs = Integer.parseInt(hms[0]) * 3600
                                + Integer.parseInt(hms[1]) * 60
                                + Double.parseDouble(hms[2]);
                    }
                    matcher = patternZeit.matcher(input);
                    if (totalSecs > 0 && matcher.find()) {
                        zeit = matcher.group();
                        double d = Double.parseDouble(zeit) / totalSecs * 100;
                        meldenDouble(d);
                    }
                } catch (Exception ex) {
                    s.datenDownload.statusMelden(DatenDownload.PROGRESS_GESTARTET);
                    Log.fehlerMeldung(912036780, Log.FEHLER_ART_PROG, "RuntimeExec.GetPercentageFromErrorStream-2", input);
                }
            }
        }

        private void meldenDouble(double d) {
            // nur ganze Int speichern, und 1000 Schritte
            d *= 10;
            int pNeu = (int) d;
            if (pNeu != percent) {
                percent = pNeu;
                if (percent_start == -1) {
                    // für wiedergestartete Downloads
                    percent_start = percent;
                }
                if (percent > (percent_start + 5)) {
                    // sonst macht es noch keinen Sinn
                    int diffZeit = s.startZeit.diffInSekunden();
                    int diffProzent = percent - percent_start;
                    int restProzent = 1000 - percent;
                    s.restSekunden = (diffZeit * restProzent / diffProzent);
                }
                s.datenDownload.statusMelden(percent);
            }
        }
    }
}
