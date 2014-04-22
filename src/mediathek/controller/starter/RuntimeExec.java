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
package mediathek.controller.starter;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import mediathek.controller.Log;
import mediathek.daten.DatenDownload;
import mediathek.tool.ListenerMediathekView;

public class RuntimeExec {

    private static final int INPUT = 1;
    private static final int ERROR = 2;
    private String prog;
    Thread clearIn;
    Thread clearOut;
    private Process process = null;
    Start start;
    private static int procnr = 0; //TH
    //private Pattern patternFlvstreamer = Pattern.compile("([0-9.]*%)");
    private Pattern patternFlvstreamer = Pattern.compile("([0-9]*.[0-9]{1}%)");
    //private Pattern patternFfmpeg = Pattern.compile("(?<=Duration: )[^,]*"); // Duration: 00:00:30.28, start: 0.000000, bitrate: N/A
    private Pattern patternFfmpeg = Pattern.compile("(?<=  Duration: )[^,]*"); // Duration: 00:00:30.28, start: 0.000000, bitrate: N/A
    //private Pattern patternZeit = Pattern.compile("(?<=time=)[\\d:.]+"); //    1611kB time=00:00:06.73 bitrate=1959.7kbits/s   
    private Pattern patternZeit = Pattern.compile("(?<=time=)[^ ]*"); //    1611kB time=00:00:06.73 bitrate=1959.7kbits/s   
    private double totalSecs = 0;

    public RuntimeExec(DatenDownload d) {
        start = d.start;
        prog = d.arr[DatenDownload.DOWNLOAD_PROGRAMM_AUFRUF_NR];
    }

    public RuntimeExec(String p) {
        prog = p;
    }

    //===================================
    // Public
    //===================================
    public Process exec() {
        try {
            //process = new ProcessBuilder(prog).start();
            process = Runtime.getRuntime().exec(prog);
            clearIn = new Thread(new ClearInOut(INPUT, process));
            clearOut = new Thread(new ClearInOut(ERROR, process));
            clearIn.start();
            clearOut.start();
        } catch (Exception ex) {
            Log.fehlerMeldung(450028932, Log.FEHLER_ART_PROG, "RuntimeExec.exec", ex, "Fehler beim Starten");
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
            } catch (IOException ignored) {
            } finally {
                try {
                    buff.close();
                } catch (IOException ignored) {
                }
            }
        }

        private void GetPercentageFromErrorStream(String input) {
            // by: siedlerchr
            // für den flvstreamer und rtmpdump
            Matcher matcher = patternFlvstreamer.matcher(input);
            if (matcher.find()) {
                try {
                    String prozent = matcher.group();
                    prozent = prozent.substring(0, prozent.length() - 1);
                    double d = Double.parseDouble(prozent);
                    meldenDouble(d);
                } catch (Exception ex) {
                    ListenerMediathekView.notify(ListenerMediathekView.EREIGNIS_ART_DOWNLOAD_PROZENT, RuntimeExec.class.getName());
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
                        String dauer = matcher.group().trim();
                        String[] hms = dauer.split(":");
                        totalSecs = Integer.parseInt(hms[0]) * 3600
                                + Integer.parseInt(hms[1]) * 60
                                + Double.parseDouble(hms[2]);
                    }
                    matcher = patternZeit.matcher(input);
                    if (totalSecs > 0 && matcher.find()) {
                        String zeit = matcher.group();
                        String[] hms = zeit.split(":");
                        double aktSecs = Integer.parseInt(hms[0]) * 3600
                                + Integer.parseInt(hms[1]) * 60
                                + Double.parseDouble(hms[2]);
                        double d = aktSecs / totalSecs * 100;
                        meldenDouble(d);
                    }
                } catch (Exception ex) {
                    ListenerMediathekView.notify(ListenerMediathekView.EREIGNIS_ART_DOWNLOAD_PROZENT, RuntimeExec.class.getName());
                    Log.fehlerMeldung(912036780, Log.FEHLER_ART_PROG, "RuntimeExec.GetPercentageFromErrorStream-2", input);
                }
            }
        }

        private void meldenDouble(double d) {
            // nur ganze Int speichern, und 1000 Schritte
            d *= 10;
            int pNeu = (int) d;
            start.percent = pNeu;
            if (pNeu != percent) {
                percent = pNeu;
                if (percent_start == -1) {
                    // für wiedergestartete Downloads
                    percent_start = percent;
                }
                if (percent > (percent_start + 5)) {
                    // sonst macht es noch keinen Sinn
                    int diffZeit = start.startZeit.diffInSekunden();
                    int diffProzent = percent - percent_start;
                    int restProzent = 1000 - percent;
                    start.restSekunden = (diffZeit * restProzent / diffProzent);
                }
                ListenerMediathekView.notify(ListenerMediathekView.EREIGNIS_ART_DOWNLOAD_PROZENT, RuntimeExec.class.getName());
            }
        }
    }
}
