/*
 * Copyright (c) 2008-2026 derreisende77.
 * This code was developed as part of the MediathekView project https://github.com/mediathekview/MediathekView
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package mediathek.controller.starter;

import mediathek.config.Config;
import mediathek.tool.MVFilmSize;
import mediathek.tool.ProcessCommandUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.time.Duration;
import java.time.LocalDateTime;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * Responsible for the interaction with ffmpeg/avconv.
 */
public class RuntimeExec {
    public static final String TRENNER_PROG_ARRAY = "<>";
    private static final Pattern PATTERN_FFMPEG = Pattern.compile("(?<=  Duration: )[^,]*"); // Duration: 00:00:30.28, start: 0.000000, bitrate: N/A
    private static final Pattern PATTERN_TIME = Pattern.compile("(?<=time=)[^ ]*"); // frame=  147 fps= 17 q=-1.0 size=    1588kB time=00:00:05.84 bitrate=2226.0kbits/s
    private static final Pattern PATTERN_SIZE = Pattern.compile("(?<=size=)[^k]*"); // frame=  147 fps= 17 q=-1.0 size=    1588kB time=00:00:05.84 bitrate=2226.0kbits/s
    private static final Logger logger = LogManager.getLogger();
    private final String strProgCall;
    private Start start;
    private double totalSecs;
    private long oldSize;
    private long oldSecs;
    private MVFilmSize mVFilmSize;
    private String[] arrProgCallArray;
    private String strProgCallArray = "";

    public RuntimeExec(MVFilmSize mVFilmSize, Start start,
                       String strProgCall, String strProgCallArray) {
        this.mVFilmSize = mVFilmSize;
        this.start = start;
        this.strProgCall = strProgCall;
        this.arrProgCallArray = strProgCallArray.split(TRENNER_PROG_ARRAY);
        this.strProgCallArray = strProgCallArray;
        if (arrProgCallArray.length <= 1) {
            arrProgCallArray = null;
        }
    }

    public RuntimeExec(String p) {
        strProgCall = p;
    }

    private void logOutput(String s, boolean isArray) {
        logger.info("=====================");
        if (isArray) {
            logger.info("Starte Array: ");
        }
        else {
            logger.info("Starte nicht als Array:");
        }
        logger.info(" -> {}", s);
        logger.info("=====================");
    }

    public Process exec(boolean log) {
        Process process = null;

        try {
            if (arrProgCallArray != null) {
                if (log) {
                    logOutput(strProgCallArray, true);
                }
                process = new ProcessBuilder(arrProgCallArray).start();
            }
            else {
                if (log) {
                    logOutput(strProgCall, false);
                }
                process = new ProcessBuilder(ProcessCommandUtils.tokenizeCommand(strProgCall)).start();
            }

            ClearInOut clearIn = new ClearInOut(IoType.INPUT, process);
            ClearInOut clearOut = new ClearInOut(IoType.ERROR, process);

            clearIn.start();
            clearOut.start();
        }
        catch (Exception ex) {
            logger.error("Fehler beim Starten", ex);
        }
        return process;
    }

    private enum IoType {INPUT, ERROR}

    private class ClearInOut extends Thread {
        private final IoType art;
        private final Process process;
        private int percent;
        private int percent_start = -1;

        public ClearInOut(IoType art, Process process) {
            this.art = art;
            this.process = process;
            setName(String.format("ClearInOut type %s for pid %d", art.toString(), process.pid()));
        }

        @Override
        public void run() {
            final String titel;
            final InputStream in;
            if (art == IoType.INPUT) {
                in = process.getInputStream();
                titel = "INPUTSTREAM";
            }
            else {
                in = process.getErrorStream();
                titel = String.format("ERRORSTREAM [%d]", process.pid());
            }

            try (in;
                 var isr = new InputStreamReader(in);
                 var buff = new BufferedReader(isr)) {
                String inStr;
                while ((inStr = buff.readLine()) != null) {
                    GetPercentageFromErrorStream(inStr);
                    // only print stream info when enhanced log mode enabled
                    if (Config.isEnhancedLoggingEnabled()) {
                        logger.trace("  >> {}}: {}}", titel, inStr);
                    }
                }
            }
            catch (IOException ex) {
                logger.error("ClearInOut.run() error occured", ex);
            }
        }

        private void GetPercentageFromErrorStream(String input) {
            Matcher matcher;

            // für ffmpeg
            // ffmpeg muss dazu mit dem Parameter -i gestartet werden:
            // -i %f -acodec copy -vcodec copy -y **
            try {
                // Gesamtzeit
                matcher = PATTERN_FFMPEG.matcher(input);
                if (matcher.find()) {
                    // Find duration
                    String dauer = matcher.group().trim();
                    String[] hms = dauer.split(":");
                    totalSecs = Integer.parseInt(hms[0]) * 3600
                            + Integer.parseInt(hms[1]) * 60
                            + Double.parseDouble(hms[2]);
                }
                // Bandbreite
                matcher = PATTERN_SIZE.matcher(input);
                if (matcher.find()) {
                    String s = matcher.group().trim();
                    if (!s.isEmpty()) {
                        try {

                            final long aktSize = Integer.parseInt(s.replace("kB", ""));
                            mVFilmSize.setAktSize(aktSize * 1_000);
                            final var akt = Duration.between(start.startTime, LocalDateTime.now()).toSeconds();
                            if (oldSecs < akt - 5) {
                                start.bandbreite = (aktSize - oldSize) * 1_000 / (akt - oldSecs);
                                oldSecs = akt;
                                oldSize = aktSize;
                            }
                        }
                        catch (NumberFormatException ignored) {
                        }
                    }
                }
                // Fortschritt
                matcher = PATTERN_TIME.matcher(input);
                if (totalSecs > 0 && matcher.find()) {
                    // ffmpeg    1611kB time=00:00:06.73 bitrate=1959.7kbits/s   
                    // avconv    size=   26182kB time=100.96 bitrate=2124.5kbits/s 
                    String zeit = matcher.group();
                    if (zeit.contains(":")) {
                        String[] hms = zeit.split(":");
                        final double aktSecs = Integer.parseInt(hms[0]) * 3600
                                + Integer.parseInt(hms[1]) * 60
                                + Double.parseDouble(hms[2]);
                        double d = aktSecs / totalSecs * 100;
                        meldenDouble(d);
                    }
                    else {
                        double aktSecs = Double.parseDouble(zeit);
                        double d = aktSecs / totalSecs * 100;
                        meldenDouble(d);
                    }
                }
            }
            catch (Exception ex) {
                DownloadProgressEventPublisher.publishThrottled();
                logger.error("GetPercentageFromErrorStream(): {}", input);
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
                    final var diffZeit = Duration.between(start.startTime, LocalDateTime.now()).toSeconds();
                    int diffProzent = percent - percent_start;
                    int restProzent = 1000 - percent;
                    start.restSekunden = (diffZeit * restProzent / diffProzent);
                }
                DownloadProgressEventPublisher.publishThrottled();
            }
        }
    }
}
