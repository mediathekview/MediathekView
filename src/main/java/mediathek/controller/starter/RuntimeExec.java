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

import com.google.common.cache.CacheBuilder;
import com.google.common.cache.CacheLoader;
import com.google.common.cache.LoadingCache;
import mediathek.config.Config;
import mediathek.gui.messages.DownloadProgressChangedEvent;
import mediathek.tool.MVFilmSize;
import mediathek.tool.MessageBus;
import org.apache.commons.lang3.StringUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.jetbrains.annotations.NotNull;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.time.Duration;
import java.time.LocalDateTime;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * Responsible for the interaction with ffmpeg/avconv.
 */
public class RuntimeExec {
    public static final String TRENNER_PROG_ARRAY = "<>";
    /**
     * The cache for compiled RegExp.
     */
    private static final LoadingCache<String, Pattern> CACHE = CacheBuilder.newBuilder()
            .expireAfterAccess(5, TimeUnit.MINUTES)
            .build(new PatternCacheLoader());
    private static final String PATTERN_FFMPEG = "(?<=  Duration: )[^,]*"; // Duration: 00:00:30.28, start: 0.000000, bitrate: N/A
    private static final String PATTERN_TIME = "(?<=time=)[^ ]*"; // frame=  147 fps= 17 q=-1.0 size=    1588kB time=00:00:05.84 bitrate=2226.0kbits/s
    private static final String PATTERN_SIZE = "(?<=size=)[^k]*"; // frame=  147 fps= 17 q=-1.0 size=    1588kB time=00:00:05.84 bitrate=2226.0kbits/s
    private static final Logger logger = LogManager.getLogger();
    private static final AtomicInteger processNr = new AtomicInteger(0);
    private final String strProgCall;
    private Process process;
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

    public Process exec(boolean log) {
        try {
            if (arrProgCallArray != null) {
                if (log) {
                    logger.info("=====================");
                    logger.info("Starte Array: ");
                    logger.info(" -> " + strProgCallArray);
                    logger.info("=====================");
                }
                process = Runtime.getRuntime().exec(arrProgCallArray);
            } else {
                if (log) {
                    logger.info("=====================");
                    logger.info("Starte nicht als Array:");
                    logger.info(" -> " + strProgCall);
                    logger.info("=====================");
                }
                process = Runtime.getRuntime().exec(strProgCall);
            }

            Thread clearIn = new Thread(new ClearInOut(IoType.INPUT, process));
            Thread clearOut = new Thread(new ClearInOut(IoType.ERROR, process));
            clearIn.setName("ClearIn: " + clearIn.getId());
            clearOut.setName("ClearOut: " + clearOut.getId());
            clearIn.start();
            clearOut.start();
        } catch (Exception ex) {
            logger.error("Fehler beim Starten", ex);
        }
        return process;
    }

    private enum IoType {INPUT, ERROR}

    /**
     * This loader will compile regexp patterns when they are not in cache.
     */
    static class PatternCacheLoader extends CacheLoader<String, Pattern> {

        @Override
        public Pattern load(@NotNull String pattern) throws IllegalArgumentException {
            logger.trace("COMPILING RuntimeExec PATTERN: " + pattern);
            return Pattern.compile(pattern);
        }
    }

    private class ClearInOut implements Runnable {
        private final IoType art;
        private final Process process;
        private int percent;
        private int percent_start = -1;

        public ClearInOut(IoType art, Process process) {
            this.art = art;
            this.process = process;
        }

        @Override
        public void run() {
            final String titel;
            final InputStream in;
            if (art == IoType.INPUT) {
                in = process.getInputStream();
                titel = "INPUTSTREAM";
            } else {
                in = process.getErrorStream();
                titel = String.format("ERRORSTREAM [%d]", processNr.incrementAndGet());
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
            } catch (IOException ex) {
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
                matcher = CACHE.get(PATTERN_FFMPEG).matcher(input);
                if (matcher.find()) {
                    // Find duration
                    String dauer = matcher.group().trim();
                    String[] hms = dauer.split(":");
                    totalSecs = Integer.parseInt(hms[0]) * 3600
                            + Integer.parseInt(hms[1]) * 60
                            + Double.parseDouble(hms[2]);
                }
                // Bandbreite
                matcher = CACHE.get(PATTERN_SIZE).matcher(input);
                if (matcher.find()) {
                    String s = matcher.group().trim();
                    if (!s.isEmpty()) {
                        try {
                            final long aktSize = Integer.parseInt(StringUtils.replace(s, "kB", ""));
                            mVFilmSize.setAktSize(aktSize * 1_000);
                            final var akt = Duration.between(start.startTime, LocalDateTime.now()).toSeconds();
                            if (oldSecs < akt - 5) {
                                start.bandbreite = (aktSize - oldSize) * 1_000 / (akt - oldSecs);
                                oldSecs = akt;
                                oldSize = aktSize;
                            }
                        } catch (NumberFormatException ignored) {
                        }
                    }
                }
                // Fortschritt
                matcher = CACHE.get(PATTERN_TIME).matcher(input);
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
                    } else {
                        double aktSecs = Double.parseDouble(zeit);
                        double d = aktSecs / totalSecs * 100;
                        meldenDouble(d);
                    }
                }
            } catch (Exception ex) {
                MessageBus.getMessageBus().publishAsync(new DownloadProgressChangedEvent());
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
                MessageBus.getMessageBus().publishAsync(new DownloadProgressChangedEvent());
            }
        }
    }
}
