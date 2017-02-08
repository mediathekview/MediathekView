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
package mediathek.controller.starter;

import mSearch.tool.Listener;
import mSearch.tool.Log;
import mSearch.tool.SysMsg;
import mediathek.config.Daten;
import mediathek.config.MVConfig;
import mediathek.controller.MVBandwidthTokenBucket;
import mediathek.controller.MVInputStream;
import mediathek.daten.DatenDownload;
import mediathek.gui.dialog.DialogContinueDownload;
import mediathek.gui.dialog.MeldungDownloadfehler;
import mediathek.gui.messages.DownloadFinishedEvent;
import mediathek.gui.messages.DownloadStartEvent;
import mediathek.tool.MVInfoFile;
import mediathek.tool.MVSubtitle;

import javax.swing.*;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.net.HttpURLConnection;
import java.net.URL;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.ArrayList;

import static mediathek.controller.starter.StarterClass.*;

public class DirectHttpDownload extends Thread {

    private final Daten daten;
    private final DatenDownload datenDownload;
    private final Start start;
    private HttpURLConnection conn = null;
    private HttpDownloadState state = HttpDownloadState.DOWNLOAD;
    private long downloaded = 0;
    private File file = null;
    private String responseCode;
    private String exMessage;

    private FileOutputStream fos = null;

    private final java.util.Timer bandwidthCalculationTimer;
    private boolean retAbbrechen;
    private boolean dialogAbbrechenIsVis;

    public DirectHttpDownload(Daten daten, DatenDownload d, java.util.Timer bandwidthCalculationTimer) {
        super();
        this.daten = daten;
        this.bandwidthCalculationTimer = bandwidthCalculationTimer;
        datenDownload = d;
        start = datenDownload.start;
        setName("DIRECT DL THREAD_" + d.arr[DatenDownload.DOWNLOAD_TITEL]);
        start.status = Start.STATUS_RUN;
        notifyStartEvent(datenDownload);
    }

    /**
     * HTTP Timeout in milliseconds.
     */
    private static final int TIMEOUT_LENGTH = 5000;

    /**
     * Return the content length of the requested Url.
     *
     * @param url {@link java.net.URL} to the specified content.
     * @return Length in bytes or -1 on error.
     */
    private long getContentLength(final URL url) {
        long ret = -1;
        HttpURLConnection connection = null;
        try {
            connection = (HttpURLConnection) url.openConnection();
            connection.setRequestProperty("User-Agent", Daten.getUserAgent());
            connection.setReadTimeout(TIMEOUT_LENGTH);
            connection.setConnectTimeout(TIMEOUT_LENGTH);
            if (connection.getResponseCode() < HttpURLConnection.HTTP_BAD_REQUEST) {
                ret = connection.getContentLengthLong();
            }
            // alles unter 300k sind Playlisten, ...
            if (ret < 300 * 1000) {
                ret = -1;
            }
        } catch (Exception ex) {
            ret = -1;
            Log.errorLog(643298301, ex);
        } finally {
            if (connection != null) {
                connection.disconnect();
            }
        }
        return ret;
    }

    /**
     * Setup the HTTP connection common settings
     *
     * @param conn The active connection.
     */
    private void setupHttpConnection(HttpURLConnection conn) {
        conn.setRequestProperty("Range", "bytes=" + downloaded + '-');
        conn.setRequestProperty("User-Agent", Daten.getUserAgent());
        conn.setDoInput(true);
        conn.setDoOutput(true);
    }

    /**
     * Start the actual download process here.
     *
     * @throws Exception
     */
    private void downloadContent() throws Exception {
        if (Boolean.parseBoolean(datenDownload.arr[DatenDownload.DOWNLOAD_INFODATEI])) {
            MVInfoFile.writeInfoFile(datenDownload);
        }
        if (Boolean.parseBoolean(datenDownload.arr[DatenDownload.DOWNLOAD_SUBTITLE])) {
            new MVSubtitle().writeSubtitle(datenDownload);
        }
        datenDownload.interruptRestart();

        start.mVInputStream = new MVInputStream(conn.getInputStream(), bandwidthCalculationTimer);

        fos = new FileOutputStream(file, (downloaded != 0));

        datenDownload.mVFilmSize.addAktSize(downloaded);
        final byte[] buffer = new byte[MVBandwidthTokenBucket.DEFAULT_BUFFER_SIZE];
        long p, pp = 0, startProzent = -1;
        int len;
        long aktBandwidth, aktSize = 0;
        boolean melden = false;

        while ((len = start.mVInputStream.read(buffer)) != -1 && (!start.stoppen)) {
            downloaded += len;
            fos.write(buffer, 0, len);
            datenDownload.mVFilmSize.addAktSize(len);

            //für die Anzeige prüfen ob sich was geändert hat
            if (aktSize != datenDownload.mVFilmSize.getAktSize()) {
                aktSize = datenDownload.mVFilmSize.getAktSize();
                melden = true;
            }
            if (datenDownload.mVFilmSize.getSize() > 0) {
                p = (aktSize * (long) 1000) / datenDownload.mVFilmSize.getSize();
                if (startProzent == -1) {
                    startProzent = p;
                }
                // p muss zwischen 1 und 999 liegen
                if (p == 0) {
                    p = Start.PROGRESS_GESTARTET;
                } else if (p >= 1000) {
                    p = 999;
                }
                start.percent = (int) p;
                if (p != pp) {
                    pp = p;
                    // Restzeit ermitteln
                    if (p > 2 && p > startProzent) {
                        // sonst macht es noch keinen Sinn
                        final int diffZeit = start.startZeit.diffInSekunden();
                        final int restProzent = 1000 - (int) p;
                        start.restSekunden = (diffZeit * restProzent / (p - startProzent));
                        // anfangen zum Schauen kann man, wenn die Restzeit kürzer ist
                        // als die bereits geladene Speilzeit des Films
                        bereitsAnschauen(datenDownload);
                    }
                    melden = true;
                }
            }
            aktBandwidth = start.mVInputStream.getBandwidth(); // bytes per second
            if (aktBandwidth != start.bandbreite) {
                start.bandbreite = aktBandwidth;
                melden = true;
            }
            if (melden) {
                Listener.notify(Listener.EREIGNIS_ART_DOWNLOAD_PROZENT, StarterClass.class.getName());
                melden = false;
            }
        }
        start.bandbreite = start.mVInputStream.getSumBandwidth();
        if (!start.stoppen) {
            if (datenDownload.quelle == DatenDownload.QUELLE_BUTTON) {
                // direkter Start mit dem Button
                start.status = Start.STATUS_FERTIG;
            } else if (pruefen(daten, datenDownload, start)) {
                //Anzeige ändern - fertig
                start.status = Start.STATUS_FERTIG;
            } else {
                //Anzeige ändern - bei Fehler fehlt der Eintrag
                start.status = Start.STATUS_ERR;
            }
        }
    }

    @Override
    public synchronized void run() {
        startmeldung(datenDownload, start);
        daten.getMessageBus().publishAsync(new DownloadStartEvent());

        try {
            Files.createDirectories(Paths.get(datenDownload.arr[DatenDownload.DOWNLOAD_ZIEL_PFAD]));
        } catch (IOException ignored) {
        }

        int restartCount = 0;
        boolean restart = true;
        while (restart) {
            restart = false;
            try {
                final URL url = new URL(datenDownload.arr[DatenDownload.DOWNLOAD_URL]);
                file = new File(datenDownload.arr[DatenDownload.DOWNLOAD_ZIEL_PFAD_DATEINAME]);

                if (!cancelDownload()) {

                    datenDownload.mVFilmSize.setSize(getContentLength(url));
                    datenDownload.mVFilmSize.setAktSize(0);
                    conn = (HttpURLConnection) url.openConnection();
                    conn.setConnectTimeout(1000 * MVConfig.getInt(MVConfig.Configs.SYSTEM_PARAMETER_DOWNLOAD_TIMEOUT_SEKUNDEN));
                    conn.setReadTimeout(1000 * MVConfig.getInt(MVConfig.Configs.SYSTEM_PARAMETER_DOWNLOAD_TIMEOUT_SEKUNDEN));

                    setupHttpConnection(conn);
                    conn.connect();
                    final int httpResponseCode = conn.getResponseCode();
                    if (httpResponseCode >= HttpURLConnection.HTTP_BAD_REQUEST) {
                        //Range passt nicht, also neue Verbindung versuchen...
                        if (httpResponseCode == 416) {
                            conn.disconnect();
                            //Get a new connection and reset download param...
                            conn = (HttpURLConnection) url.openConnection();
                            downloaded = 0;
                            setupHttpConnection(conn);
                            conn.connect();
                            //hier war es dann nun wirklich...
                            state = conn.getResponseCode() >= HttpURLConnection.HTTP_BAD_REQUEST ? HttpDownloadState.ERROR : HttpDownloadState.DOWNLOAD;
                        } else {
                            // ==================================
                            // dann wars das
                            responseCode = "Responsecode: " + conn.getResponseCode() + '\n' + conn.getResponseMessage();
                            Log.errorLog(915236798, "HTTP-Fehler: " + conn.getResponseCode() + ' ' + conn.getResponseMessage());
                            SwingUtilities.invokeLater(() -> {
                                if (!Daten.isAuto()) {
                                    new MeldungDownloadfehler(Daten.getInstance().getMediathekGui(), "URL des Films:\n"
                                            + datenDownload.arr[DatenDownload.DOWNLOAD_URL] + "\n\n"
                                            + responseCode + '\n', datenDownload).setVisible(true);
                                }
                            });
                            state = HttpDownloadState.ERROR;
                        }
                    }
                }
                switch (state) {
                    case DOWNLOAD:
                        downloadContent();
                        break;
                    case CANCEL:
//                        start.status = Start.STATUS_RUN; // bei "init" wird er sonst nochmal gestartet
                        break;
                    case ERROR:
                        start.status = Start.STATUS_ERR;
                        break;
                }
            } catch (Exception ex) {
                if ((ex instanceof java.io.IOException || ex instanceof java.net.SocketTimeoutException)
                        && restartCount < MVConfig.getInt(MVConfig.Configs.SYSTEM_PARAMETER_DOWNLOAD_MAX_RESTART_HTTP)) {

                    if (ex instanceof java.net.SocketTimeoutException) {
                        //Timeout Fehlermeldung für zxd :)
                        ArrayList<String> text = new ArrayList<>();
                        text.add("Timeout, Download Restarts: " + restartCount);
                        text.add("Ziel: " + datenDownload.arr[DatenDownload.DOWNLOAD_ZIEL_PFAD_DATEINAME]);
                        text.add("URL: " + datenDownload.arr[DatenDownload.DOWNLOAD_URL]);
                        SysMsg.sysMsg(text.toArray(new String[text.size()]));
                    }

                    restartCount++;
                    restart = true;
                } else {
                    // dann weiß der Geier!
                    exMessage = ex.getLocalizedMessage();
                    Log.errorLog(316598941, ex, "Fehler");
                    start.status = Start.STATUS_ERR;
                    SwingUtilities.invokeLater(() -> {
                        if (!Daten.isAuto()) {
                            new MeldungDownloadfehler(Daten.getInstance().getMediathekGui(), exMessage, datenDownload).setVisible(true);
                        }
                    });
                }
            }
        }

        try {
            if (start.mVInputStream != null) {
                start.mVInputStream.close();
            }
            if (fos != null) {
                fos.close();
            }
            if (conn != null) {
                conn.disconnect();
            }
        } catch (Exception ignored) {
        }

        StarterClass.finalizeDownload(datenDownload, start, state);
        daten.getMessageBus().publishAsync(new DownloadFinishedEvent());
    }

    private boolean cancelDownload() {
        if (!file.exists()) {
            // dann ist alles OK
            return false;
        }
        if (Daten.isAuto()) {
            return false; // immer überschreiben, keine GUI!!!
        }

        dialogAbbrechenIsVis = true;
        retAbbrechen = true;
        if (SwingUtilities.isEventDispatchThread()) {
            retAbbrechen = abbrechen_();
        } else {
            SwingUtilities.invokeLater(() -> {
                retAbbrechen = abbrechen_();
                dialogAbbrechenIsVis = false;
            });
        }
        while (dialogAbbrechenIsVis) {
            try {
                wait(100);
            } catch (Exception ignored) {

            }
        }
        return retAbbrechen;
    }

    private boolean abbrechen_() {
        boolean result = false;
        if (file.exists()) {
            DialogContinueDownload dialogContinueDownload = new DialogContinueDownload(Daten.getInstance().getMediathekGui(), datenDownload, true /*weiterführen*/);
            dialogContinueDownload.setVisible(true);

            switch (dialogContinueDownload.getResult()) {
                case CANCELLED:
                    // dann wars das
                    state = HttpDownloadState.CANCEL;
                    result = true;
                    break;

                case CONTINUE:
                    downloaded = file.length();
                    break;

                case RESTART_WITH_NEW_NAME:
                    if (dialogContinueDownload.isNewName()) {
                        Listener.notify(Listener.EREIGNIS_LISTE_DOWNLOADS, this.getClass().getSimpleName());
                        try {
                            Files.createDirectories(Paths.get(datenDownload.arr[DatenDownload.DOWNLOAD_ZIEL_PFAD]));
                        } catch (IOException ignored) {
                        }
                        file = new File(datenDownload.arr[DatenDownload.DOWNLOAD_ZIEL_PFAD_DATEINAME]);
                    }
                    break;
            }
        }
        return result;
    }

    private void bereitsAnschauen(DatenDownload datenDownload) {
        if (datenDownload.film != null && datenDownload.start != null) {
            if (datenDownload.film.dauerL > 0
                    && datenDownload.start.restSekunden > 0
                    && datenDownload.mVFilmSize.getAktSize() > 0
                    && datenDownload.mVFilmSize.getSize() > 0) {
                // macht nur dann Sinn
                final long zeitGeladen = datenDownload.film.dauerL * datenDownload.mVFilmSize.getAktSize() / datenDownload.mVFilmSize.getSize();
                if (zeitGeladen > (datenDownload.start.restSekunden * 1.1 /* plus 10% zur Sicherheit*/)) {
                    datenDownload.start.beginnAnschauen = true;
                }
            }
        }
    }
}
