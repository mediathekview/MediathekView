/*
 *  MediathekView
 *  Copyright (C) 2008 W. Xaver
 *  W.Xaver[at]googlemail.com
 *  http://zdfmediathk.sourceforge.net/
 *
 *  This program is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation, either version 3 of the License, or
 *  any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package mediathek;

import javafx.application.Application;
import mSearch.filmeSuchen.ListenerFilmeLaden;
import mSearch.filmeSuchen.ListenerFilmeLadenEvent;
import mSearch.filmlisten.reader.FastAutoFilmListReader;
import mSearch.filmlisten.reader.FilmListReader;
import mSearch.tool.Log;
import mSearch.tool.SysMsg;
import mediathek.config.Daten;
import mediathek.config.MVConfig;
import mediathek.controller.IoXmlLesen;
import mediathek.daten.DatenDownload;
import mediathek.tool.MVFilmSize;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.awt.*;
import java.io.File;
import java.nio.file.Path;
import java.util.concurrent.TimeUnit;

import static mSearch.tool.Log.LILNE;
import static mediathek.tool.MVFunctionSys.startMeldungen;

public class MediathekAuto {

    private Daten daten;
    private String pfad = "";
    private boolean bFastAuto = false;

    public MediathekAuto(String[] ar) {
        Thread fxThread = new Thread(() -> {
            Application.launch(DummyFXApp.class);
        });
        fxThread.setName("DummyFXApp Thread");
        fxThread.start();

        if (ar != null) {
            if (ar.length > 0) {
                if (!ar[0].startsWith("-")) {
                    if (!ar[0].endsWith(File.separator)) {
                        ar[0] += File.separator;
                    }
                    pfad = ar[0];
                }
            }
        }
        try {
            final SplashScreen splash = SplashScreen.getSplashScreen();
            if (splash != null) {
                splash.close();
            }
        } catch (Exception ex) {
            logger.warn(ex);
        }
    }

    private static final Logger logger = LogManager.getLogger(MediathekAuto.class);

    /**
     * Set fast auto mode for reading film list.
     * In this mode no film descriptions will be stored in memory.
     * Speeds up startup significantly on low power machines.
     *
     * @param bFastAuto if true, don´t read descriptions.
     */
    public void setFastAuto(boolean bFastAuto) {
        this.bFastAuto = bFastAuto;
    }

    @SuppressWarnings("resource")
    public void starten() {
        daten = Daten.getInstance(pfad);
        Daten.setAuto(true);
        startMeldungen();

        final IoXmlLesen configReader = new IoXmlLesen();
        if (!configReader.einstellungenExistieren()) {
            // Programm erst mit der GuiVersion einrichten
            logger.error("Das Programm muss erst mit der Gui-Version eingerichtet werden!");
            System.exit(1);
        }

        // Einstellungen laden
        Path xmlFilePath = Daten.getMediathekXmlFilePath();
        logger.info("Einstellungen laden: {}", xmlFilePath.toString());
        if (!configReader.datenLesen(xmlFilePath)) {
            // dann hat das Laden nicht geklappt
            logger.error("Einstellungen konnten nicht geladen werden: {}", xmlFilePath.toString());
            System.exit(1);
        }

        // Filmliste laden
        FilmListReader filmList = null;
        try {
            if (bFastAuto)
                filmList = new FastAutoFilmListReader();
            else
                filmList = new FilmListReader();
            filmList.readFilmListe(Daten.getDateiFilmliste(), daten.getListeFilme(), Integer.parseInt(MVConfig.get(MVConfig.Configs.SYSTEM_ANZ_TAGE_FILMLISTE)));
        } finally {
            if (filmList != null) {
                filmList.close();
            }
        }
        if (daten.getListeFilme().isTooOld()) {
            // erst neue Filmliste laden
            logger.info("Neue Filmliste laden");
            daten.getFilmeLaden().addAdListener(new ListenerFilmeLaden() {
                @Override
                public void fertig(ListenerFilmeLadenEvent event) {
                    download();
                }
            });
            daten.getFilmeLaden().loadFilmlist("", true);
        } else {
            // mit aktueller Filmliste starten
            logger.info("aktuelle Filmliste verwenden");
            // Liste erst mal aufbereiten
            daten.getListeAbo().setAboFuerFilm(daten.getListeFilme(), false /*aboLoeschen*/);
            daten.getListeBlacklist().filterListe();
            download();
        }
    }

    final private static int SLEEP_VALUE = 2_000;
    final private static int SLEEP_VALUE_TIMES_TWO = 2 * SLEEP_VALUE;

    private synchronized void download() {
        try {
            SysMsg.playerMeldungenAus = true;
            daten.getListeDownloads().abosSuchen(null);
            daten.getListeDownloads().filmEintragen(); //für gespeicherte Downloads

            logger.info("{} Filme zum Laden", daten.getListeDownloads().size());
            logger.info("");
            // erst mal die Filme schreiben
            int i = 1;
            for (DatenDownload d : daten.getListeDownloads()) {
                logger.info("Film " + (i++) + ": ");
                logger.info("\tSender: " + d.arr[DatenDownload.DOWNLOAD_SENDER]);
                logger.info("\tThema: " + d.arr[DatenDownload.DOWNLOAD_THEMA]);
                logger.info("\tTitel: " + d.arr[DatenDownload.DOWNLOAD_TITEL]);
                String size = MVFilmSize.getGroesse(d.mVFilmSize.getSize());
                if (!size.isEmpty()) {
                    logger.info("\tGröße: " + size + " MByte");
                }
                logger.info("");
            }
            logger.info(LILNE);
            // und jetzt starten
            for (DatenDownload d : daten.getListeDownloads()) {
                d.startDownload(daten);
            }

            while (daten.getListeDownloads().getNumberOfStartsNotFinished() > 0) {
                long remTime = daten.getListeDownloads().getMaximumFinishTimeOfRunningStarts();
                if (remTime == 0 || (remTime < 10)) {
                    remTime = SLEEP_VALUE;
                } else {
                    //The following hack ensures that when we are close to the end we start polling faster...
                    remTime = TimeUnit.MILLISECONDS.convert(remTime, TimeUnit.SECONDS);
                    remTime /= 2;
                    if (remTime < SLEEP_VALUE_TIMES_TWO) {
                        remTime = SLEEP_VALUE;
                    }
                }

                Thread.sleep(remTime);
            }
        } catch (Exception ex) {
            Log.errorLog(769325469, ex);
        }
        daten.allesSpeichern();
        Log.endMsg();
        System.exit(0);
    }
}
