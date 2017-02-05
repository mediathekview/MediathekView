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

import mSearch.filmeSuchen.ListenerFilmeLaden;
import mSearch.filmeSuchen.ListenerFilmeLadenEvent;
import mSearch.filmlisten.FilmlisteLesen;
import mSearch.tool.Log;
import mSearch.tool.SysMsg;
import mediathek.config.Daten;
import mediathek.config.MVConfig;
import mediathek.controller.IoXmlLesen;
import mediathek.daten.DatenDownload;
import mediathek.tool.MVFilmSize;

import java.awt.*;
import java.io.File;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.concurrent.TimeUnit;

import static mSearch.tool.Log.LILNE;
import static mediathek.tool.MVFunctionSys.startMeldungen;

public class MediathekAuto {

    private Daten daten;
    private String pfad = "";
    private boolean bFastAuto = false;

    public MediathekAuto(String[] ar) {
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
        } catch (Exception ignored) {
            SysMsg.sysMsg("NoSplashscreen");
        }
    }

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

    public void starten() {
        daten = Daten.getInstance(pfad);
        daten.setAuto(true);
        startMeldungen();

        Path xmlFilePath = daten.getMediathekXmlFilePath();
        if (!Files.exists(xmlFilePath)) {
            // Programm erst mit der GuiVersion einrichten
            Log.errorLog(834986137, "Das Programm muss erst mit der GUI-Version eingerichtet werden!");
            System.exit(1);
        }

        try (IoXmlLesen reader = new IoXmlLesen(daten)) {
            // Einstellungen laden
            SysMsg.sysMsg("Einstellungen laden: " + xmlFilePath.toString());
            if (!reader.readConfiguration(xmlFilePath)) {
                // dann hat das Laden nicht geklappt
                Log.errorLog(834986137, "Einstellungen konnten nicht geladen werden: " + xmlFilePath.toString());
                System.exit(1);
            }
        } catch (Exception ex) {
            ex.printStackTrace();
        }

        // Filmliste laden
        FilmlisteLesen filmList = new FilmlisteLesen();
        if (bFastAuto) {
            //do not read film descriptions in FASTAUTO mode as they won´t be used...
            FilmlisteLesen.setWorkMode(FilmlisteLesen.WorkMode.FASTAUTO);
        }
        filmList.readFilmListe(Daten.getDateiFilmliste(), daten.getListeFilme(), Integer.parseInt(MVConfig.get(MVConfig.Configs.SYSTEM_ANZ_TAGE_FILMLISTE)));

        if (daten.getListeFilme().isTooOld()) {
            // erst neue Filmliste laden
            SysMsg.sysMsg("Neue Filmliste laden");
            daten.getFilmeLaden().addAdListener(new ListenerFilmeLaden() {
                @Override
                public void fertig(ListenerFilmeLadenEvent event) {
                    download();
                }
            });
            daten.getFilmeLaden().loadFilmlist("", true);
        } else {
            // mit aktueller Filmliste starten
            SysMsg.sysMsg("aktuelle Filmliste verwenden");
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

            SysMsg.sysMsg(daten.getListeDownloads().size() + " Filme zum Laden");
            SysMsg.sysMsg("");
            // erst mal die Filme schreiben
            int i = 1;
            for (DatenDownload d : daten.getListeDownloads()) {
                SysMsg.sysMsg("Film " + (i++) + ": ");
                SysMsg.sysMsg("\tSender: " + d.arr[DatenDownload.DOWNLOAD_SENDER]);
                SysMsg.sysMsg("\tThema: " + d.arr[DatenDownload.DOWNLOAD_THEMA]);
                SysMsg.sysMsg("\tTitel: " + d.arr[DatenDownload.DOWNLOAD_TITEL]);
                String size = MVFilmSize.getGroesse(d.mVFilmSize.getSize());
                if (!size.isEmpty()) {
                    SysMsg.sysMsg("\tGröße: " + size + " MByte");
                }
                SysMsg.sysMsg("");
            }
            SysMsg.sysMsg(LILNE);
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
