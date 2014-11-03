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

import java.io.File;
import java.nio.file.Path;
import java.util.concurrent.TimeUnit;
import mediathek.controller.IoXmlLesen;
import mediathek.controller.Log;
import mediathek.daten.Daten;
import mediathek.daten.DatenDownload;
import mediathek.tool.MVConfig;
import mediathek.tool.MVListeFilme;
import msearch.filmeSuchen.MSListenerFilmeLaden;
import msearch.filmeSuchen.MSListenerFilmeLadenEvent;
import msearch.io.MSFilmlisteLesen;

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
        daten = new Daten(pfad, null);
        Daten.auto = true;
        Log.startMeldungen(this.getClass().getName());

        if (!IoXmlLesen.einstellungenExistieren()) {
            // Programm erst mit der GuiVersion einrichten
            Log.fehlerMeldung(834986137, "MediathekAuto", "Das Programm muss erst mit der Gui-Version eingerichtet werden!");
            System.exit(1);
        }

        // Einstellungen laden
        Path xmlFilePath = Daten.getMediathekXmlFilePath();
        Log.systemMeldung("Einstellungen laden: " + xmlFilePath.toString());
        if (!IoXmlLesen.datenLesen(xmlFilePath)) {
            // dann hat das Laden nicht geklappt
            Log.fehlerMeldung(834986137, "MediathekAuto", "Einstellungen konnten nicht geladen werden: " + xmlFilePath.toString());
            System.exit(1);
        }

        // Filmliste laden
        MSFilmlisteLesen filmList = new MSFilmlisteLesen();
        if (bFastAuto) {
            //do not read film descriptions in FASTAUTO mode as they won´t be used...
            filmList.setWorkMode(MSFilmlisteLesen.WorkMode.FASTAUTO);
        }
        filmList.readFilmListe(Daten.getDateiFilmliste(), Daten.listeFilme, Integer.parseInt(Daten.mVConfig.get(MVConfig.SYSTEM_ANZ_TAGE_FILMLISTE)));

        if (Daten.listeFilme.isTooOld()) {
            // erst neue Filmliste laden
            Log.systemMeldung("Neue Filmliste laden");
            Daten.filmeLaden.addAdListener(new MSListenerFilmeLaden() {
                @Override
                public void fertig(MSListenerFilmeLadenEvent event) {
                    download();
                }
            });
            Daten.filmeLaden.importFilmliste("");
        } else {
            // mit aktueller Filmliste starten
            Log.systemMeldung("aktuelle Filmliste verwenden");
            // Liste erst mal aufbereiten
            Daten.listeAbo.setAboFuerFilm(Daten.listeFilme, false /*aboLoeschen*/);
            MVListeFilme.checkBlacklist();
            download();
        }
    }

    final private static int SLEEP_VALUE = 2_000;
    final private static int SLEEP_VALUE_TIMES_TWO = 2 * SLEEP_VALUE;

    private synchronized void download() {
        try {
            Log.playerMeldungenAus = true;
            Daten.listeDownloads.abosSuchen(null);

            Log.systemMeldung(Daten.listeDownloads.size() + " Filme zum Laden");
            Log.systemMeldung("");
            // erst mal die Filme schreiben
            int i = 1;
            for (DatenDownload d : Daten.listeDownloads) {
                Log.systemMeldung("Film " + (i++) + ": ");
                Log.systemMeldung("\tSender: " + d.arr[DatenDownload.DOWNLOAD_SENDER_NR]);
                Log.systemMeldung("\tThema: " + d.arr[DatenDownload.DOWNLOAD_THEMA_NR]);
                Log.systemMeldung("\tTitel: " + d.arr[DatenDownload.DOWNLOAD_TITEL_NR]);
                Log.systemMeldung("");
            }
            Log.systemMeldung("###########################################################");
            // und jetzt starten
            for (DatenDownload d : Daten.listeDownloads) {
                d.startDownload(daten);
            }

            while (Daten.listeDownloads.getNumberOfStartsNotFinished() > 0) {
                long remTime = Daten.listeDownloads.getMaximumFinishTimeOfRunningStarts();
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
            Log.fehlerMeldung(769325469, "MediathekAuto.filmeLaden", ex);
        }
        daten.allesSpeichern();
        Log.printEndeMeldung();
        System.exit(0);
    }
}
