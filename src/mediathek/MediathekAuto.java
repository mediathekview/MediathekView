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
import java.util.ListIterator;
import mediathek.controller.IoXmlLesen;
import mediathek.daten.Daten;
import mediathek.daten.DatenDownload;
import mediathek.tool.Log;
import msearch.filmeSuchen.MSearchListenerFilmeLaden;
import msearch.filmeSuchen.MSearchListenerFilmeLadenEvent;

public class MediathekAuto {

    private Daten daten;
    private String pfad = "";

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

    public void starten() {
        daten = new Daten(pfad, null);
        Daten.auto = true;
        Log.startMeldungen(this.getClass().getName());
        if (IoXmlLesen.einstellungenExistieren()) {
            daten.allesLaden();
            Daten.filmeLaden.addAdListener(new MSearchListenerFilmeLaden() {
                @Override
                public void fertig(MSearchListenerFilmeLadenEvent event) {
                    filmeLaden();
                }
            });
            if (Daten.listeFilme.filmlisteZuAlt()) {
                Log.systemMeldung("Neue Filmliste laden");
                Daten.filmeLaden.importFilmliste("");
            } else {
                Log.systemMeldung("aktuelle Filmliste verwenden");
                filmeLaden();
            }
        } else {
            // Programm erst mit der GuiVersion einrichten
            Log.fehlerMeldung(834986137, Log.FEHLER_ART_AUTO, "MediathekAuto", "Das Programm muss erst mit der Gui-Version eingerichtet werden!");
            System.exit(0);
        }
    }

    private synchronized void filmeLaden() {
        try {
            Log.playerMeldungenAus = true;
            Daten.listeDownloads.abosSuchen(null);
            Log.systemMeldung(Daten.listeDownloads.size() + " Filme zum Laden");
            // erst mal die Filme schreiben
            int i = 0;
            ListIterator<DatenDownload> it = Daten.listeDownloads.listIterator();
            while (it.hasNext()) {
                DatenDownload d = it.next();
                Log.systemMeldung("Film " + (i++) + ": ");
                Log.systemMeldung(" Sender: " + d.arr[DatenDownload.DOWNLOAD_SENDER_NR]);
                Log.systemMeldung(" Thema: " + d.arr[DatenDownload.DOWNLOAD_THEMA_NR]);
                Log.systemMeldung(" Titel: " + d.arr[DatenDownload.DOWNLOAD_TITEL_NR]);
            }
            Log.systemMeldung("###########################################################");
            // und jetzt starten
            it = Daten.listeDownloads.listIterator(0);
            while (it.hasNext()) {
                // alle 5 Sekungen einen Download starten
                it.next().startenDownload(daten);
                this.wait(5000);
            }
            while (Daten.listeDownloads.getStartsNotFinished() > 0) {
                //alle 5 Sekunden nachschauen ob schon fertig
                this.wait(5000);
            }
        } catch (Exception ex) {
            Log.fehlerMeldung(769325469, Log.FEHLER_ART_AUTO, "MediathekAuto.filmeLaden", ex);
        }
        daten.allesSpeichern();
        Log.printEndeMeldung();
        System.exit(0);
    }
}
