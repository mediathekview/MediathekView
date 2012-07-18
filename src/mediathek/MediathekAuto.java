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
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.ListIterator;
import mediathek.controller.filme.BeobFilmeLaden;
import mediathek.controller.filme.FilmListenerElement;
import mediathek.controller.io.IoXmlLesen;
import mediathek.controller.io.starter.Starts;
import mediathek.daten.DDaten;
import mediathek.daten.DatenDownload;

public class MediathekAuto {

    private DDaten ddaten;
    private Date startZeit = new Date(System.currentTimeMillis());
    private Date stopZeit = null;
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
        ddaten = new DDaten(pfad, false);
        Log.startMeldungen(this.getClass().getName());
        if (IoXmlLesen.einstellungenExistieren()) {
            ddaten.allesLaden();
            DDaten.filmeLaden.addAdListener(new BeobachterLadenFilme());
            if (Daten.listeFilme.filmlisteIstAelter()) {
                DDaten.filmeLaden.importFilmliste("");
            } else {
                filmeLaden();
            }
        } else {
            // Programm erst mit der GuiVersion einrichten
            Log.fehlerMeldung(834986137, "MediathekAuto", "Das Programm muss erst mit der Gui-Version eingerichtet werden!");
            System.exit(0);
        }
    }

    private synchronized void filmeLaden() {
        try {
            Log.playerMeldungenAus = true;
            ddaten.listeDownloads.abosLoschen();
            ddaten.listeDownloads.abosEintragen();
            Log.systemMeldung(ddaten.listeDownloads.size() + " Filme zum Laden");
            // erst mal die Filme schreiben
            int i = 0;
            ListIterator<DatenDownload> it = ddaten.listeDownloads.listIterator();
            while (it.hasNext()) {
                DatenDownload d = it.next();
                Log.systemMeldung("Film " + (i++) + ": ");
                Log.systemMeldung(" Sender: " + d.arr[DatenDownload.DOWNLOAD_SENDER_NR]);
                Log.systemMeldung(" Thema: " + d.arr[DatenDownload.DOWNLOAD_THEMA_NR]);
                Log.systemMeldung(" Titel: " + d.arr[DatenDownload.DOWNLOAD_TITEL_NR]);
            }
            Log.systemMeldung("###########################################################");
            // und jetzt starten
            it = ddaten.listeDownloads.listIterator(0);
            while (it.hasNext()) {
                // alle 5 Sekungen einen Download starten
                Starts s = new Starts(it.next());
                ddaten.starterClass.addStarts(s);
                this.wait(5000);
            }
            while (ddaten.starterClass.getStartsWaiting() > 0) {
                //alle 5 Sekunden nachschauen ob schon fertig
                this.wait(5000);
            }
        } catch (Exception ex) {
            Log.fehlerMeldung(769325469, "MediathekAuto.filmeLaden", ex);
        }
        ddaten.listeDownloads.listePutzen();
        ddaten.listeDownloads.abosLoschen();
        ddaten.allesSpeichern();
        undTschuess();
    }

    private void undTschuess() {
        stopZeit = new Date(System.currentTimeMillis());
        SimpleDateFormat sdf = new SimpleDateFormat("dd.MM.yyyy HH:mm:ss");
        int minuten;
        try {
            minuten = Math.round((stopZeit.getTime() - startZeit.getTime()) / (1000 * 60));
        } catch (Exception ex) {
            minuten = -1;
        }
        Log.printFehlerNummer();
        Log.systemMeldung(new String[]{
                    "========================================",
                    "  --> Beginn: " + sdf.format(startZeit),
                    "  --> Fertig: " + sdf.format(stopZeit), "  --> Dauer[Min]: " + (minuten == 0 ? "<1" : minuten),
                    "========================================"});
        System.exit(0);
    }

    private class BeobachterLadenFilme extends BeobFilmeLaden {

        @Override
        public void fertig(FilmListenerElement filmListenerElement) {
                filmeLaden();
        }
    }
}
