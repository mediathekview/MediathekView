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
package mediathek.controller.filmeLaden.importieren;

import mediathek.daten.Daten;
import mediathek.tool.Konstanten;
import mediathek.tool.Log;
import mediathek.tool.ListenerMediathekView;

public class FilmUpdateServer {
    //Tags FilmUpdateServer Filmliste

    public static final String FILM_UPDATE_SERVER_PRIO_1 = "1";
    public static final String FILM_UPDATE_SERVER_PRIO_2 = "2";
    public static final String FILM_UPDATE_SERVER = "film-update-server";
    public static final int FILM_UPDATE_SERVER_MAX_ELEM = 6;
    public static final String FILM_UPDATE_SERVER_NR = "film-update-server-nr";
    public static final int FILM_UPDATE_SERVER_NR_NR = 0;
    public static final String FILM_UPDATE_SERVER_URL = "film-update-server-url";
    public static final int FILM_UPDATE_SERVER_URL_NR = 1;
    public static final String FILM_UPDATE_SERVER_DATUM = "film-update-server-datum";
    public static final int FILM_UPDATE_SERVER_DATUM_NR = 2;
    public static final String FILM_UPDATE_SERVER_ZEIT = "film-update-server-zeit";
    public static final int FILM_UPDATE_SERVER_ZEIT_NR = 3;
    public static final String FILM_UPDATE_SERVER_ANZAHL = "film-update-server-anzahl";
    public static final int FILM_UPDATE_SERVER_ANZAHL_NR = 4;
    public static final String FILM_UPDATE_SERVER_PRIO = "film-update-server-prio";
    public static final int FILM_UPDATE_SERVER_PRIO_NR = 5;
    public static final String[] FILM_UPDATE_SERVER_COLUMN_NAMES = {FILM_UPDATE_SERVER_NR, FILM_UPDATE_SERVER_URL, FILM_UPDATE_SERVER_DATUM, FILM_UPDATE_SERVER_ZEIT, FILM_UPDATE_SERVER_ANZAHL, FILM_UPDATE_SERVER_PRIO};
    public static final String[] FILM_UPDATE_SERVER_COLUMN_NAMES_ANZEIGE = {"Nr", "Update-Url", "Datum", "Zeit", "Anzahl", FILM_UPDATE_SERVER_PRIO};
    public ListeFilmUpdateServer listeUpdateServer = new ListeFilmUpdateServer();

    public String suchen() {
        String retUrl;
        ListeFilmUpdateServer tmp = new ListeFilmUpdateServer();
        try {
            FilmUpdateServerSuchen.getListe(Konstanten.ADRESSE_UPDATE_SERVER, tmp);
        } catch (Exception ex) {
            Log.fehlerMeldung(347895642, "FilmUpdateServer.suchen", ex);
        }
        if (tmp.isEmpty()) {
            Log.systemMeldung(new String[]{"Es ist ein Fehler aufgetreten!",
                        "Es konnten keine Updateserver zum aktualisieren der Filme",
                        "gefunden werden."});
        } else {
            listeUpdateServer = tmp;
            listeUpdateServer.sort();
        }
        if (listeUpdateServer.size() == 0) {
            listeUpdateServer.add(new DatenFilmUpdateServer("http://176.28.14.91/mediathek1/Mediathek_14.bz2", "1"));
            listeUpdateServer.add(new DatenFilmUpdateServer("http://176.28.14.91/mediathek2/Mediathek_16.bz2", "1"));
            listeUpdateServer.add(new DatenFilmUpdateServer("http://176.28.14.91/mediathek3/Mediathek_18.bz2", "1"));
            listeUpdateServer.add(new DatenFilmUpdateServer("http://176.28.14.91/mediathek4/Mediathek_20.bz2", "1"));
            listeUpdateServer.add(new DatenFilmUpdateServer("http://176.28.14.91/mediathek1/Mediathek_22.bz2", "1"));
        }
        retUrl = listeUpdateServer.getRand(0); //eine Zufällige Adresse wählen
        Daten.notifyMediathekListener(ListenerMediathekView.EREIGNIS_LISTE_UPDATESERVER, this.getClass().getSimpleName());
        Daten.setGeaendert();
        return retUrl;
    }
}
