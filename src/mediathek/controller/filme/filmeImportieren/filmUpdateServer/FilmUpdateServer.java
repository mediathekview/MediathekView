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
package mediathek.controller.filme.filmeImportieren.filmUpdateServer;

import mediathek.Daten;
import mediathek.controller.filme.FilmeLaden;
import mediathek.Log;

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
        String retUrl = "";
        String version;
        String release;
        String downloadUrlProgramm;
        String[] ret = {"", "", ""};
        try {
            listeUpdateServer.clear();
            ret = FilmUpdateServerSuchen.getListe(listeUpdateServer);
            version = ret[0];
            release = ret[1];
            downloadUrlProgramm = ret[2];
        } catch (Exception ex) {
            Log.fehlerMeldung("FilmUpdateServer.suchen", ex);
            version = "";
            release = "";
            downloadUrlProgramm = "";
        }
        if (listeUpdateServer.isEmpty()) {
            Log.systemMeldung(new String[]{"Es ist ein Fehler aufgetreten!",
                        "Es konnten keine Updateserver zum aktualisieren der Filme",
                        "gefunden werden."});
        } else {
            listeUpdateServer.sort();
            retUrl = listeUpdateServer.getRand(0); //eine Zufällige Adresse wählen
        }
        Daten.setGeaendert();
        return retUrl;
    }
}
