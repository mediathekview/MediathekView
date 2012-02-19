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

import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.*;

public class ListeFilmUpdateServer extends LinkedList<DatenFilmUpdateServer> {

    public boolean addWithCheck(DatenFilmUpdateServer film) {
        // Film wird in die BackListe geschrieben, check20 muss dann selbst aufgerufen werden
        ListIterator<DatenFilmUpdateServer> it = listIterator();
        while (it.hasNext()) {
            if (it.next().arr[FilmUpdateServer.FILM_UPDATE_SERVER_URL_NR].equals(film.arr[FilmUpdateServer.FILM_UPDATE_SERVER_URL_NR])) {
                return false;
            }
        }
        return add(film);
    }

    public void sort() {
        int nr = 0;
        Collections.<DatenFilmUpdateServer>sort(this);
        Iterator<DatenFilmUpdateServer> it = this.iterator();
        while (it.hasNext()) {
            String str = String.valueOf(nr++);
            while (str.length() < 3) {
                str = "0" + str;
            }
            it.next().arr[FilmUpdateServer.FILM_UPDATE_SERVER_NR_NR] = str;
        }
    }

    public String[][] getTableObjectData() {
        DatenFilmUpdateServer filmUpdate;
        String[][] object;
        ListIterator<DatenFilmUpdateServer> iterator = this.listIterator();
        object = new String[this.size()][FilmUpdateServer.FILM_UPDATE_SERVER_MAX_ELEM];
        int i = 0;
        while (iterator.hasNext()) {
            filmUpdate = iterator.next();
            object[i] = filmUpdate.arr;
            ++i;
        }
        return object;
    }

    public int getNr(String url) {
        int nr = 0;
        ListIterator<DatenFilmUpdateServer> iterator = this.listIterator();
        while (iterator.hasNext()) {
            if (iterator.next().arr[FilmUpdateServer.FILM_UPDATE_SERVER_URL_NR].equals(url)) {
                break;
            }
            ++nr;
        }
        if (nr >= this.size()) {
            nr = 0;
        }
        return nr;
    }

    public DatenFilmUpdateServer getNrUpdate(String url) {
        DatenFilmUpdateServer update;
        ListIterator<DatenFilmUpdateServer> iterator = this.listIterator();
        while (iterator.hasNext()) {
            update = iterator.next();
            if (update.arr[FilmUpdateServer.FILM_UPDATE_SERVER_URL_NR].equals(url)) {
                return update;
            }
        }
        return null;
    }

    public String getRand(int errcount) {
        final int MAXMINUTEN = 50;
        int minCount = 2;
        if (errcount > 0) {
            minCount = 2 * errcount;
        }
        String ret = "";
        if (!this.isEmpty()) {
            DatenFilmUpdateServer filmUpdate;
            Iterator<DatenFilmUpdateServer> it;
            LinkedList<DatenFilmUpdateServer> listePrio = new LinkedList<DatenFilmUpdateServer>();
            LinkedList<DatenFilmUpdateServer> listeZeit = new LinkedList<DatenFilmUpdateServer>();
            //aktuellsten auswählen
            it = this.iterator();
            Date today = new Date(System.currentTimeMillis());
            String date;
            SimpleDateFormat sdf = new SimpleDateFormat("dd.MM.yyyy HH:mm:ss");
            Date d;
            int minuten = 200;
            int count = 0;
            while (it.hasNext()) {
                filmUpdate = it.next();
                date = filmUpdate.arr[FilmUpdateServer.FILM_UPDATE_SERVER_DATUM_NR] + " " + filmUpdate.arr[FilmUpdateServer.FILM_UPDATE_SERVER_ZEIT_NR];
                try {
                    d = sdf.parse(date);
                    minuten = Math.round((today.getTime() - d.getTime()) / (1000 * 60));
                } catch (ParseException ex) {
                }
                if (minuten < MAXMINUTEN) {
                    listeZeit.add(filmUpdate);
                    ++count;
                } else if (count < minCount) {
                    listeZeit.add(filmUpdate);
                    ++count;
                }
            }
            //nach prio gewichten
            it = listeZeit.iterator();
            while (it.hasNext()) {
                filmUpdate = it.next();
                if (filmUpdate.arr[FilmUpdateServer.FILM_UPDATE_SERVER_PRIO_NR].equals(FilmUpdateServer.FILM_UPDATE_SERVER_PRIO_1)) {
                    listePrio.add(filmUpdate.getCopy());
                } else {
                    listePrio.add(filmUpdate.getCopy());
                    listePrio.add(filmUpdate.getCopy());
                }
            }
            int nr = new Random().nextInt(listePrio.size());
            filmUpdate = listePrio.get(nr);
            ret = filmUpdate.arr[FilmUpdateServer.FILM_UPDATE_SERVER_URL_NR];
        }
        return ret;
    }

    public void listeSchreiben() {
        DatenFilmUpdateServer filmUpdate;
        Iterator<DatenFilmUpdateServer> it = this.iterator();
        while (it.hasNext()) {
            filmUpdate = it.next();
            System.out.println("Datum: " + filmUpdate.arr[FilmUpdateServer.FILM_UPDATE_SERVER_DATUM_NR]
                    + "   Uhrzeit: " + filmUpdate.arr[FilmUpdateServer.FILM_UPDATE_SERVER_ZEIT_NR]
                    + "   URL: " + filmUpdate.arr[FilmUpdateServer.FILM_UPDATE_SERVER_URL_NR]);
        }
    }
}
