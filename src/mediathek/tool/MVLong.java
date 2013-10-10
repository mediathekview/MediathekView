/*
 *   MediathekView
 *   Copyright (C) 2013 W. Xaver
 *   W.Xaver[at]googlemail.com
 *   http://zdfmediathk.sourceforge.net/
 *
 *   This program is free software: you can redistribute it and/or modify
 *   it under the terms of the GNU General Public License as published by
 *   the Free Software Foundation, either version 3 of the License, or
 *   any later version.
 *
 *   This program is distributed in the hope that it will be useful,
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 *   GNU General Public License for more details.
 *
 *   You should have received a copy of the GNU General Public License
 *   along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package mediathek.tool;

import msearch.daten.DatenFilm;

public class MVLong implements Comparable<MVLong> {

    Long l = 0L;
    String s = "";

    public MVLong(long ll) {
        l = new Long(ll);
        if (l != 0) {
            s = l.toString();
        }
    }

    public MVLong(String s) {
        setString(s);
    }

    public MVLong(DatenFilm film) {
        if (film.arr[DatenFilm.FILM_GROESSE_NR].equals("<1")) {
            film.arr[DatenFilm.FILM_GROESSE_NR] = "1";
        }
        setString(film.arr[DatenFilm.FILM_GROESSE_NR]);
    }

    @Override
    public String toString() {
        return s;
    }

    @Override
    public int compareTo(MVLong ll) {
        return (l.compareTo(ll.l));
    }

    private void setString(String ss) {
        if (!ss.isEmpty()) {
            try {
                l = Long.valueOf(ss);
                s = l.toString();
            } catch (Exception ex) {
                Log.fehlerMeldung(978745320, Log.FEHLER_ART_MREADER, MVLong.class.getName(), ex, "String: " + ss);
                l = 0L;
                s = "";
            }
        }
    }
}
