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
package mSearch.tool;

import mSearch.daten.DatenFilm;
import org.jetbrains.annotations.NotNull;

public class MSLong implements Comparable<MSLong> {

    private long l = 0;

    public MSLong(final long l) {
        this.l = l;
    }

    public MSLong(DatenFilm film) {
        if (film.arr[DatenFilm.FILM_GROESSE].equals("<1")) {
            film.arr[DatenFilm.FILM_GROESSE] = "1";
        }
        try {
            if (!film.arr[DatenFilm.FILM_GROESSE].isEmpty()) {
                l = Long.valueOf(film.arr[DatenFilm.FILM_GROESSE]);
            }
        } catch (NumberFormatException ex) {
            Log.errorLog(649891025, ex, "String: " + film.arr[DatenFilm.FILM_GROESSE]);
            l = 0;
        }
    }

    @Override
    public String toString() {
        return (l == 0) ? "" : Long.toString(l);
    }

    @Override
    public int compareTo(@NotNull MSLong other) {
        return (Long.compare(l, other.l));
    }
}
