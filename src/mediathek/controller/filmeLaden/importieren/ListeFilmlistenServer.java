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

import java.util.Collections;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.ListIterator;

public class ListeFilmlistenServer extends LinkedList<DatenFilmlistenServer> {

    private final static int MAX_ALTER = 25; // entspricht 25x laden der update.xml auf sf.net
    
    public boolean addCheck(DatenFilmlistenServer d) {
        // neuer Eintag
        Iterator<DatenFilmlistenServer> it = this.iterator();
        while (it.hasNext()) {
            DatenFilmlistenServer df = it.next();
            if (df.compareTo(d) == 0) {
                df.arr[DatenFilmlistenServer.FILM_LISTEN_SERVER_ALTER_NR] = "0"; // Alter zurücksetzen
                return false;
            }
        }
        return super.add(d);
    }

    public void alteLoeschen() {
        // zu alte entfernen
        Iterator<DatenFilmlistenServer> it = this.iterator();
        while (it.hasNext()) {
            DatenFilmlistenServer df = it.next();
            df.incAlter();
            if (df.getAlter() > MAX_ALTER) {
                it.remove();
            }
        }
    }

    public void sort() {
        Collections.<DatenFilmlistenServer>sort(this);
        // und jetzt noch die Nummerierung in Ordnung brinegen
        Iterator<DatenFilmlistenServer> it = this.iterator();
        int i = 0;
        while (it.hasNext()) {
            it.next().arr[DatenFilmlistenServer.FILM_LISTEN_SERVER_NR_NR] = getNr(i++);
        }
    }

    public String[][] getTableObjectData() {
        DatenFilmlistenServer filmUpdate;
        String[][] object;
        this.sort();
        ListIterator<DatenFilmlistenServer> iterator = this.listIterator();
        object = new String[this.size()][DatenFilmlistenServer.FILM_LISTEN_SERVER_MAX_ELEM];
        int i = 0;
        while (iterator.hasNext()) {
            filmUpdate = iterator.next();
            object[i] = filmUpdate.arr;
            ++i;
        }
        return object;
    }

    private String getNr(int nr) {
        final int MAX_STELLEN = 3;
        final String FUELL_ZEICHEN = "0";
        String str = String.valueOf(nr);
        while (str.length() < MAX_STELLEN) {
            str = FUELL_ZEICHEN + str;
        }
        return str;
    }
}
