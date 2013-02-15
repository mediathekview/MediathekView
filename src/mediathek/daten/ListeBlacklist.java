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
package mediathek.daten;

import java.util.Date;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.ListIterator;
import mediathek.gui.GuiFilme;
import mediathek.tool.Konstanten;
import mediathek.tool.ListenerMediathekView;
import mediathek.tool.Log;

public class ListeBlacklist extends LinkedList<DatenBlacklist> {
    //Tags Blacklist

    private long tage = 0;
    private String[] filterTage = {};

    public static String[] getBlacklistTitel() {
        String[] ret = {};
        if (!Daten.system[Konstanten.SYSTEM_BLACKLIST_TITEL_NR].equals("")) {
            String s = Daten.system[Konstanten.SYSTEM_BLACKLIST_TITEL_NR];
            ret = s.split(";");
        }
        return ret;
    }

    @Override
    public boolean add(DatenBlacklist b) {
        boolean ret = super.add(b);
        ListenerMediathekView.notify(ListenerMediathekView.EREIGNIS_BLACKLIST_GEAENDERT, ListeBlacklist.class.getSimpleName());
        return ret;
    }

    @Override
    public boolean remove(Object b) {
        boolean ret = super.remove(b);
        ListenerMediathekView.notify(ListenerMediathekView.EREIGNIS_BLACKLIST_GEAENDERT, ListeBlacklist.class.getSimpleName());
        return ret;
    }

    @Override
    public DatenBlacklist remove(int idx) {
        DatenBlacklist ret = super.remove(idx);
        ListenerMediathekView.notify(ListenerMediathekView.EREIGNIS_BLACKLIST_GEAENDERT, ListeBlacklist.class.getSimpleName());
        return ret;
    }

    @Override
    public void clear() {
        super.clear();
        ListenerMediathekView.notify(ListenerMediathekView.EREIGNIS_BLACKLIST_GEAENDERT, ListeBlacklist.class.getSimpleName());
    }

    public Object[][] getObjectData() {
        Object[][] object;
        DatenBlacklist blacklist;
        int i = 0;
        ListIterator<DatenBlacklist> iterator = this.listIterator();
        object = new Object[this.size()][DatenBlacklist.BLACKLIST_MAX_ELEM];
        while (iterator.hasNext()) {
            blacklist = iterator.next();
            object[i] = blacklist.arr;
            ++i;
        }
        return object;
    }

    public ListeFilme filterListe(ListeFilme listeFilme) {
        try {
            if (Daten.system[Konstanten.SYSTEM_FILTER_TAGE_NR].equals("") || Daten.system[Konstanten.SYSTEM_FILTER_TAGE_NR].equals("0")) {
                tage = 0;
            } else {
                long max = 1000L * 60L * 60L * 24L * GuiFilme.COMBO_ZEIT_INT[Integer.parseInt(Daten.system[Konstanten.SYSTEM_FILTER_TAGE_NR])];
                tage = new Date().getTime() - max;
            }
        } catch (Exception ex) {
            tage = 0;
        }
        filterTage = getBlacklistTitel();
        for (int i = 0; i < filterTage.length; ++i) {
            filterTage[i] = filterTage[i].toLowerCase(); // erspart anschließend einen Schritt
        }
        ListeFilme listeRet = new ListeFilme();
        if (listeFilme != null) {
            DatenFilm film;
            listeRet.setMeta(listeFilme.metaDaten);
//            listeRet.setInfo(listeFilme.infos);
            Iterator<DatenFilm> it = listeFilme.iterator();
            while (it.hasNext()) {
                film = it.next();
                if (checkBlackOk(film)) {
                    listeRet.add(film);
                }
            }
        }
        return listeRet;
    }

    private boolean checkBlackOk(DatenFilm film) {
        // Eintrag suchen, true wenn Film NICHT in der Blacklist ist, also angezeigt wird!!
        // Alte Filme werden auch ausgewertet
        DatenBlacklist blacklist;
        Iterator<DatenBlacklist> it = this.iterator();
        if (filterTage.length > 0) {
            if (!checkTitel(film)) {
                return false;
            }
        }
        if (tage != 0) {
            if (!checkDate(film)) {
                return false;
            }
        }
        while (it.hasNext()) {
            blacklist = it.next();
            if (!blacklist.arr[DatenBlacklist.BLACKLIST_SENDER_NR].equals("") && !blacklist.arr[DatenBlacklist.BLACKLIST_THEMA_NR].equals("")) {
                // Sender und Thema müssen passen
                if (blacklist.arr[DatenBlacklist.BLACKLIST_SENDER_NR].equalsIgnoreCase(film.arr[DatenFilm.FILM_SENDER_NR])
                        && blacklist.arr[DatenBlacklist.BLACKLIST_THEMA_NR].equalsIgnoreCase(film.arr[DatenFilm.FILM_THEMA_NR])) {
                    // es gibt einen Eintrag in der Blacklist
                    return false;
                }
            } else if (!blacklist.arr[DatenBlacklist.BLACKLIST_SENDER_NR].equals("")) {
                // nur der Sender muss passen
                if (blacklist.arr[DatenBlacklist.BLACKLIST_SENDER_NR].equalsIgnoreCase(film.arr[DatenFilm.FILM_SENDER_NR])) {
                    // es gibt einen Eintrag in der Blacklist
                    return false;
                }
            } else {
                // nur das Thema muss passen
                if (blacklist.arr[DatenBlacklist.BLACKLIST_THEMA_NR].equalsIgnoreCase(film.arr[DatenFilm.FILM_THEMA_NR])) {
                    // es gibt einen Eintrag in der Blacklist
                    return false;
                }
            }
        }
        return true;
    }

    private boolean checkDate(DatenFilm film) {
        // true wenn der Film angezeigt werden kann!
        long d;
        try {
            d = film.datumFilm.getTime();
            //d = DatumZeit.getDatumForObject(film).getTime();
            if (d == 0 || d > tage) {
                return true;
            } else {
                return false;
            }
        } catch (Exception ex) {
            Log.fehlerMeldung(462558700, Log.FEHLER_ART_PROG, "ListeBlacklist.checkDate: ", ex);
        }
        return true;
    }

    private boolean checkTitel(DatenFilm film) {
        // true wenn der Film angezeigt werden kann!
        for (String s : filterTage) {
            if (film.arr[DatenFilm.FILM_TITEL_NR].toLowerCase().contains(s)) {
                return false;
            }
        }
        return true;
    }
}
