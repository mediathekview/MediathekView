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
import mediathek.tool.DatumZeit;
import mediathek.tool.Filter;
import mediathek.tool.Konstanten;
import mediathek.tool.ListenerMediathekView;
import mediathek.tool.Log;

public class ListeBlacklist extends LinkedList<DatenBlacklist> {
    //Tags Blacklist

    private long tage = 0;
    private long jetzt;
    private boolean zukunftNichtAnzeigen;
    private boolean blacklistAusgeschaltet;
    private long filmlaengeSoll = 0;
    private int nr = 0;

    @Override
    public boolean add(DatenBlacklist b) {
        b.arr[DatenBlacklist.BLACKLIST_NR_NR] = getNr(nr++);
        boolean ret = super.add(b);
        ListenerMediathekView.notify(ListenerMediathekView.EREIGNIS_BLACKLIST_GEAENDERT, ListeBlacklist.class.getSimpleName());
        return ret;
    }

    public boolean change(String idx, DatenBlacklist b) {
        boolean ret;
        remove(idx);
        b.arr[DatenBlacklist.BLACKLIST_NR_NR] = getNr(nr++);
        ret = super.add(b);
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

    public DatenBlacklist remove(String idx) {
        DatenBlacklist bl;
        if ((bl = get(idx)) != null) {
            remove(bl);
        }
        ListenerMediathekView.notify(ListenerMediathekView.EREIGNIS_BLACKLIST_GEAENDERT, ListeBlacklist.class.getSimpleName());
        return bl;
    }

    @Override
    public DatenBlacklist get(int idx) {
        DatenBlacklist ret = super.get(idx);
        return ret;
    }

    public DatenBlacklist get(String nr) {
        for (DatenBlacklist b : this) {
            if (b.arr[DatenBlacklist.BLACKLIST_NR_NR].equals(nr)) {
                return b;
            }
        }
        return null;
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
        try {
            filmlaengeSoll = Long.valueOf(Daten.system[Konstanten.SYSTEM_BLACKLIST_FILMLAENGE_NR]) * 60; // Minuten
        } catch (Exception ex) {
            filmlaengeSoll = 0;
        }
        blacklistAusgeschaltet = Boolean.parseBoolean(Daten.system[Konstanten.SYSTEM_BLACKLIST_AUSGESCHALTET_NR]);
        zukunftNichtAnzeigen = !blacklistAusgeschaltet && Boolean.parseBoolean(Daten.system[Konstanten.SYSTEM_BLACKLIST_ZUKUNFT_NICHT_ANZEIGEN_NR]);
        jetzt = DatumZeit.getMorgen_0_Uhr();
        ListeFilme listeRet = new ListeFilme();
        if (listeFilme != null) {
            DatenFilm film;
            listeRet.setMeta(listeFilme.metaDaten);
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

    public boolean checkBlackOkFilm(DatenFilm film) {
        tage = 0;
        blacklistAusgeschaltet = Boolean.parseBoolean(Daten.system[Konstanten.SYSTEM_BLACKLIST_AUSGESCHALTET_NR]);
        zukunftNichtAnzeigen = !blacklistAusgeschaltet && Boolean.parseBoolean(Daten.system[Konstanten.SYSTEM_BLACKLIST_ZUKUNFT_NICHT_ANZEIGEN_NR]);
        jetzt = DatumZeit.getMorgen_0_Uhr();
        return checkBlackOk(film);
    }

    private boolean checkBlackOk(DatenFilm film) {
        // true wenn Film angezeigt wird!!
        //
        // Alte Filme ausgewerten
        DatenBlacklist blacklist;
        if (!checkDate(film)) {
            return false;
        }
        if (!checkFilmlaenge(film)) {
            return false;
        }
        if (this.size() == 0) {
            return true;
        }
        if (blacklistAusgeschaltet) {
            return true;
        }
        // nur wenn Blacklist nicht ausgeschaltet: Blacklist-Regeln prüfen
        Iterator<DatenBlacklist> it = this.iterator();
        while (it.hasNext()) {
            blacklist = it.next();
            // aboPruefen(String senderSuchen, String themaSuchen, boolean themaExakt, String textSuchen,
            //                     String imSender, String imThema, String imText) {
            if (Filter.filterAufAboPruefen(blacklist.arr[DatenBlacklist.BLACKLIST_SENDER_NR], blacklist.arr[DatenBlacklist.BLACKLIST_THEMA_NR],
                    blacklist.arr[DatenBlacklist.BLACKLIST_TITEL_NR], blacklist.arr[DatenBlacklist.BLACKLIST_THEMA_TITEL_NR],
                    film.arr[DatenFilm.FILM_SENDER_NR], film.arr[DatenFilm.FILM_THEMA_NR], film.arr[DatenFilm.FILM_TITEL_NR])) {
                if (Boolean.parseBoolean(Daten.system[Konstanten.SYSTEM_BLACKLIST_IST_WHITELIST_NR])) {
                    return true;
                } else {
                    return false;
                }
            }
        }
        if (Boolean.parseBoolean(Daten.system[Konstanten.SYSTEM_BLACKLIST_IST_WHITELIST_NR])) {
            // nur anzeigen wenn ein Filter passt
            return false;
        } else {
            // kein Filter gefunden, also anzeigen
            return true;
        }
    }

    private boolean checkDate(DatenFilm film) {
        // true wenn der Film angezeigt werden kann!
        long d;
        try {
            if (tage != 0 || zukunftNichtAnzeigen) {
                // "Zukunft" nur wenn Blacklist nicht ausgeschaltet
                d = film.datumFilm.getTime();
                // erst Filter Tage
                if (tage != 0) {
                    if (d != 0 && d < tage) {
                        return false;
                    }
                }
                // Blacklist Zukunft
                if (zukunftNichtAnzeigen) {
                    if (d > jetzt) {
                        return false;
                    }
                }
            }
        } catch (Exception ex) {
            Log.fehlerMeldung(462558700, Log.FEHLER_ART_PROG, "ListeBlacklist.checkDate: ", ex);
        }
        return true;
    }

    private boolean checkFilmlaenge(DatenFilm film) {
        // true wenn der Film angezeigt werden kann!
        try {
            if (filmlaengeSoll != 0 && film.durationL != 0 && filmlaengeSoll > film.durationL) {
                return false;
            }
        } catch (Exception ex) {
            Log.fehlerMeldung(912304894, Log.FEHLER_ART_PROG, "ListeBlacklist.checkFilmlänge: ", ex);
        }
        return true;
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
