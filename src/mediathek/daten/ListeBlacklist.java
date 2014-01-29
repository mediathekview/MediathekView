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
import mediathek.controller.Log;
import mediathek.tool.MVListeFilme;
import msearch.daten.DatenFilm;
import msearch.daten.ListeFilme;

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
        notifyBlack();
        return ret;
    }

    private void notifyBlack() {
        MVListeFilme.checkBlacklist();
        ListenerMediathekView.notify(ListenerMediathekView.EREIGNIS_BLACKLIST_GEAENDERT, ListeBlacklist.class.getSimpleName());
    }

    public boolean change(String idx, DatenBlacklist b) {
        boolean ret;
        remove(idx);
        b.arr[DatenBlacklist.BLACKLIST_NR_NR] = getNr(nr++);
        ret = super.add(b);
        notifyBlack();
        return ret;
    }

    @Override
    public boolean remove(Object b) {
        boolean ret = super.remove(b);
        notifyBlack();
        return ret;
    }

    @Override
    public DatenBlacklist remove(int idx) {
        DatenBlacklist ret = super.remove(idx);
        notifyBlack();
        return ret;
    }

    public DatenBlacklist remove(String idx) {
        DatenBlacklist bl;
        if ((bl = get(idx)) != null) {
            remove(bl);
        }
        notifyBlack();
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
        notifyBlack();
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

//    public boolean blockSender(String sender) {
//        // für die Senderliste im Panel Filter
//        // Sender die komplett geblockt sind, dort nicht anzeigen
//        ListIterator<DatenBlacklist> iterator = this.listIterator();
//        while (iterator.hasNext()) {
//            DatenBlacklist datenBlacklist = iterator.next();
//            if (datenBlacklist.arr[DatenBlacklist.BLACKLIST_SENDER_NR].equals(sender)
//                    && datenBlacklist.arr[DatenBlacklist.BLACKLIST_THEMA_NR].isEmpty()
//                    && datenBlacklist.arr[DatenBlacklist.BLACKLIST_TITEL_NR].isEmpty()
//                    && datenBlacklist.arr[DatenBlacklist.BLACKLIST_THEMA_TITEL_NR].isEmpty()) {
//                return true;
//            }
//        }
//        return false;
//    }
    public void filterListe(ListeFilme listeFilme, ListeFilme listeRet) {
        listeRet.clear();
        setFilter();
        if (listeFilme != null) {
            DatenFilm film;
            listeRet.setMeta(listeFilme);
            Iterator<DatenFilm> it = listeFilme.iterator();
            while (it.hasNext()) {
                film = it.next();
                if (checkFilm(film)) {
                    listeRet.add(film);
                    if (film.neuerFilm) {
                        listeRet.neueFilme = true;
                    }
                }
            }
            // Array mit Sendernamen/Themen füllen
            listeRet.themenLaden();
        }
    }

    public boolean checkBlackOkFilme_Downloads(DatenFilm film) {
        // true wenn Film angezeigt wird!!
        // hier werden die Filme für Downloads gesucht, Zeit ist "0"
        // ob die Blackliste dafür verwendet werden soll, ist schon geklärt
        setFilter();
        tage = 0; // soll nur im TabFilme ausgewertet werden (Filter: Tage)
        blacklistAusgeschaltet = Boolean.parseBoolean(Daten.mVConfig.get(Konstanten.SYSTEM_BLACKLIST_AUSGESCHALTET));
        zukunftNichtAnzeigen = Boolean.parseBoolean(Daten.mVConfig.get(Konstanten.SYSTEM_BLACKLIST_ZUKUNFT_NICHT_ANZEIGEN));
        jetzt = DatumZeit.Morgen_0_Uhr;
        return checkFilm(film);
    }

    private void setFilter() {
        try {
            if (Daten.mVConfig.get(Konstanten.SYSTEM_FILTER_TAGE).equals("") || Daten.mVConfig.get(Konstanten.SYSTEM_FILTER_TAGE).equals("0")) {
                tage = 0;
            } else {
                long max = 1000L * 60L * 60L * 24L * GuiFilme.COMBO_ZEIT_INT[Integer.parseInt(Daten.mVConfig.get(Konstanten.SYSTEM_FILTER_TAGE))];
                tage = new Date().getTime() - max;
            }
        } catch (Exception ex) {
            tage = 0;
        }
        try {
            filmlaengeSoll = Long.valueOf(Daten.mVConfig.get(Konstanten.SYSTEM_BLACKLIST_FILMLAENGE)) * 60; // Minuten
        } catch (Exception ex) {
            filmlaengeSoll = 0;
        }
        blacklistAusgeschaltet = Boolean.parseBoolean(Daten.mVConfig.get(Konstanten.SYSTEM_BLACKLIST_AUSGESCHALTET));
        zukunftNichtAnzeigen = Boolean.parseBoolean(Daten.mVConfig.get(Konstanten.SYSTEM_BLACKLIST_ZUKUNFT_NICHT_ANZEIGEN));
        jetzt = DatumZeit.Morgen_0_Uhr;
    }

    private boolean checkFilm(DatenFilm film) {
        // true wenn Film angezeigt wird!!
        DatenBlacklist blacklist;
        // erst mal den Filter Tage
        if (!checkDate(film)) {
            return false;
        }
        // dann die Blacklist, nur wenn eingeschaltet
        if (blacklistAusgeschaltet) {
            return true;
        }
        if (!checkZukunft(film)) {
            return false;
        }
        if (!checkFilmlaenge(film)) {
            // wegen der Möglichkeit "Whiteliste" muss das extra geprüft werden
            return false;
        }
        if (this.size() == 0) {
            return true;
        }
        Iterator<DatenBlacklist> it = this.iterator();
        while (it.hasNext()) {
            blacklist = it.next();
            if (Filter.filterAufFilmPruefen(blacklist.arr[DatenBlacklist.BLACKLIST_SENDER_NR], blacklist.arr[DatenBlacklist.BLACKLIST_THEMA_NR],
                    Filter.isPattern(blacklist.arr[DatenBlacklist.BLACKLIST_TITEL_NR])
                    ? new String[]{blacklist.arr[DatenBlacklist.BLACKLIST_TITEL_NR].toLowerCase()} : blacklist.arr[DatenBlacklist.BLACKLIST_TITEL_NR].toLowerCase().split(","),
                    Filter.isPattern(blacklist.arr[DatenBlacklist.BLACKLIST_THEMA_TITEL_NR])
                    ? new String[]{blacklist.arr[DatenBlacklist.BLACKLIST_THEMA_TITEL_NR].toLowerCase()} : blacklist.arr[DatenBlacklist.BLACKLIST_THEMA_TITEL_NR].toLowerCase().split(","),
                    new String[]{""}, 0, film, true /*auch die Länge prüfen*/)) {
                if (Boolean.parseBoolean(Daten.mVConfig.get(Konstanten.SYSTEM_BLACKLIST_IST_WHITELIST))) {
                    return true;
                } else {
                    return false;
                }
            }
        }
        if (Boolean.parseBoolean(Daten.mVConfig.get(Konstanten.SYSTEM_BLACKLIST_IST_WHITELIST))) {
            // nur anzeigen wenn ein Filter passt
            return false;
        } else {
            // kein Filter gefunden, also anzeigen
            return true;
        }
    }

    private boolean checkDate(DatenFilm film) {
        // true wenn der Film angezeigt werden kann!
        try {
            if (tage != 0) {
                if (film.datumFilm.getTime() != 0) {
                    if (film.datumFilm.getTime() < tage) {
                        return false;
                    }
                }
            }
        } catch (Exception ex) {
            Log.fehlerMeldung(462558700, Log.FEHLER_ART_PROG, "ListeBlacklist.checkDate: ", ex);
        }
        return true;
    }

    private boolean checkZukunft(DatenFilm film) {
        // true wenn der Film angezeigt werden kann!
        try {
            // Blacklist Zukunft
            if (zukunftNichtAnzeigen) {
                if (film.datumFilm.getTime() > jetzt) {
                    return false;
                }
            }
        } catch (Exception ex) {
            Log.fehlerMeldung(696987123, Log.FEHLER_ART_PROG, "ListeBlacklist.checkZukunft: ", ex);
        }
        return true;
    }

    private boolean checkFilmlaenge(DatenFilm film) {
        // true wenn der Film angezeigt werden kann!
        try {
            if (filmlaengeSoll != 0 && film.dauerL != 0 && filmlaengeSoll > film.dauerL) {
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
