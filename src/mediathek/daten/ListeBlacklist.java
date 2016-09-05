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
import java.util.List;
import java.util.stream.Collectors;
import mSearch.daten.DatenFilm;
import mSearch.daten.ListeFilme;
import mSearch.tool.Duration;
import mSearch.tool.Listener;
import mSearch.tool.Log;
import mediathek.config.Daten;
import mediathek.config.MVConfig;
import mediathek.tool.Filter;

public class ListeBlacklist extends LinkedList<DatenBlacklist> {

    private long tage = 0;
    private long jetzt;
    private boolean zukunftNichtAnzeigen, geoNichtAnzeigen;
    private boolean blacklistOn;
    private long filmlaengeSoll = 0;
    private int nr = 0;

    public ListeBlacklist() {
    }

    public synchronized boolean addProgStart(DatenBlacklist b) {
        //nur beim Programmstart!!, wird nicht gemeldet
        b.arr[DatenBlacklist.BLACKLIST_NR] = getNr(nr++);
        return super.add(b);
    }

    @Override
    public synchronized boolean add(DatenBlacklist b) {
        b.arr[DatenBlacklist.BLACKLIST_NR] = getNr(nr++);
        boolean ret = super.add(b);
        notifyBlack();
        return ret;
    }

    @Override
    public synchronized boolean remove(Object b) {
        boolean ret = super.remove(b);
        notifyBlack();
        return ret;
    }

    @Override
    public synchronized DatenBlacklist remove(int idx) {
        DatenBlacklist ret = super.remove(idx);
        notifyBlack();
        return ret;
    }

    public synchronized DatenBlacklist remove(String idx) {
        DatenBlacklist bl;
        if ((bl = get(idx)) != null) {
            remove(bl);
        }
        notifyBlack();
        return bl;
    }

    @Override
    public synchronized DatenBlacklist get(int idx) {
        return super.get(idx);
    }

    public synchronized DatenBlacklist get(String nr) {
        for (DatenBlacklist b : this) {
            if (b.arr[DatenBlacklist.BLACKLIST_NR].equals(nr)) {
                return b;
            }
        }
        return null;
    }

    @Override
    public synchronized void clear() {
        super.clear();
        notifyBlack();
    }

    public synchronized Object[][] getObjectData() {
        Object[][] object;
        DatenBlacklist blacklist;
        int i = 0;
        Iterator<DatenBlacklist> iterator = this.iterator();
        object = new Object[this.size()][DatenBlacklist.MAX_ELEM];
        while (iterator.hasNext()) {
            blacklist = iterator.next();
            object[i] = blacklist.arr;
            ++i;
        }
        return object;
    }

    public synchronized void filterListe() {
        filterListe(Daten.listeFilme, Daten.listeFilmeNachBlackList);
    }

    public synchronized void filterListe(ListeFilme listeFilme, ListeFilme listeRet) {
        Duration.counterStart("Blacklist filtern");
        listeRet.clear();
        setFilter();
        if (listeFilme != null) {
            listeRet.setMeta(listeFilme);

            this.stream().forEach(entry -> {
                entry.toLower();
                entry.hasPattern();
            });
            listeRet.neueFilme = false;

            final List<DatenFilm> col = listeFilme.parallelStream().filter(this::checkFilm_).collect(Collectors.toList());
            col.parallelStream().filter(DatenFilm::isNew).findFirst().ifPresent(ignored -> listeRet.neueFilme = true);
            listeRet.addAll(col);
            col.clear();

            // Array mit Sendernamen/Themen füllen
            listeRet.themenLaden();
        }
        Duration.counterStop("Blacklist filtern");
    }

    public synchronized boolean checkBlackOkFilme_Downloads(DatenFilm film) {
        // true wenn Film angezeigt wird!!
        // hier werden die Filme für Downloads gesucht, Zeit ist "0"
        // ob die Blackliste dafür verwendet werden soll, ist schon geklärt
        setFilter();
        tage = 0; // soll nur im TabFilme ausgewertet werden (Filter: Tage)
        blacklistOn = true; // Blacklist nur wenn "auch für Abos" geklickt, egal ob ein- oder ausgeschaltet
        zukunftNichtAnzeigen = Boolean.parseBoolean(MVConfig.get(MVConfig.SYSTEM_BLACKLIST_ZUKUNFT_NICHT_ANZEIGEN));
        geoNichtAnzeigen = Boolean.parseBoolean(MVConfig.get(MVConfig.SYSTEM_BLACKLIST_GEO_NICHT_ANZEIGEN));
        jetzt = getZeitZukunftBlacklist();
        return checkFilm(film);
    }

    public synchronized void notifyBlack() {
        filterListe();
        Listener.notify(Listener.EREIGNIS_BLACKLIST_GEAENDERT, ListeBlacklist.class.getSimpleName());
    }

    private void setFilter() {
        try {
            //if (MVConfig.get(MVConfig.SYSTEM_FILTER_TAGE).equals("") || MVConfig.get(MVConfig.SYSTEM_FILTER_TAGE).equals("0")) {
            if (Daten.guiFilme.getFilterTage() == 0) {
                tage = 0;
            } else {
                long max = 1000L * 60L * 60L * 24L * Daten.guiFilme.getFilterTage();
                tage = new Date().getTime() - max;
            }
        } catch (Exception ex) {
            tage = 0;
        }
        try {
            filmlaengeSoll = Long.valueOf(MVConfig.get(MVConfig.SYSTEM_BLACKLIST_FILMLAENGE)) * 60; // Minuten
        } catch (Exception ex) {
            filmlaengeSoll = 0;
        }
        blacklistOn = Boolean.parseBoolean(MVConfig.get(MVConfig.SYSTEM_BLACKLIST_ON));
        zukunftNichtAnzeigen = Boolean.parseBoolean(MVConfig.get(MVConfig.SYSTEM_BLACKLIST_ZUKUNFT_NICHT_ANZEIGEN));
        geoNichtAnzeigen = Boolean.parseBoolean(MVConfig.get(MVConfig.SYSTEM_BLACKLIST_GEO_NICHT_ANZEIGEN));
        jetzt = getZeitZukunftBlacklist();
    }

    private static long getZeitZukunftBlacklist() {
        return new Date().getTime();
    }

    private boolean checkFilm(DatenFilm film) {
        // true wenn Film angezeigt wird!!
        // erst mal den Filter Tage, kommt aus dem Filter und deswegen immer
        if (!checkDate(film)) {
            return false;
        }

        //===========================================
        // dann die Blacklist, nur wenn eingeschaltet
        if (!blacklistOn) {
            return true;
        }
        // keine Geo-gesperrten Filme
        if (geoNichtAnzeigen) {
            if (!film.arr[DatenFilm.FILM_GEO].isEmpty() && !film.arr[DatenFilm.FILM_GEO].contains(MVConfig.get(MVConfig.SYSTEM_GEO_STANDORT))) {
                return false;
            }
        }
        if (!checkZukunft(film)) {
            return false;
        }
        if (!checkFilmlaenge(film)) {
            // wegen der Möglichkeit "Whiteliste" muss das extra geprüft werden
            return false;
        }
        if (this.isEmpty()) {
            return true;
        }
        for (DatenBlacklist blacklistEntry : this) {
            if (Filter.filterAufFilmPruefen(blacklistEntry.arr[DatenBlacklist.BLACKLIST_SENDER], blacklistEntry.arr[DatenBlacklist.BLACKLIST_THEMA],
                    Filter.isPattern(blacklistEntry.arr[DatenBlacklist.BLACKLIST_TITEL])
                    ? new String[]{blacklistEntry.arr[DatenBlacklist.BLACKLIST_TITEL]} : blacklistEntry.arr[DatenBlacklist.BLACKLIST_TITEL].toLowerCase().split(","),
                    Filter.isPattern(blacklistEntry.arr[DatenBlacklist.BLACKLIST_THEMA_TITEL])
                    ? new String[]{blacklistEntry.arr[DatenBlacklist.BLACKLIST_THEMA_TITEL]} : blacklistEntry.arr[DatenBlacklist.BLACKLIST_THEMA_TITEL].toLowerCase().split(","),
                    new String[]{""}, 0, film, true /*auch die Länge prüfen*/
            )) {
                return Boolean.parseBoolean(MVConfig.get(MVConfig.SYSTEM_BLACKLIST_IST_WHITELIST));
            }
        }
        return !Boolean.parseBoolean(MVConfig.get(MVConfig.SYSTEM_BLACKLIST_IST_WHITELIST));
    }

    private boolean checkFilm_(DatenFilm film) {
        // true wenn Film angezeigt wird!!
        // erst mal den Filter Tage, kommt aus dem Filter und deswegen immer
        if (!checkDate(film)) {
            return false;
        }

        //===========================================
        // dann die Blacklist, nur wenn eingeschaltet
        if (!blacklistOn) {
            return true;
        }
        // keine Geo-gesperrten Filme
        if (geoNichtAnzeigen) {
            if (!film.arr[DatenFilm.FILM_GEO].isEmpty() && !film.arr[DatenFilm.FILM_GEO].contains(MVConfig.get(MVConfig.SYSTEM_GEO_STANDORT))) {
                return false;
            }
        }
        if (!checkZukunft(film)) {
            return false;
        }
        if (!checkFilmlaenge(film)) {
            // wegen der Möglichkeit "Whiteliste" muss das extra geprüft werden
            return false;
        }
        if (this.isEmpty()) {
            return true;
        }
        for (DatenBlacklist blacklistEntry : this) {
            if (Filter.filterAufFilmPruefen(blacklistEntry.arr[DatenBlacklist.BLACKLIST_SENDER], blacklistEntry.arr[DatenBlacklist.BLACKLIST_THEMA],
                    blacklistEntry.patternTitle
                            ? new String[]{blacklistEntry.arr[DatenBlacklist.BLACKLIST_TITEL]} : blacklistEntry.arr[DatenBlacklist.BLACKLIST_TITEL].split(","),
                    blacklistEntry.patternThema
                            ? new String[]{blacklistEntry.arr[DatenBlacklist.BLACKLIST_THEMA_TITEL]} : blacklistEntry.arr[DatenBlacklist.BLACKLIST_THEMA_TITEL].split(","),
                    new String[]{""}, 0, film, true /*auch die Länge prüfen*/
            )) {
                return Boolean.parseBoolean(MVConfig.get(MVConfig.SYSTEM_BLACKLIST_IST_WHITELIST));
            }
        }
        return !Boolean.parseBoolean(MVConfig.get(MVConfig.SYSTEM_BLACKLIST_IST_WHITELIST));
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
            Log.errorLog(462558700, ex);
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
            Log.errorLog(696987123, ex);
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
            Log.errorLog(912304894, ex);
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
