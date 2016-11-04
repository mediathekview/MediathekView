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

import mSearch.daten.DatenFilm;
import mSearch.daten.ListeFilme;
import mSearch.tool.Duration;
import mSearch.tool.Listener;
import mSearch.tool.Log;
import mediathek.config.Daten;
import mediathek.config.MVConfig;
import mediathek.tool.Filter;

import java.util.LinkedList;
import java.util.List;
import java.util.stream.Collectors;

public class ListeBlacklist extends LinkedList<DatenBlacklist> {

    private long days = 0;
    private long now;
    private boolean doNotShowFutureFilms, doNotShowGeoBlockedFilms;
    private boolean blacklistIsActive;
    private long filmlaengeSoll = 0;
    private int nr = 0;

    public ListeBlacklist() {
    }

    /**
     * Add item without notifying registered listeners.
     * @param b {@link DatenBlacklist} item.
     * @return true if collection is changed
     */
    public synchronized boolean addWithoutNotification(DatenBlacklist b) {
        b.arr[DatenBlacklist.BLACKLIST_NR] = getNr(nr++);
        return super.add(b);
    }

    @Override
    public synchronized boolean add(DatenBlacklist b) {
        b.arr[DatenBlacklist.BLACKLIST_NR] = getNr(nr++);
        boolean ret = super.add(b);
        filterListAndNotifyListeners();
        return ret;
    }

    @Override
    public synchronized boolean remove(Object b) {
        boolean ret = super.remove(b);
        filterListAndNotifyListeners();
        return ret;
    }

    @Override
    public synchronized DatenBlacklist remove(int idx) {
        DatenBlacklist ret = super.remove(idx);
        filterListAndNotifyListeners();
        return ret;
    }

    public synchronized DatenBlacklist remove(String idx) {
        DatenBlacklist bl;
        if ((bl = get(idx)) != null) {
            remove(bl);
        }
        filterListAndNotifyListeners();
        return bl;
    }

    @Override
    public synchronized DatenBlacklist get(int idx) {
        return super.get(idx);
    }

    /**
     * Return the element at the specified {@link String} position.
     *
     * @param strIndex Index string of the specified element
     * @return the specified element in the list
     */
    public synchronized DatenBlacklist get(final String strIndex) {
        return stream()
                .filter(e -> e.arr[DatenBlacklist.BLACKLIST_NR].equals(strIndex))
                .findFirst()
                .orElse(null);

    }

    @Override
    public synchronized void clear() {
        super.clear();
        filterListAndNotifyListeners();
    }

    public synchronized Object[][] getObjectData() {
        Object[][] object = new Object[size()][DatenBlacklist.MAX_ELEM];

        int i = 0;
        for (DatenBlacklist blacklist : this) {
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
        loadCurrentFilterSettings();
        if (listeFilme != null) {
            listeRet.setMeta(listeFilme);

            forEach(entry -> {
                entry.toLower();
                entry.hasPattern();
            });
            listeRet.neueFilme = false;

            final List<DatenFilm> col = listeFilme.parallelStream()
                    .filter(this::checkFilm_)
                    .collect(Collectors.toList());
            col.parallelStream()
                    .filter(DatenFilm::isNew)
                    .findFirst()
                    .ifPresent(ignored -> listeRet.neueFilme = true);

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
        loadCurrentFilterSettings();
        days = 0; // soll nur im TabFilme ausgewertet werden (Filter: Tage)
        blacklistIsActive = true; // Blacklist nur wenn "auch für Abos" geklickt, egal ob ein- oder ausgeschaltet
        doNotShowFutureFilms = Boolean.parseBoolean(MVConfig.get(MVConfig.Configs.SYSTEM_BLACKLIST_ZUKUNFT_NICHT_ANZEIGEN));
        doNotShowGeoBlockedFilms = Boolean.parseBoolean(MVConfig.get(MVConfig.Configs.SYSTEM_BLACKLIST_GEO_NICHT_ANZEIGEN));
        now = System.currentTimeMillis();
        return checkFilm(film);
    }

    /**
     * Filter the list and notify all registered listeners.
     */
    public synchronized void filterListAndNotifyListeners() {
        filterListe();
        Listener.notify(Listener.EREIGNIS_BLACKLIST_GEAENDERT, ListeBlacklist.class.getSimpleName());
    }

    /**
     * Load current filter settings from Config
     */
    private void loadCurrentFilterSettings() {
        try {
            //if (MVConfig.get(MVConfig.Configs.SYSTEM_FILTER_TAGE).equals("") || MVConfig.get(MVConfig.Configs.SYSTEM_FILTER_TAGE).equals("0")) {
            if (Daten.guiFilme.getFilterTage() == 0) {
                days = 0;
            } else {
                long max = 1000L * 60L * 60L * 24L * Daten.guiFilme.getFilterTage();
                days = System.currentTimeMillis() - max;
            }
        } catch (Exception ex) {
            days = 0;
        }
        try {
            filmlaengeSoll = Long.valueOf(MVConfig.get(MVConfig.Configs.SYSTEM_BLACKLIST_FILMLAENGE)) * 60; // Minuten
        } catch (Exception ex) {
            filmlaengeSoll = 0;
        }
        blacklistIsActive = Boolean.parseBoolean(MVConfig.get(MVConfig.Configs.SYSTEM_BLACKLIST_ON));
        doNotShowFutureFilms = Boolean.parseBoolean(MVConfig.get(MVConfig.Configs.SYSTEM_BLACKLIST_ZUKUNFT_NICHT_ANZEIGEN));
        doNotShowGeoBlockedFilms = Boolean.parseBoolean(MVConfig.get(MVConfig.Configs.SYSTEM_BLACKLIST_GEO_NICHT_ANZEIGEN));
        now = System.currentTimeMillis();
    }

    private boolean checkFilm(DatenFilm film) {
        // true wenn Film angezeigt wird!!
        // erst mal den Filter Tage, kommt aus dem Filter und deswegen immer
        if (!checkDate(film)) {
            return false;
        }

        //===========================================
        // dann die Blacklist, nur wenn eingeschaltet
        if (!blacklistIsActive) {
            return true;
        }
        // keine Geo-gesperrten Filme
        if (doNotShowGeoBlockedFilms) {
            if (!film.arr[DatenFilm.FILM_GEO].isEmpty() && !film.arr[DatenFilm.FILM_GEO].contains(MVConfig.get(MVConfig.Configs.SYSTEM_GEO_STANDORT))) {
                return false;
            }
        }
        if (!checkIfFilmIsInFuture(film)) {
            return false;
        }
        if (!checkFilmLength(film)) {
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
                    new String[]{""}, 0, true /*min*/, film, true /*auch die Länge prüfen*/
            )) {
                return Boolean.parseBoolean(MVConfig.get(MVConfig.Configs.SYSTEM_BLACKLIST_IST_WHITELIST));
            }
        }
        return !Boolean.parseBoolean(MVConfig.get(MVConfig.Configs.SYSTEM_BLACKLIST_IST_WHITELIST));
    }

    private boolean checkFilm_(DatenFilm film) {
        // true wenn Film angezeigt wird!!
        // erst mal den Filter Tage, kommt aus dem Filter und deswegen immer
        if (!checkDate(film)) {
            return false;
        }

        //===========================================
        // dann die Blacklist, nur wenn eingeschaltet
        if (!blacklistIsActive) {
            return true;
        }
        // keine Geo-gesperrten Filme
        if (doNotShowGeoBlockedFilms) {
            if (!film.arr[DatenFilm.FILM_GEO].isEmpty() && !film.arr[DatenFilm.FILM_GEO].contains(MVConfig.get(MVConfig.Configs.SYSTEM_GEO_STANDORT))) {
                return false;
            }
        }
        if (!checkIfFilmIsInFuture(film)) {
            return false;
        }
        if (!checkFilmLength(film)) {
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
                    new String[]{""}, 0, true /*min*/, film, true /*auch die Länge prüfen*/
            )) {
                return Boolean.parseBoolean(MVConfig.get(MVConfig.Configs.SYSTEM_BLACKLIST_IST_WHITELIST));
            }
        }
        return !Boolean.parseBoolean(MVConfig.get(MVConfig.Configs.SYSTEM_BLACKLIST_IST_WHITELIST));
    }

    /**
     * Check film based on date
     * @param film item to be checked
     * @return true if film can be displayed
     */
    private boolean checkDate(DatenFilm film) {
        boolean result = true;

        if (days != 0) {
            final long filmTime = film.datumFilm.getTime();
            if (filmTime != 0 && filmTime < days)
                result = false;
        }

        return result;
    }

    /**
     * Check if a future film should be displayed.
     * @param film item to be checked.
     * @return true if it should be displayed.
     */
    private boolean checkIfFilmIsInFuture(DatenFilm film) {
        // true wenn der Film angezeigt werden kann!
        try {
            // Blacklist Zukunft
            if (doNotShowFutureFilms) {
                if (film.datumFilm.getTime() > now) {
                    return false;
                }
            }
        } catch (Exception ex) {
            Log.errorLog(696987123, ex);
        }
        return true;
    }

    /**
     * Filter based on film length.
     * @param film item to check
     * @return true if film should be displayed
     */
    private boolean checkFilmLength(DatenFilm film) {
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
