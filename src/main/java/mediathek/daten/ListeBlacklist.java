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

import java.util.ArrayList;
import java.util.LinkedList;
import java.util.List;
import java.util.function.Predicate;
import java.util.stream.Collectors;
import java.util.stream.Stream;

@SuppressWarnings("serial")
public class ListeBlacklist extends LinkedList<DatenBlacklist> {
    /**
     * List for dynamic application of filters
     */
    final List<Predicate<DatenFilm>> filterList = new ArrayList<>();
    private long days = 0;
    private boolean doNotShowFutureFilms, doNotShowGeoBlockedFilms;
    private boolean blacklistIsActive;
    private long filmlaengeSoll = 0;
    private int nr = 0;

    public ListeBlacklist() {
    }

    /**
     * Add item without notifying registered listeners.
     *
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

    /**
     * Main filtering routine
     */
    @SuppressWarnings("unchecked")
    public synchronized void filterListe() {
        Daten daten = Daten.getInstance();
        final ListeFilme listeFilme = daten.getListeFilme();
        final ListeFilme listeRet = daten.getListeFilmeNachBlackList();

        loadCurrentFilterSettings();

        Duration.counterStart("Blacklist filtern");
        listeRet.clear();

        if (listeFilme != null) {
            listeRet.setMeta(listeFilme);

            forEach(entry -> {
                entry.toLower();
                entry.hasPattern();
            });
            listeRet.neueFilme = false;

            Stream<DatenFilm> initialStream = listeFilme.parallelStream()
                    //always filter for date
                    .filter(this::checkDate);

            filterList.clear();
            if (blacklistIsActive) {
                //add the filter predicates to the list
                if (doNotShowGeoBlockedFilms)
                    filterList.add(this::checkGeoBlockedFilm);
                if (doNotShowFutureFilms)
                    filterList.add(this::checkIfFilmIsInFuture);
                filterList.add(this::checkFilmLength);
                if (!isEmpty())
                    filterList.add(this::applyBlacklistFilters);

                for (Predicate<DatenFilm> pred : filterList) {
                    initialStream = initialStream.filter(pred);
                }
            }

            final List<DatenFilm> col = initialStream.collect(Collectors.toList());
            //are there new film entries?
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

    /**
     * Filterfunction for Abos dialog.
     * @param film item to te tested
     * @return true if item should be displayed.
     */
    public synchronized boolean checkBlackOkFilme_Downloads(DatenFilm film) {
        // hier werden die Filme für Downloads gesucht, Zeit ist "0"
        // ob die Blackliste dafür verwendet werden soll, ist schon geklärt
        loadCurrentFilterSettings();
        days = 0; // soll nur im TabFilme ausgewertet werden (Filter: Tage)
        blacklistIsActive = true; // Blacklist nur wenn "auch für Abos" geklickt, egal ob ein- oder ausgeschaltet

        return applyFiltersForAbos(film);
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
                final long max = 1000L * 60L * 60L * 24L * Daten.guiFilme.getFilterTage();
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
    }

    /**
     * Apply filters for ABOS if check box is active
     *
     * @param film item to be filtered
     * @return true if film shall be displayed
     */
    private boolean applyFiltersForAbos(DatenFilm film) {
        // erst mal den Filter Tage, kommt aus dem Filter und deswegen immer
        if (!checkDate(film)) {
            return false;
        }

        //===========================================
        // dann die Blacklist, nur wenn eingeschaltet
        if (!blacklistIsActive) {
            return true;
        }

        if (!checkGeoBlockedFilm(film))
            return false;

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

    /**
     * Check if film would be geoblocked for user
     *
     * @param film item to be checked
     * @return true if it is NOT blocked, false if it IS blocked
     */
    private boolean checkGeoBlockedFilm(DatenFilm film) {
        boolean result = true;
        if (!film.arr[DatenFilm.FILM_GEO].isEmpty() && !film.arr[DatenFilm.FILM_GEO].contains(MVConfig.get(MVConfig.Configs.SYSTEM_GEO_STANDORT))) {
            result = false;
        }

        return result;
    }

    /**
     * Apply filters to film.
     *
     * @param film item to be filtered
     * @return true if film can be displayed
     */
    private boolean applyBlacklistFilters(DatenFilm film) {
        for (DatenBlacklist entry : this) {
            if (Filter.filterAufFilmPruefen(entry.arr[DatenBlacklist.BLACKLIST_SENDER], entry.arr[DatenBlacklist.BLACKLIST_THEMA],
                    entry.patternTitle
                            ? new String[]{entry.arr[DatenBlacklist.BLACKLIST_TITEL]} : entry.arr[DatenBlacklist.BLACKLIST_TITEL].split(","),
                    entry.patternThema
                            ? new String[]{entry.arr[DatenBlacklist.BLACKLIST_THEMA_TITEL]} : entry.arr[DatenBlacklist.BLACKLIST_THEMA_TITEL].split(","),
                    new String[]{""}, 0, true /*min*/, film, true /*auch die Länge prüfen*/
            )) {
                return Boolean.parseBoolean(MVConfig.get(MVConfig.Configs.SYSTEM_BLACKLIST_IST_WHITELIST));
            }
        }
        return !Boolean.parseBoolean(MVConfig.get(MVConfig.Configs.SYSTEM_BLACKLIST_IST_WHITELIST));
    }

    /**
     * Check film based on date
     *
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
     *
     * @param film item to be checked.
     * @return true if it should be displayed.
     */
    private boolean checkIfFilmIsInFuture(DatenFilm film) {
        try {
            if (film.datumFilm.getTime() > System.currentTimeMillis()) {
                return false;
            }
        } catch (Exception ex) {
            Log.errorLog(696987123, ex);
        }
        return true;
    }

    /**
     * Filter based on film length.
     *
     * @param film item to check
     * @return true if film should be displayed
     */
    private boolean checkFilmLength(DatenFilm film) {
        return !(filmlaengeSoll != 0 && film.dauerL != 0 && filmlaengeSoll > film.dauerL);

    }

    private String getNr(int nr) {
        return String.valueOf(nr);
    }
}
