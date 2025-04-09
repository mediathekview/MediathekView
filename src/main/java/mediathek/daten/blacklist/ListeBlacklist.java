package mediathek.daten.blacklist;

import mediathek.config.Daten;
import mediathek.config.MVConfig;
import mediathek.daten.Country;
import mediathek.daten.DatenFilm;
import mediathek.daten.IndexedFilmList;
import mediathek.daten.ListeFilme;
import mediathek.gui.messages.BlacklistChangedEvent;
import mediathek.gui.tabs.tab_film.filter.zeitraum.ZeitraumSpinner;
import mediathek.mainwindow.MediathekGui;
import mediathek.tool.ApplicationConfiguration;
import mediathek.tool.Filter;
import mediathek.tool.MessageBus;
import org.jetbrains.annotations.NotNull;

import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.TimeUnit;
import java.util.function.Predicate;

public class ListeBlacklist extends ArrayList<BlacklistRule> {

    private final GeoblockingPredicate geoblockingPredicate = new GeoblockingPredicate();
    /**
     * This specifies the lower boundary for all films to be shown or not.
     * Content is num of days converted to milliseconds from UNIX start.
     */
    private long days_lower_boundary;
    private boolean doNotShowFutureFilms, doNotShowGeoBlockedFilms;
    private boolean blacklistIsActive;
    /**
     * The minimum length in minutes a film should have.
     * Configuration in Settings/Blacklist panel.
     */
    private long minimumFilmLength;

    /**
     * Add item without notifying registered listeners.
     *
     * @param b {@link BlacklistRule} item.
     */
    public synchronized void addWithoutNotification(BlacklistRule b) {
        super.add(b);
    }

    @Override
    public synchronized boolean add(BlacklistRule b) {
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

    /**
     * Remove a list of rules and filter after all objects have been removed.
     * @param ruleList a list of objects that need to be deleted.
     */
    public synchronized void remove(List<BlacklistRule> ruleList) {
        for (var rule : ruleList) {
            super.remove(rule);
        }
        filterListAndNotifyListeners();
    }

    @Override
    public synchronized BlacklistRule remove(int idx) {
        BlacklistRule ret = super.remove(idx);
        filterListAndNotifyListeners();
        return ret;
    }

    @Override
    public synchronized BlacklistRule get(int idx) {
        return super.get(idx);
    }

    @Override
    public synchronized void clear() {
        super.clear();
        filterListAndNotifyListeners();
    }

    /**
     * Main filtering routine
     */
    public synchronized void filterListe() {
        final Daten daten = Daten.getInstance();
        final ListeFilme completeFilmList = daten.getListeFilme();
        final ListeFilme filteredList = daten.getListeFilmeNachBlackList();

        filteredList.clear();

        loadCurrentFilterSettings();

        if (completeFilmList != null && !completeFilmList.isEmpty()) { // Check if there are any movies
            filteredList.setMetaData(completeFilmList.getMetaData());

            this.parallelStream().forEach(entry -> {
                entry.convertToLowerCase();
                entry.checkPatterns();
            });


            var stream = completeFilmList.parallelStream();

            //TODO add config dialog setting
            final var config = ApplicationConfiguration.getConfiguration();
            //if we don't evaluate there will be no chance to filter here...
            var evaluateDuplicates = config.getBoolean(ApplicationConfiguration.FILM_EVALUATE_DUPLICATES, true);
            if (evaluateDuplicates) {
                var filterBlacklistDuplicates = config.getBoolean(ApplicationConfiguration.BLACKLIST_FILTER_DUPLICATES, false);
                if (filterBlacklistDuplicates) {
                    stream = stream.filter(film -> !film.isDuplicate());
                }
            }

            stream.filter(createPredicate()).forEachOrdered(filteredList::add);

            setupNewEntries();
        }
    }

    /**
     * Setup dynamically the list of filter to be applied to blacklist film list
     *
     * @return The reduced filter predicates.
     */
    private Predicate<DatenFilm> createPredicate() {
        final List<Predicate<DatenFilm>> filterList = new ArrayList<>();
        // we must keep it for the "old-style search. for lucene it is useless
        if (!(Daten.getInstance().getListeFilmeNachBlackList() instanceof IndexedFilmList)) {
            if (days_lower_boundary != 0)
                filterList.add(this::checkDate);
        }

        if (blacklistIsActive) {
            if (doNotShowGeoBlockedFilms) {
                filterList.add(geoblockingPredicate);
            }
            if (doNotShowFutureFilms) {
                filterList.add(this::checkIfFilmIsInFuture);
            }
            if (minimumFilmLength != 0) {
                filterList.add(this::checkFilmLength);
            }

            //add the filter predicates to the list
            if (!isEmpty()) {
                ApplyBlacklistFilterPredicate predicate = new ApplyBlacklistFilterPredicate(this);
                filterList.add(predicate);
            }

        }

        final Predicate<DatenFilm> pred = filterList.stream().reduce(Predicate::and).orElse(f -> true);
        filterList.clear();

        return pred;
    }

    /**
     * Detect if there are new entried in the blacklist filtered film list.
     */
    private void setupNewEntries() {
        //are there new film entries?
        final Daten daten = Daten.getInstance();
        daten.getListeFilmeNachBlackList().stream()
                .filter(DatenFilm::isNew)
                .findAny()
                .ifPresent(ignored -> daten.getListeFilmeNachBlackList().neueFilme = true);
    }

    /**
     * Filterfunction for Abos dialog.
     *
     * @param film item to te tested
     * @return true if item should be displayed.
     */
    public synchronized boolean checkBlackOkFilme_Downloads(DatenFilm film) {
        // hier werden die Filme für Downloads gesucht, Zeit ist "0"
        // ob die Blackliste dafür verwendet werden soll, ist schon geklärt
        loadCurrentFilterSettings();
        days_lower_boundary = 0; // soll nur im TabFilme ausgewertet werden (Filter: Tage)
        blacklistIsActive = true; // Blacklist nur wenn "auch für Abos" geklickt, egal ob ein- oder ausgeschaltet

        return applyFiltersForAbos(film);
    }

    /**
     * Filter the list and notify all registered listeners.
     */
    public synchronized void filterListAndNotifyListeners() {
        filterListe();
        MessageBus.getMessageBus().publishAsync(new BlacklistChangedEvent());
    }

    private void calculateZeitraumBoundaries() {
        try {
            var strZeitraum = MediathekGui.ui().tabFilme.getFilterConfiguration().getZeitraum();
            if (strZeitraum.equalsIgnoreCase(ZeitraumSpinner.UNLIMITED_VALUE))
                days_lower_boundary = 0;
            else {
                var days_ms = TimeUnit.MILLISECONDS.convert(Long.parseLong(strZeitraum), TimeUnit.DAYS);
                days_lower_boundary = System.currentTimeMillis() - days_ms;
            }
        } catch (Exception ex) {
            days_lower_boundary = 0;
        }
    }

    private void calculateMinimumFilmLength() {
        try {
            var filmlength_minutes = Long.parseLong(MVConfig.get(MVConfig.Configs.SYSTEM_BLACKLIST_FILMLAENGE));
            minimumFilmLength = filmlength_minutes * 60; // convert to seconds
        } catch (Exception ex) {
            minimumFilmLength = 0;
        }
    }

    /**
     * Load current filter settings from Config
     */
    private void loadCurrentFilterSettings() {
        calculateZeitraumBoundaries();
        calculateMinimumFilmLength();

        var config = ApplicationConfiguration.getConfiguration();
        blacklistIsActive = config.getBoolean(ApplicationConfiguration.BLACKLIST_IS_ON, false);
        doNotShowFutureFilms = Boolean.parseBoolean(MVConfig.get(MVConfig.Configs.SYSTEM_BLACKLIST_ZUKUNFT_NICHT_ANZEIGEN));
        doNotShowGeoBlockedFilms = ApplicationConfiguration.getInstance().getBlacklistDoNotShowGeoblockedFilms();

        geoblockingPredicate.updateLocation();
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
        if (doNotShowGeoBlockedFilms && !geoblockingPredicate.test(film)) {
            return false;
        }
        if (doNotShowFutureFilms && !checkIfFilmIsInFuture(film)) {
            return false;
        }

        if (minimumFilmLength != 0) {
            if (!checkFilmLength(film)) {
                // wegen der Möglichkeit "Whiteliste" muss das extra geprüft werden
                return false;
            }
        }
        if (this.isEmpty()) {
            return true;
        }

        final boolean bl_is_whitelist = Boolean.parseBoolean(MVConfig.get(MVConfig.Configs.SYSTEM_BLACKLIST_IST_WHITELIST));
        for (BlacklistRule rule : this) {
            if (Filter.filterAufFilmPruefenWithLength(rule.getSender(),
                    rule.getThema(),
                    makePattern(rule.getTitel()),
                    makePattern(rule.getThema_titel()),
                    EMPTY_STRING_ARRAY, 0, true, film, true)) {
                return bl_is_whitelist;
            }
        }
        return !bl_is_whitelist;
    }
    final static private String[] EMPTY_STRING_ARRAY = {""};

    private String[] makePattern(String input) {
        return Filter.isPattern(input) ? new String[]{input} : input.toLowerCase().split(",");
    }

    /**
     * Check film based on date
     *
     * @param film item to be checked
     * @return true if film can be displayed
     */
    private boolean checkDate(@NotNull DatenFilm film) {
        // always show livestreams
        if (film.isLivestream())
            return true;

        if (days_lower_boundary != 0) {
            final long filmTime = film.getDatumFilm().getTime();
            return filmTime == 0 || filmTime >= days_lower_boundary;
        }

        return true;
    }

    /**
     * Check if a future film should be displayed.
     *
     * @param film item to be checked.
     * @return true if it should be displayed.
     */
    private boolean checkIfFilmIsInFuture(@NotNull DatenFilm film) {
        return film.getDatumFilm().getTime() <= System.currentTimeMillis();
    }

    /**
     * Filter based on film length.
     *
     * @param film item to check
     * @return true if film should be displayed
     */
    private boolean checkFilmLength(@NotNull DatenFilm film) {
        var filmLength = film.getFilmLength();
        return !(filmLength != 0 && minimumFilmLength > filmLength);
    }

    static class GeoblockingPredicate implements Predicate<DatenFilm> {
        /**
         * Stores the current user´s location. Can be modified by another thread.
         */
        private Country geoLocation;

        public GeoblockingPredicate() {
            updateLocationData();
        }

        public void updateLocation() {
            updateLocationData();
        }

        private void updateLocationData() {
            geoLocation = ApplicationConfiguration.getInstance().getGeographicLocation();
        }

        @Override
        public boolean test(DatenFilm film) {
            if (film.countrySet.isEmpty())
                return true;
            else {
                return film.countrySet.contains(geoLocation);
            }
        }
    }
}
