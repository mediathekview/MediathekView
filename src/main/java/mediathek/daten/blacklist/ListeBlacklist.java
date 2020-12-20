package mediathek.daten.blacklist;

import com.google.common.base.Stopwatch;
import mediathek.config.Daten;
import mediathek.config.MVConfig;
import mediathek.daten.DatenFilm;
import mediathek.daten.ListeFilme;
import mediathek.gui.messages.BlacklistChangedEvent;
import mediathek.javafx.filterpanel.ZeitraumSpinner;
import mediathek.mainwindow.MediathekGui;
import mediathek.tool.ApplicationConfiguration;
import mediathek.tool.Filter;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.jetbrains.annotations.NotNull;

import java.util.ArrayList;
import java.util.LinkedList;
import java.util.List;
import java.util.function.Predicate;

@SuppressWarnings("serial")
public class ListeBlacklist extends LinkedList<BlacklistRule> {

    private static final Logger logger = LogManager.getLogger(ListeBlacklist.class);
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
    private int nr;

    /**
     * Add item without notifying registered listeners.
     *
     * @param b {@link BlacklistRule} item.
     */
    public synchronized void addWithoutNotification(BlacklistRule b) {
        b.arr[BlacklistRule.BLACKLIST_NR] = Integer.toString(nr++);
        super.add(b);
    }

    @Override
    public synchronized boolean add(BlacklistRule b) {
        b.arr[BlacklistRule.BLACKLIST_NR] = Integer.toString(nr++);
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
    public synchronized BlacklistRule remove(int idx) {
        BlacklistRule ret = super.remove(idx);
        filterListAndNotifyListeners();
        return ret;
    }

    public synchronized BlacklistRule remove(String ruleNumber) {
        BlacklistRule bl;
        if ((bl = getRuleByNr(ruleNumber)) != null) {
            remove(bl);
        }
        filterListAndNotifyListeners();
        return bl;
    }

    @Override
    public synchronized BlacklistRule get(int idx) {
        return super.get(idx);
    }

    /**
     * Return the element at the specified {@link String} position.
     *
     * @param ruleNumber Index string of the specified element
     * @return the specified element in the list
     */
    public synchronized BlacklistRule getRuleByNr(final String ruleNumber) {
        return stream()
                .filter(e -> e.arr[BlacklistRule.BLACKLIST_NR].equals(ruleNumber))
                .findFirst()
                .orElse(null);

    }

    @Override
    public synchronized void clear() {
        super.clear();
        filterListAndNotifyListeners();
    }

    public synchronized Object[][] getObjectData() {
        Object[][] object = new Object[size()][BlacklistRule.MAX_ELEM];

        int i = 0;
        for (BlacklistRule blacklist : this) {
            object[i] = blacklist.arr;
            ++i;
        }
        return object;
    }

    /**
     * Main filtering routine
     */
    public synchronized void filterListe() {
        Stopwatch stopwatch = Stopwatch.createStarted();
        final Daten daten = Daten.getInstance();
        final ListeFilme completeFilmList = daten.getListeFilme();
        final ListeFilme filteredList = daten.getListeFilmeNachBlackList();

        filteredList.clear();

        loadCurrentFilterSettings();

        if (completeFilmList != null && !completeFilmList.isEmpty()) { // Check if there are any movies
            filteredList.setMetaData(completeFilmList.metaData());

            this.parallelStream().forEach(entry -> {
                entry.convertToLowerCase();
                entry.checkPatterns();
            });


            final Predicate<DatenFilm> pred = createPredicate();

            completeFilmList.parallelStream().filter(pred).forEachOrdered(filteredList::add);

            setupNewEntries();

            // Array mit Sendernamen/Themen füllen
            filteredList.fillSenderList();
        }
        stopwatch.stop();
        logger.trace("Complete filtering took: {}", stopwatch);
    }

    /**
     * Setup dynamically the list of filter to be applied to blacklist film list
     *
     * @return The reduced filter predicates.
     */
    private Predicate<DatenFilm> createPredicate() {
        final List<Predicate<DatenFilm>> filterList = new ArrayList<>();
        if (days_lower_boundary != 0)
            filterList.add(this::checkDate);

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

        final Predicate<DatenFilm> pred = filterList.stream().reduce(Predicate::and).orElse(x -> true);
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
        Daten.getInstance().getMessageBus().publishAsync(new BlacklistChangedEvent());
    }

    /**
     * Convert days to milliseconds
     * @param days days
     * @return the input converted to milliseconds
     */
    private long daysToMilliseconds(int days) {
        return 1000L * 60L * 60L * 24L * days;
    }

    /**
     * Load current filter settings from Config
     */
    private void loadCurrentFilterSettings() {
        try {
            final String val = MediathekGui.ui().tabFilme.fap.zeitraumProperty.getValue();
            if (val.equals(ZeitraumSpinner.UNLIMITED_VALUE))
                days_lower_boundary = 0;
            else {
                days_lower_boundary = System.currentTimeMillis() - daysToMilliseconds(Integer.parseInt(val));
            }
        } catch (Exception ex) {
            days_lower_boundary = 0;
        }
        try {
            minimumFilmLength = Long.parseLong(MVConfig.get(MVConfig.Configs.SYSTEM_BLACKLIST_FILMLAENGE)) * 60; // Minuten
        } catch (Exception ex) {
            minimumFilmLength = 0;
        }
        blacklistIsActive = Boolean.parseBoolean(MVConfig.get(MVConfig.Configs.SYSTEM_BLACKLIST_ON));
        doNotShowFutureFilms = Boolean.parseBoolean(MVConfig.get(MVConfig.Configs.SYSTEM_BLACKLIST_ZUKUNFT_NICHT_ANZEIGEN));
        var config = ApplicationConfiguration.getConfiguration();
        doNotShowGeoBlockedFilms = config.getBoolean(ApplicationConfiguration.BLACKLIST_DO_NOT_SHOW_GEOBLOCKED_FILMS, false);

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
            if (Filter.filterAufFilmPruefenWithLength(rule.arr[BlacklistRule.BLACKLIST_SENDER],
                    rule.arr[BlacklistRule.BLACKLIST_THEMA],
                    makePattern(rule.arr[BlacklistRule.BLACKLIST_TITEL]),
                    makePattern(rule.arr[BlacklistRule.BLACKLIST_THEMA_TITEL]),
                    EMPTY_STRING_ARRAY, 0, true, film, true)) {
                return bl_is_whitelist;
            }
        }
        return !bl_is_whitelist;
    }
    final static private String[] EMPTY_STRING_ARRAY = new String[]{""};

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
        final long filmLength = film.getFilmLength();
        return !(filmLength != 0 && minimumFilmLength > filmLength);
    }

    static class GeoblockingPredicate implements Predicate<DatenFilm> {
        /**
         * Stores the current user´s location. Can be modified by another thread.
         */
        private String geoLocation;

        public GeoblockingPredicate() {
            setLocationData();
        }

        public void updateLocation() {
            setLocationData();
        }

        private void setLocationData() {
            geoLocation = ApplicationConfiguration.getConfiguration().getString(ApplicationConfiguration.GEO_LOCATION);
        }

        @Override
        public boolean test(DatenFilm film) {
            var geoOpt = film.getGeo();
            if (geoOpt.isEmpty())
                return true;
            else
                return geoOpt.orElse("").contains(geoLocation);
        }
    }
}
