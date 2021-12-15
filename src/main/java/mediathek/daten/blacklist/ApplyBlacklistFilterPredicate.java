package mediathek.daten.blacklist;

import mediathek.config.MVConfig;
import mediathek.daten.DatenFilm;
import mediathek.tool.Filter;
import org.apache.commons.lang3.StringUtils;

import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import java.util.function.Predicate;

class ApplyBlacklistFilterPredicate implements Predicate<DatenFilm> {
    private static final String[] EMPTY_STRING = {""};
    private final boolean isWhitelist;
    private final ListeBlacklist listeBlacklist;
    private final Map<BlacklistRule,BlacklistPattern> rulePatternMap;

    public ApplyBlacklistFilterPredicate(ListeBlacklist listeBlacklist) {
        isWhitelist = Boolean.parseBoolean(MVConfig.get(MVConfig.Configs.SYSTEM_BLACKLIST_IST_WHITELIST));
        this.listeBlacklist = listeBlacklist;

        rulePatternMap = new ConcurrentHashMap<>(listeBlacklist.size());

        //pre-create the patterns concurrently before use
        createPatterns();
    }

    /**
     * Create all needed patterns in parallel.
     */
    private void createPatterns() {
        listeBlacklist.parallelStream().forEach(entry -> {
            final String[] pTitel = createPattern(entry.hasTitlePattern(), entry.arr[BlacklistRule.BLACKLIST_TITEL]);
            final String[] pThema = createPattern(entry.hasThemaPattern(), entry.arr[BlacklistRule.BLACKLIST_THEMA_TITEL]);
            var blPattern = new BlacklistPattern(pTitel, pThema);
            rulePatternMap.putIfAbsent(entry, blPattern);
        });
    }

    @Override
    public boolean test(DatenFilm film) {

        for (BlacklistRule entry : listeBlacklist) {
            var blPattern = rulePatternMap.get(entry);

            if (performFiltering(entry, blPattern.pTitel, blPattern.pThema, film)) {
                return isWhitelist;
            }
        }

        //found nothing
        return !isWhitelist;
    }

    private String[] mySplit(final String inputString) {
        final String[] pTitle = StringUtils.split(inputString, ',');
        if (pTitle.length == 0)
            return EMPTY_STRING;
        else
            return pTitle;
    }

    private String[] createPattern(final boolean isPattern, final String inputString) {
        if (isPattern)
            return new String[]{inputString};
        else
            return mySplit(inputString);
    }

    private boolean performFiltering(final BlacklistRule entry,
                                     final String[] titelSuchen, final String[] themaTitelSuchen,
                                     final DatenFilm film) {
        // prüfen ob xxxSuchen im String imXxx enthalten ist, themaTitelSuchen wird mit Thema u. Titel verglichen
        // senderSuchen exakt mit sender
        // themaSuchen exakt mit thema
        // titelSuchen muss im Titel nur enthalten sein

        boolean result = false;
        final String thema = film.getThema();
        final String title = film.getTitle();


        final String senderSuchen = entry.arr[BlacklistRule.BLACKLIST_SENDER];
        final String themaSuchen = entry.arr[BlacklistRule.BLACKLIST_THEMA];

        if (senderSuchen.isEmpty() || film.getSender().compareTo(senderSuchen) == 0) {
            if (themaSuchen.isEmpty() || thema.equalsIgnoreCase(themaSuchen)) {
                if (titelSuchen.length == 0 || Filter.pruefen(titelSuchen, title)) {
                    if (themaTitelSuchen.length == 0
                            || Filter.pruefen(themaTitelSuchen, thema)
                            || Filter.pruefen(themaTitelSuchen, title)) {
                        // die Länge soll mit geprüft werden
                        if (checkLengthWithMin(film.getFilmLength())) {
                            result = true;
                        }
                    }
                }
            }
        }

        return result;
    }

    public boolean lengthCheck(int filterLaengeInMinuten, long filmLaenge) {
        return filterLaengeInMinuten == 0 || filmLaenge == 0;
    }

    private boolean checkLengthWithMin(long filmLaenge) {
        return lengthCheck(0, filmLaenge) || filmLaenge > 0;
    }

    record BlacklistPattern(String[] pTitel, String[] pThema) {

    }
}
