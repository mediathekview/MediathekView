package mediathek.daten.blacklist;

import mediathek.daten.DatenFilm;
import mediathek.tool.Filter;

import java.util.ArrayList;
import java.util.List;
import java.util.function.Predicate;

class ApplyBlacklistFilterPredicate implements Predicate<DatenFilm> {
    private static final String[] EMPTY_STRING = {""};
    private final BlacklistMode mode;
    private final List<CompiledBlacklistRule> compiledRules;

    public ApplyBlacklistFilterPredicate(List<BlacklistRule> listeBlacklist) {
        mode = BlacklistMode.fromConfig();
        compiledRules = createPatterns(listeBlacklist);
    }

    /**
     * Create all needed patterns once and keep the original rule order.
     */
    private List<CompiledBlacklistRule> createPatterns(List<BlacklistRule> listeBlacklist) {
        var compiled = new ArrayList<CompiledBlacklistRule>(listeBlacklist.size());
        for (BlacklistRule entry : listeBlacklist) {
            final String[] pTitel = createPattern(entry.hasTitlePattern(), entry.getTitel());
            final String[] pThema = createPattern(entry.hasThemaPattern(), entry.getThema_titel());
            compiled.add(new CompiledBlacklistRule(entry, pTitel, pThema));
        }
        return compiled;
    }

    @Override
    public boolean test(DatenFilm film) {
        for (CompiledBlacklistRule compiledRule : compiledRules) {
            if (performFiltering(compiledRule.rule(), compiledRule.pTitel(), compiledRule.pThema(), film)) {
                return mode.keepFilm(true);
            }
        }

        return mode.keepFilm(false);
    }

    protected String[] mySplit(final String inputString) {
        final String[] pTitle = inputString.split(",");
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


        final String senderSuchen = entry.getSender();
        final String themaSuchen = entry.getThema();

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

}
