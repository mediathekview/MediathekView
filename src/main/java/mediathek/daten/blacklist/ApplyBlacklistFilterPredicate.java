package mediathek.daten.blacklist;

import mediathek.config.MVConfig;
import mediathek.daten.DatenFilm;
import mediathek.tool.Filter;
import org.apache.commons.lang3.StringUtils;

import java.util.function.Predicate;

class ApplyBlacklistFilterPredicate implements Predicate<DatenFilm> {
    private static final String[] EMPTY_STRING = new String[]{""};
    private final boolean isWhitelist;
    private final ListeBlacklist listeBlacklist;

    public ApplyBlacklistFilterPredicate(ListeBlacklist listeBlacklist) {
        isWhitelist = Boolean.parseBoolean(MVConfig.get(MVConfig.Configs.SYSTEM_BLACKLIST_IST_WHITELIST));
        this.listeBlacklist = listeBlacklist;
    }

    @Override
    public boolean test(DatenFilm film) {
        //logger.trace("BL ENTRY SIZE: {}", listeBlacklist.size());
        //long counter = 0;
        for (DatenBlacklist entry : listeBlacklist) {
            //counter++;
            final String[] pTitel = createPattern(entry.hasTitlePattern(), entry.arr[DatenBlacklist.BLACKLIST_TITEL]);
            final String[] pThema = createPattern(entry.hasThemaPattern(), entry.arr[DatenBlacklist.BLACKLIST_THEMA_TITEL]);

            if (performFiltering(entry, pTitel, pThema, film)) {
                //logger.trace("LEAVING AFTER ITERATION: {}", counter);
                return isWhitelist;
            }
        }
        //logger.trace("HAVE REACHED FINAL RETURN");

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

    private boolean performFiltering(final DatenBlacklist entry,
                                     final String[] titelSuchen, final String[] themaTitelSuchen,
                                     final DatenFilm film) {
        // prüfen ob xxxSuchen im String imXxx enthalten ist, themaTitelSuchen wird mit Thema u. Titel verglichen
        // senderSuchen exakt mit sender
        // themaSuchen exakt mit thema
        // titelSuchen muss im Titel nur enthalten sein

        boolean result = false;
        final String thema = film.getThema();
        final String title = film.getTitle();


        final String senderSuchen = entry.arr[DatenBlacklist.BLACKLIST_SENDER];
        final String themaSuchen = entry.arr[DatenBlacklist.BLACKLIST_THEMA];

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

    private boolean checkLengthWithMin(long filmLaenge) {
        return Filter.lengthCheck(0, filmLaenge) || filmLaenge > 0;
    }
}
