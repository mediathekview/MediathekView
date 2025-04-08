package mediathek.tool;

import com.google.common.cache.CacheBuilder;
import com.google.common.cache.CacheLoader;
import com.google.common.cache.LoadingCache;
import mediathek.daten.DatenFilm;
import mediathek.daten.abo.DatenAbo;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.jetbrains.annotations.NotNull;

import java.util.HashSet;
import java.util.Set;
import java.util.concurrent.TimeUnit;
import java.util.regex.Pattern;

public class Filter {
    /**
     * Stores the regexp strings that were rejected as invalid.
     */
    public static final Set<String> regExpErrorList = new HashSet<>();
    /**
     * The cache for already compiled RegExp.
     * Entries will be removed if the haven´t been accessed for more than 5 minutes.
     */
    private static final LoadingCache<String, Pattern> CACHE = CacheBuilder.newBuilder()
            .expireAfterAccess(5, TimeUnit.MINUTES)
            .build(new PatternCacheLoader());
    private static final Logger logger = LogManager.getLogger(Filter.class);

    public static boolean aboExistiertBereits(DatenAbo aboExistiert, DatenAbo aboPruefen) {
        // prüfen ob "aboExistiert" das "aboPrüfen" mit abdeckt, also die gleichen (oder mehr)
        // Filme findet, dann wäre das neue Abo hinfällig

        String senderExistiert = aboExistiert.getSender();
        String themaExistiert = aboExistiert.getThema();

        String[] titelExistiert = aboExistiert.getTitle().toLowerCase().split(",");
        String[] themaTitelExistiert = aboExistiert.getThemaTitel().toLowerCase().split(",");
        String[] irgendwoExistiert = aboExistiert.getIrgendwo().toLowerCase().split(",");

        // Abos sollen sich nicht nur in der Länge unterscheiden
        String themaPruefen = aboPruefen.getThema();
        String titelPruefen = aboPruefen.getTitle();
        String irgendwoPruefen = aboPruefen.getIrgendwo();

        if (conditionExists(senderExistiert, aboPruefen.getSender())) {
            if (conditionExists(themaExistiert, themaPruefen)) {
                if (titleConditionExists(titelExistiert, titelPruefen)) {
                    if (themaTitelConditionExists(themaTitelExistiert, themaPruefen, titelPruefen)) {
                        if (aboIrgendwoConditionExists(irgendwoExistiert, themaPruefen, titelPruefen, irgendwoPruefen)) {
                            return true;
                        }
                    }
                }
            }
        }
        return false;
    }

    private static boolean aboIrgendwoConditionExists(String[] irgendwoExistiert, String themaPruefen, String titelPruefen, String irgendwoPruefen) {
        return irgendwoExistiert.length == 0
                || pruefen(irgendwoExistiert, themaPruefen)
                || pruefen(irgendwoExistiert, titelPruefen)
                || pruefen(irgendwoExistiert, irgendwoPruefen);
    }

    public static boolean filterAufFilmPruefenWithLength(final String senderSuchen, final String themaSuchen,
                                                         final String[] titelSuchen, final String[] themaTitelSuchen,
                                                         final String[] irgendwoSuchen,
                                                         final int laengeMinutenSuchen, final boolean min,
                                                         final DatenFilm film, final boolean checkLength) {
        // prüfen ob xxxSuchen im String imXxx enthalten ist, themaTitelSuchen wird mit Thema u. Titel verglichen
        // senderSuchen exakt mit sender
        // themaSuchen exakt mit thema
        // titelSuchen muss im Titel nur enthalten sein
        boolean result = false;
        final var thema = film.getThema();
        final var title = film.getTitle();

        if (senderConditionExists(senderSuchen, film)) {
            if (conditionExists(themaSuchen, thema)) {
                if (titleConditionExists(titelSuchen, title)) {
                    if (themaTitelConditionExists(themaTitelSuchen, thema, title)) {
                        if (irgendwoConditionExists(film, irgendwoSuchen, thema, title)) {
                            if (checkLength) {
                                result = laengePruefen(laengeMinutenSuchen, film.getFilmLength(), min);
                            } else {
                                result = true;
                            }
                        }
                    }
                }
            }
        }

        return result;
    }

    public static boolean filterAufFilmPruefen(@NotNull final String senderSuchen, @NotNull final String themaSuchen,
                                               @NotNull final String[] titelSuchen, @NotNull final String[] themaTitelSuchen,
                                               @NotNull final String[] irgendwoSuchen,
                                               @NotNull final DatenFilm film) {
        // prüfen ob xxxSuchen im String imXxx enthalten ist, themaTitelSuchen wird mit Thema u. Titel verglichen
        // senderSuchen exakt mit sender
        // themaSuchen exakt mit thema
        // titelSuchen muss im Titel nur enthalten sein
        boolean result = false;
        final var thema = film.getThema();
        final var title = film.getTitle();

        if (senderConditionExists(senderSuchen, film)) {
            if (conditionExists(themaSuchen, thema)) {
                if (titleConditionExists(titelSuchen, title)) {
                    if (themaTitelConditionExists(themaTitelSuchen, thema, title)) {
                        if (irgendwoConditionExists(film, irgendwoSuchen, thema, title)) {
                            result = true;
                        }
                    }
                }
            }
        }

        return result;
    }

    private static boolean irgendwoConditionExists(@NotNull DatenFilm film, @NotNull String[] irgendwoSuchen, @NotNull String thema, @NotNull String title) {
        return irgendwoSuchen.length == 0
                || pruefen(irgendwoSuchen, film.getDescription())
                || pruefen(irgendwoSuchen, thema)
                || pruefen(irgendwoSuchen, title);
    }

    private static boolean themaTitelConditionExists(@NotNull String[] themaTitelSuchen, @NotNull String thema, @NotNull String title) {
        return themaTitelSuchen.length == 0
                || pruefen(themaTitelSuchen, thema)
                || pruefen(themaTitelSuchen, title);
    }

    private static boolean titleConditionExists(@NotNull String[] titelSuchen, @NotNull String title) {
        //performance bottleneck
        return titelSuchen.length == 0 || pruefen(titelSuchen, title);
    }

    private static boolean conditionExists(@NotNull String obj1, @NotNull String obj2) {
        return obj1.isEmpty() || obj2.equalsIgnoreCase(obj1);
    }

    private static boolean senderConditionExists(@NotNull String senderSuchen, @NotNull DatenFilm film) {
        //performance bottleneck
        return senderSuchen.isEmpty() || film.getSender().compareTo(senderSuchen) == 0;
    }

    public static boolean lengthCheck(int filterLaengeInMinuten, long filmLaenge) {
        return filterLaengeInMinuten == 0 || filmLaenge == 0;
    }

    private static boolean checkLengthNoMin(int filterLaengeInMinuten, long filmLaenge) {
        final int filterLength = filterLaengeInMinuten * 60;

        return lengthCheck(filterLaengeInMinuten, filmLaenge) || filmLaenge < filterLength;
    }

    public static boolean checkLengthWithMin(int filterLaengeInMinuten, long filmLaenge) {
        final int filterLength = filterLaengeInMinuten * 60;

        return lengthCheck(filterLaengeInMinuten, filmLaenge) || filmLaenge > filterLength;
    }

    public static boolean laengePruefen(int filterLaengeInMinuten, long filmLaenge, boolean min) {
        boolean result;

        if (min)
            result = checkLengthWithMin(filterLaengeInMinuten, filmLaenge);
        else
            result = checkLengthNoMin(filterLaengeInMinuten, filmLaenge);

        return result;
    }

    public static boolean pruefen(@NotNull String[] filter, @NotNull final String im) {
        // wenn einer passt, dann ists gut
        final var strFilter = filter[0];
        if (filter.length == 1) {
            if (strFilter.isEmpty()) {
                return true; // Filter ist leer, das wars
            } else {
                final Pattern p;
                if ((p = makePattern(strFilter)) != null) {
                    // dann ists eine RegEx
                    return p.matcher(im).matches();
                }
            }
        }

        return checkLowercase(filter, im.toLowerCase());
    }

    /**
     * @param filter the filters array
     * @param im     checked String IN LOWERCASE!!!!!
     * @return true or false
     */
    public static boolean checkLowercase(@NotNull String[] filter, @NotNull String im) {
        for (String s : filter) {
            // dann jeden Suchbegriff checken
            if (im.contains(s)) {
                return true;
            }
        }

        return false;
    }

    public static boolean isPattern(@NotNull final String textSuchen) {
        return textSuchen.startsWith("#:");
    }

    /**
     * Compile a regexp pattern if it doesn´t exist in the pattern cache.
     *
     * @param regExpStr regexp to be compiled
     * @return the compiled regexp or null on error.
     */
    public static Pattern makePattern(final String regExpStr) {
        Pattern p;
        if (isPattern(regExpStr)) {
            try {
                p = CACHE.get(regExpStr);
            } catch (Exception ex) {
                logger.error("!!!!");
                logger.error("INVALID REGEX PATTERN DETECTED: {}", regExpStr);
                logger.error("!!! Please review your config files !!!");
                logger.error("!!!!");
                regExpErrorList.add(regExpStr);
                p = null;
            }
        } else
            p = null;

        return p;
    }

    /**
     * Create pattern without using the cache.
     * Used for interactive search field where cache pollution is not wanted.
     *
     * @param regExpStr the regexp pattern
     * @return Pattern if successful, otherwise null.
     */
    public static Pattern makePatternNoCache(final String regExpStr) {
        Pattern p;
        if (isPattern(regExpStr)) {
            try {
                final String regexPattern = regExpStr.substring(2);
                p = Pattern.compile(regexPattern,
                        Pattern.CASE_INSENSITIVE | Pattern.UNICODE_CASE | Pattern.DOTALL);
            } catch (IllegalArgumentException ex) {
                p = null;
            }
        } else
            p = null;

        return p;
    }

    /**
     * Check if we have errors
     *
     * @return true if there are errors, false otherwise
     */
    public static boolean regExpErrorsOccured() {
        return !regExpErrorList.isEmpty();
    }

    /**
     * This loader will compile regexp patterns when they are not in cache.
     */
    static class PatternCacheLoader extends CacheLoader<String, Pattern> {

        @Override
        public @NotNull Pattern load(@NotNull String pattern) throws IllegalArgumentException {
            logger.trace("COMPILING PATTERN: {}", pattern);
            final String regexPattern = pattern.substring(2);

            return Pattern.compile(regexPattern,
                    Pattern.CASE_INSENSITIVE | Pattern.UNICODE_CASE | Pattern.DOTALL);
        }
    }
}
