package mediathek.tool;

import com.github.benmanes.caffeine.cache.Caffeine;
import com.github.benmanes.caffeine.cache.LoadingCache;
import mediathek.daten.DatenFilm;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.jspecify.annotations.NonNull;

import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.TimeUnit;
import java.util.regex.Pattern;

public class Filter {
    /**
     * Stores the regexp strings that were rejected as invalid.
     */
    public static final Set<String> regExpErrorList = ConcurrentHashMap.newKeySet();
    /**
     * The cache for already compiled RegExp.
     * Entries will be removed if the haven´t been accessed for more than 5 minutes.
     */
    private static final LoadingCache<String, Pattern> CACHE = Caffeine.newBuilder()
            .expireAfterAccess(5, TimeUnit.MINUTES)
            .build(Filter::compilePattern);
    private static final Logger logger = LogManager.getLogger(Filter.class);

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

    public static boolean filterAufFilmPruefen(@NonNull final String senderSuchen, @NonNull final String themaSuchen,
                                               @NonNull final String[] titelSuchen, @NonNull final String[] themaTitelSuchen,
                                               @NonNull final String[] irgendwoSuchen,
                                               @NonNull final DatenFilm film) {
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

    private static boolean irgendwoConditionExists(@NonNull DatenFilm film, @NonNull String[] irgendwoSuchen, @NonNull String thema, @NonNull String title) {
        return irgendwoSuchen.length == 0
                || pruefen(irgendwoSuchen, film.getDescription())
                || pruefen(irgendwoSuchen, thema)
                || pruefen(irgendwoSuchen, title);
    }

    private static boolean themaTitelConditionExists(@NonNull String[] themaTitelSuchen, @NonNull String thema, @NonNull String title) {
        return themaTitelSuchen.length == 0
                || pruefen(themaTitelSuchen, thema)
                || pruefen(themaTitelSuchen, title);
    }

    private static boolean titleConditionExists(@NonNull String[] titelSuchen, @NonNull String title) {
        //performance bottleneck
        return titelSuchen.length == 0 || pruefen(titelSuchen, title);
    }

    private static boolean conditionExists(@NonNull String obj1, @NonNull String obj2) {
        return obj1.isEmpty() || obj2.equalsIgnoreCase(obj1);
    }

    private static boolean senderConditionExists(@NonNull String senderSuchen, @NonNull DatenFilm film) {
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

    public static boolean pruefen(@NonNull String[] filter, @NonNull final String im) {
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
    public static boolean checkLowercase(@NonNull String[] filter, @NonNull String im) {
        for (String s : filter) {
            // dann jeden Suchbegriff checken
            if (im.contains(s)) {
                return true;
            }
        }

        return false;
    }

    public static boolean isPattern(@NonNull final String textSuchen) {
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

    private static @NonNull Pattern compilePattern(@NonNull String pattern) throws IllegalArgumentException {
        logger.trace("COMPILING PATTERN: {}", pattern);
        final String regexPattern = pattern.substring(2);

        return Pattern.compile(regexPattern,
                Pattern.CASE_INSENSITIVE | Pattern.UNICODE_CASE | Pattern.DOTALL);
    }
}
