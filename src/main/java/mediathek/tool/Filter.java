/*
 *    MediathekView
 *    Copyright (C) 2013   W. Xaver
 *    W.Xaver[at]googlemail.com
 *    http://zdfmediathk.sourceforge.net/
 *
 *    This program is free software: you can redistribute it and/or modify
 *    it under the terms of the GNU General Public License as published by
 *    the Free Software Foundation, either version 3 of the License, or
 *    any later version.
 *
 *    This program is distributed in the hope that it will be useful,
 *    but WITHOUT ANY WARRANTY; without even the implied warranty of
 *    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *    GNU General Public License for more details.
 *
 *    You should have received a copy of the GNU General Public License
 *    along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */
package mediathek.tool;

import mSearch.daten.DatenFilm;
import mediathek.config.MVColor;
import mediathek.daten.DatenAbo;

import javax.swing.*;
import java.awt.*;
import java.util.HashMap;
import java.util.regex.Pattern;

public class Filter {

    /**
     * The cache for already compiled RegExp.
     */
    private static final HashMap<String, Pattern> PATTERN_CACHE = new HashMap<>();

    public static boolean aboExistiertBereits(DatenAbo aboExistiert, DatenAbo aboPruefen) {
        // prüfen ob "aboExistiert" das "aboPrüfen" mit abdeckt, also die gleichen (oder mehr)
        // Filme findet, dann wäre das neue Abo hinfällig

        String senderExistiert = aboExistiert.arr[DatenAbo.ABO_SENDER];
        String themaExistiert = aboExistiert.arr[DatenAbo.ABO_THEMA];
        String[] titelExistiert = aboExistiert.arr[DatenAbo.ABO_TITEL].toLowerCase().split(",");
        String[] themaTitelExistiert = aboExistiert.arr[DatenAbo.ABO_THEMA_TITEL].toLowerCase().split(",");
        String[] irgendwoExistiert = aboExistiert.arr[DatenAbo.ABO_IRGENDWO].toLowerCase().split(",");
        // Abos sollen sich nicht nur in der Länge unterscheiden
        String senderPruefen = aboPruefen.arr[DatenAbo.ABO_SENDER];
        String themaPruefen = aboPruefen.arr[DatenAbo.ABO_THEMA];
        String titelPruefen = aboPruefen.arr[DatenAbo.ABO_TITEL];
        String irgendwoPruefen = aboPruefen.arr[DatenAbo.ABO_IRGENDWO];

        if (senderExistiert.isEmpty() || senderPruefen.equalsIgnoreCase(senderExistiert)) {
            if (themaExistiert.isEmpty() || themaPruefen.equalsIgnoreCase(themaExistiert)) {

                if (titelExistiert.length == 0 || pruefen(titelExistiert, titelPruefen)) {

                    if (themaTitelExistiert.length == 0
                            || pruefen(themaTitelExistiert, themaPruefen)
                            || pruefen(themaTitelExistiert, titelPruefen)) {

                        if (irgendwoExistiert.length == 0
                                || pruefen(irgendwoExistiert, themaPruefen)
                                || pruefen(irgendwoExistiert, titelPruefen)
                                || pruefen(irgendwoExistiert, irgendwoPruefen)) {
                            return true;
                        }
                    }
                }
            }
        }
        return false;
    }

    public static boolean filterAufFilmPruefen(final String senderSuchen, final String themaSuchen,
                                               final String[] titelSuchen, final String[] themaTitelSuchen,
                                               final String[] irgendwoSuchen,
                                               final int laengeMinutenSuchen, final boolean min,
                                               final DatenFilm film, final boolean mitLaenge) {
        // prüfen ob xxxSuchen im String imXxx enthalten ist, themaTitelSuchen wird mit Thema u. Titel verglichen
        // senderSuchen exakt mit sender
        // themaSuchen exakt mit thema
        // titelSuchen muss im Titel nur enthalten sein
        boolean result = false;

        if (senderSuchen.isEmpty() || film.arr[DatenFilm.FILM_SENDER].compareTo(senderSuchen) == 0) {
            if (themaSuchen.isEmpty() || film.arr[DatenFilm.FILM_THEMA].equalsIgnoreCase(themaSuchen)) {

                if (titelSuchen.length == 0 || pruefen(titelSuchen, film.arr[DatenFilm.FILM_TITEL])) {

                    if (themaTitelSuchen.length == 0
                            || pruefen(themaTitelSuchen, film.arr[DatenFilm.FILM_THEMA])
                            || pruefen(themaTitelSuchen, film.arr[DatenFilm.FILM_TITEL])) {

                        if (irgendwoSuchen.length == 0
                                || pruefen(irgendwoSuchen, film.arr[DatenFilm.FILM_DATUM])
                                || pruefen(irgendwoSuchen, film.arr[DatenFilm.FILM_THEMA])
                                || pruefen(irgendwoSuchen, film.arr[DatenFilm.FILM_TITEL])) {
                            if (mitLaenge) {
                                // die Länge soll mit gefrüft werden
                                if (laengePruefen(laengeMinutenSuchen, film.getFilmLength(), min)) {
                                    result = true;
                                }
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

    public static boolean laengePruefen(int filterLaengeInMinuten, long filmLaenge, boolean min) {
        if (min) {
            return filterLaengeInMinuten == 0 || filmLaenge == 0 || filmLaenge > (filterLaengeInMinuten * 60);
        } else {
            return filterLaengeInMinuten == 0 || filmLaenge == 0 || filmLaenge < (filterLaengeInMinuten * 60);
        }
    }

    private static boolean pruefen(String[] filter, final String im) {
        // wenn einer passt, dann ists gut
        Pattern p;
        if (filter.length == 1) {
            if (filter[0].isEmpty()) {
                // Filter ist leer, das wars
                return true;
            } else if ((p = makePattern(filter[0])) != null) {
                // dann ists eine RegEx
                return (p.matcher(im).matches());
            }
        }

        final String lowerCase = im.toLowerCase();
        for (String s : filter) {
            // dann jeden Suchbegriff checken
            if (lowerCase.contains(s)) {
                return true;
            }
        }

        // nix wars
        return false;
    }

    public static boolean isPattern(final String textSuchen) {
        return textSuchen.startsWith("#:");
    }

    /**
     * Compile a regexp pattern if it doesn´t exist in the pattern cache.
     *
     * @param textSuchen regexp to be compiled
     * @return the compiled regexp
     */
    public static Pattern makePattern(final String textSuchen) {
        Pattern p;
        if (isPattern(textSuchen)) {
            p = PATTERN_CACHE.get(textSuchen);
            if (p == null) {
                //nothing in cache, so we have to compile...
                try {
                    p = Pattern.compile(textSuchen.substring(2), Pattern.CASE_INSENSITIVE | Pattern.UNICODE_CASE);
                    PATTERN_CACHE.put(textSuchen, p);
                } catch (Exception ignored) {
                    p = null;
                }
            }
        } else
            p = null;

        return p;
    }

    public static void checkPattern1(JTextField tf) {
        // Hintergrund ändern wenn eine RegEx
        String text = tf.getText();
        if (Filter.isPattern(text)) {
            if (Filter.makePattern(text) == null) {
                //soll Pattern sein, ist aber falsch
                tf.setBackground(MVColor.FILTER_REGEX_FEHLER.color);
            } else {
                tf.setBackground(MVColor.FILTER_REGEX.color);
            }
        } else {
            tf.setBackground(Color.WHITE);
        }
    }
}
