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

import mSearch.tool.MVColor;
import java.awt.Color;
import java.util.regex.Pattern;
import javax.swing.JTextField;
import mSearch.daten.DatenFilm;
import mediathek.daten.DatenAbo;

public class Filter {

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

        if (senderExistiert.equals("") || senderPruefen.equalsIgnoreCase(senderExistiert)) {
            if (themaExistiert.equals("") || themaPruefen.equalsIgnoreCase(themaExistiert)) {

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

    public static boolean filterAufFilmPruefen(String senderSuchen, String themaSuchen,
            String[] titelSuchen, String[] themaTitelSuchen, String[] irgendwoSuchen, int laengeMinutenSuchen,
            DatenFilm film, boolean mitLaenge) {
        // prüfen ob xxxSuchen im String imXxx enthalten ist, themaTitelSuchen wird mit Thema u. Titel verglichen
        // senderSuchen exakt mit sender
        // themaSuchen exakt mit thema
        // titelSuchen muss im Titel nur enthalten sein

        if (senderSuchen.equals("") || film.arr[DatenFilm.FILM_SENDER_NR].equalsIgnoreCase(senderSuchen)) {
            if (themaSuchen.equals("") || film.arr[DatenFilm.FILM_THEMA_NR].equalsIgnoreCase(themaSuchen)) {

                if (titelSuchen.length == 0 || pruefen(titelSuchen, film.arr[DatenFilm.FILM_TITEL_NR])) {

                    if (themaTitelSuchen.length == 0
                            || pruefen(themaTitelSuchen, film.arr[DatenFilm.FILM_THEMA_NR])
                            || pruefen(themaTitelSuchen, film.arr[DatenFilm.FILM_TITEL_NR])) {

                        if (irgendwoSuchen.length == 0
                                || pruefen(irgendwoSuchen, film.arr[DatenFilm.FILM_DATUM_NR])
                                || pruefen(irgendwoSuchen, film.arr[DatenFilm.FILM_THEMA_NR])
                                || pruefen(irgendwoSuchen, film.arr[DatenFilm.FILM_TITEL_NR])
                                || pruefen(irgendwoSuchen, film.arr[DatenFilm.FILM_BESCHREIBUNG_NR])) {
                            // || pruefen(irgendwoSuchen, film.arr[DatenFilm.FILM_WEBSEITE_NR])) { kostet 25% Zeit zusätzlich!
                            if (mitLaenge) {
                                // die Länge soll mit gefrüft werden
                                if (laengePruefen(laengeMinutenSuchen, film.dauerL)) {
                                    return true;
                                }
                            } else {
                                return true;
                            }
                        }
                    }
                }
            }
        }
        return false;
    }

    public static boolean laengePruefen(int filterLaengeInMinuten, long filmLaenge) {
        return filterLaengeInMinuten == 0 || filmLaenge == 0 || filmLaenge > (filterLaengeInMinuten * 60);
    }

    private static boolean pruefen(String[] filter, String im) {
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

        for (String s : filter) {
            // dann jeden Suchbegriff checken
            if (im.toLowerCase().contains(s)) {
                return true;
            }
        }

        // nix wars
        return false;
    }

    public static boolean isPattern(String textSuchen) {
        return textSuchen.startsWith("#:");
    }

    public static Pattern makePattern(String textSuchen) {
        Pattern p = null;
        try {
            if (isPattern(textSuchen)) {
                p = Pattern.compile(textSuchen.substring(2), Pattern.CASE_INSENSITIVE | Pattern.UNICODE_CASE);
            }
        } catch (Exception ex) {
            p = null;
        }
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

    public static void checkPattern2(JTextField tf) {
        // Schriftfarbe ändern wenn eine RegEx
        String text = tf.getText();
        if (Filter.isPattern(text)) {
            if (Filter.makePattern(text) == null) {
                //soll Pattern sein, ist aber falsch
                tf.setForeground(Color.RED);
            } else {
                tf.setForeground(Color.BLUE);
            }
        } else {
            tf.setForeground(Color.BLACK);
        }
    }
}
