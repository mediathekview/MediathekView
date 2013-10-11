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

import java.awt.Color;
import java.util.regex.Pattern;
import javax.swing.JTextField;

import com.jidesoft.utils.SystemInfo;
import mediathek.daten.DatenAbo;
import msearch.daten.DatenFilm;

public class Filter {

    public static boolean aboExistiertBereits(DatenAbo aboExistiert, DatenAbo aboPruefen) {
        // prüfen ob aboExistiert das aboPrüfen mit abdeckt, also die gleichen (oder mehr)
        // Filme findet, dann wäre das neue Abo hinfällig

        String senderExistiert = aboExistiert.arr[DatenAbo.ABO_SENDER_NR];
        String themaExistiert = aboExistiert.arr[DatenAbo.ABO_THEMA_NR];
        String[] titelExistiert = aboExistiert.arr[DatenAbo.ABO_TITEL_NR].split(",");
        String[] themaTitelExistiert = aboExistiert.arr[DatenAbo.ABO_THEMA_TITEL_NR].split(",");
        String[] irgendwoExistiert = aboExistiert.arr[DatenAbo.ABO_IRGENDWO_NR].split(",");
        // Abos sollen sich nicht nur in der Länge unterscheiden
        // int laengeExistiert = aboExistiert.mindestdauerMinuten;
        String senderPruefen = aboPruefen.arr[DatenAbo.ABO_SENDER_NR];
        String themaPruefen = aboPruefen.arr[DatenAbo.ABO_THEMA_NR];
        String titelPruefen = aboPruefen.arr[DatenAbo.ABO_TITEL_NR];
        String irgendwoPruefen = aboPruefen.arr[DatenAbo.ABO_IRGENDWO_NR];
        // int laengePruefen = aboPruefen.mindestdauerMinuten;

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
                            //if (laengeExistiert == 0 || laengePruefen >= laengeExistiert) {
                            return true;
                            //}
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
                                || pruefen(irgendwoSuchen, film.arr[DatenFilm.FILM_THEMA_NR])
                                || pruefen(irgendwoSuchen, film.arr[DatenFilm.FILM_TITEL_NR])
                                || pruefen(irgendwoSuchen, film.arr[DatenFilm.FILM_BESCHREIBUNG_NR])
                                || pruefen(irgendwoSuchen, film.arr[DatenFilm.FILM_KEYWORDS_NR])) {
                            if (mitLaenge) {
                                // die Länge soll mit gefrüft werden
                                if (laengePruefen(laengeMinutenSuchen, film.dauerL)) {
                                    //if (laengeMinutenSuchen == 0 || film.dauerL == 0 || film.dauerL > (laengeMinutenSuchen * 60)) {
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

    public static boolean laengePruefen(int aboLaengeInMinuten, long filmLaenge) {
        return aboLaengeInMinuten == 0 || filmLaenge == 0 || filmLaenge > (aboLaengeInMinuten * 60);
    }

    private static boolean pruefen(String[] aboFilter, String im) {
        // wenn einer passt, dann ists gut
        if (aboFilter.length == 1) {
            Pattern p = makePattern(aboFilter[0]);
            if (p != null) {
                return (p.matcher(im).matches());
            }
        }
        for (String s : aboFilter) {
            if (textPruefen(s, im)) {
                return true;
            }
        }
        return false;
    }

    private static boolean textPruefen(String filter, String imFilm) {
        return imFilm.toLowerCase().contains(filter.toLowerCase());
    }

    public static boolean isPattern(String textSuchen) {
        return textSuchen.startsWith("#:");
    }

    public static Pattern makePattern(String textSuchen) {
        Pattern p = null;
        try {
            if (isPattern(textSuchen)) {
                //p = Pattern.compile(textSuchen.substring(2));
                //String s = textSuchen.substring(2);
                p = Pattern.compile(textSuchen.substring(2), Pattern.CASE_INSENSITIVE);
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
                tf.setBackground(GuiKonstanten.FILTER_REGEX_FEHLER);
            } else {
                tf.setBackground(GuiKonstanten.FILTER_REGEX);
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
//    public static boolean filterAufAboPruefen(String aboFilter_SenderSuchen, String aboFilter_themaSuchen, String aboFilter_titelSuchen, String aboFilter_themaTitelSuchen,
//            String imFilm_Sender, String imFilm_Thema, String imFilm_Titel) {
//        // prüfen ob xxxSuchen im String imXxx enthalten ist, themaTitelSuchen wird mit Thema u. Titel verglichen
//        // senderSuchen exakt mit sender
//        // themaSuchen exakt mit thema
//        // titelSuchen muss im Titel nur enthalten sein
//
//        if (aboFilter_SenderSuchen.equals("") || imFilm_Sender.equalsIgnoreCase(aboFilter_SenderSuchen)) {
//            if (aboFilter_themaSuchen.equals("") || imFilm_Thema.equalsIgnoreCase(aboFilter_themaSuchen)) {
//
//                if (aboFilter_titelSuchen.equals("") || pruefenTitel(aboFilter_titelSuchen, imFilm_Titel)) {
//
//                    if (aboFilter_themaTitelSuchen.equals("")
//                            || pruefenThemaTitel(aboFilter_themaTitelSuchen, imFilm_Thema)
//                            || pruefenThemaTitel(aboFilter_themaTitelSuchen, imFilm_Titel)) {
//                        return true;
//                    }
//                }
//            }
//        }
//        return false;
//    }
//    public static boolean filterBlacklist(String aboFilter_SenderSuchen, String aboFilter_themaSuchen, String aboFilter_titelSuchen, String aboFilter_themaTitelSuchen,
//            String imFilm_Sender, String imFilm_Thema, String imFilm_Titel) {
//        // liefert true wenn der Film angezeigt werden darf!
//        // prüfen ob xxxSuchen im String imXxx enthalten ist, themaTitelSuchen wird mit Thema u. Titel verglichen
//        // themaSuchen exakt mit thema
//        // titelSuchen muss im Titel nur enthalten sein
//
//        if (aboFilter_SenderSuchen.equals("") || !imFilm_Sender.equalsIgnoreCase(aboFilter_SenderSuchen)) {
//            if (aboFilter_themaSuchen.equals("") || !imFilm_Thema.equalsIgnoreCase(aboFilter_themaSuchen)) {
//
//                if (aboFilter_titelSuchen.equals("") || !pruefenTitel(aboFilter_titelSuchen, imFilm_Titel)) {
//
//                    if (aboFilter_themaTitelSuchen.equals("")) {
//                        return true;
//                    } else if (!pruefenThemaTitel(aboFilter_themaTitelSuchen, imFilm_Thema)) {
//                        return true;
//                    } else if (!pruefenThemaTitel(aboFilter_themaTitelSuchen, imFilm_Titel)) {
//                        return true;
//                    }
//                }
//            }
//        }
//        return false;
//    }
//    private static boolean pruefenTitel(String aboFilter, String im) {
//        Pattern p = makePattern(aboFilter);
//        if (p != null) {
//            return (p.matcher(im).matches());
//        }
//        return (textPruefen(aboFilter, im));
//    }
//
//    private static boolean pruefenThemaTitel(String aboFilter, String im) {
//        Pattern p = makePattern(aboFilter);
//        if (p != null) {
//            return (p.matcher(im).matches());
//        } else if (!aboFilter.contains(" ")) {
//            return (textPruefen(aboFilter, im));
//        }
//        String[] arr = getArr(aboFilter);
//        for (int i = 0; i < arr.length; ++i) {
//            if (textPruefen(arr[i], im)) {
//                return true;
//            }
//        }
//        return false;
//    }
//    private static boolean pruefen(String aboFilter, String im) {
//        Pattern p = makePattern(aboFilter);
//        if (p != null) {
//            return (p.matcher(im).matches());
//        } else {
//            return (textPruefen(aboFilter, im));
//        }
//    }
//    private static String[] getArr(String str) {
//        LinkedList<String> liste = new LinkedList<String>();
//        String[] s;
//        s = str.split(" ");
//        for (int i = 0; i < s.length; ++i) {
//            if (!s[i].equals("")) {
//                liste.add(s[i]);
//            }
//        }
//        return liste.toArray(new String[0]);
//    }
}
