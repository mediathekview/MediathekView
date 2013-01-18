/*
 * MediathekView
 * Copyright (C) 2008 W. Xaver
 * W.Xaver[at]googlemail.com
 * http://zdfmediathk.sourceforge.net/
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package mediathek.tool;

import java.awt.Frame;
import java.awt.Toolkit;
import java.awt.Window;
import java.awt.datatransfer.StringSelection;
import java.io.File;
import java.util.Collections;
import java.util.LinkedList;
import javax.swing.JFrame;
import javax.swing.SwingUtilities;
import javax.swing.UIManager;
import mediathek.daten.DDaten;
import mediathek.daten.Daten;
import mediathek.daten.DatenFilm;

public class GuiFunktionen extends Funktionen {

    public static boolean setLook(JFrame frame) {
        int look;
        if (DDaten.system[Konstanten.SYSTEM_LOOK_NR].equals("")) {
            DDaten.system[Konstanten.SYSTEM_LOOK_NR] = "0";
        }
        look = Integer.parseInt(DDaten.system[Konstanten.SYSTEM_LOOK_NR]);
        if (look != 0) {
            return setLook(frame, look);
        }
        return true;
    }

    public static boolean setLook(JFrame frame, int look) {
        boolean ret = false;
        try {
            switch (look) {
                case 0:
                case 1:
                    //bei 0 egentlich nichts tun, wenn aber gewechselt, dann zurückschalten
                    UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName());
                    break;
                default:
                    UIManager.setLookAndFeel(GuiKonstanten.THEME[look][1]);
                    break;
            }
            SwingUtilities.updateComponentTreeUI(frame);
            for (Frame f : Frame.getFrames()) {
                SwingUtilities.updateComponentTreeUI(f);
                for (Window w : f.getOwnedWindows()) {
                    SwingUtilities.updateComponentTreeUI(w);
                }
            }
            ret = true;
        } catch (Exception e) {
            Log.fehlerMeldung(305964198, Log.FEHLER_ART_PROG, "GuiFunktionen.setLook", e, "Kann das Look and Feel nicht ändern!");
        }
        return ret;
    }

    public static void copyToClipboard(String s) {
        Toolkit.getDefaultToolkit().getSystemClipboard().setContents(new StringSelection(s), null);
    }

    public static void setProxy(DDaten daten) {
        if (Boolean.parseBoolean(DDaten.system[Konstanten.SYSTEM_HTTP_PROXY_ON_NR])) {
            System.setProperty("proxySet", "true");
            System.setProperty("http.proxyHost", DDaten.system[Konstanten.SYSTEM_HTTP_PROXY_SERVER_NR]);
            System.setProperty("http.proxyPort", DDaten.system[Konstanten.SYSTEM_HTTP_PROXY_PORT_NR]);
            System.setProperty("http.proxyUser", DDaten.system[Konstanten.SYSTEM_HTTP_PROXY_USER_NR]);
            System.setProperty("http.proxyPassword", DDaten.system[Konstanten.SYSTEM_HTTP_PROXY_PWD_NR]);
        } else {
            System.setProperty("proxySet", "false");
        }
    }

    public static String replaceString(String s, DatenFilm film) {
        s = s.replace("%D", film.arr[DatenFilm.FILM_DATUM_NR].equals("") ? DatumZeit.getHeute_yyyyMMdd() : datumDatumZeitReinigen(datumDrehen(film.arr[DatenFilm.FILM_DATUM_NR])));
        s = s.replace("%d", film.arr[DatenFilm.FILM_ZEIT_NR].equals("") ? DatumZeit.getJetzt_HHMMSS() : datumDatumZeitReinigen(film.arr[DatenFilm.FILM_ZEIT_NR]));
        s = s.replace("%t", film.arr[DatenFilm.FILM_THEMA_NR]);
        s = s.replace("%T", film.arr[DatenFilm.FILM_TITEL_NR]);
        s = s.replace("%s", film.arr[DatenFilm.FILM_SENDER_NR]);
        s = s.replace("%H", DatumZeit.getHeute_yyyyMMdd());
        s = s.replace("%h", DatumZeit.getJetzt_HHMMSS());
        s = s.replace("%N", GuiFunktionen.getDateiName(film.arr[DatenFilm.FILM_URL_NR]));
        return s;
    }

    private static String datumDrehen(String datum) {
        String ret = "";
        if (!datum.equals("")) {
            try {
                if (datum.length() == 10) {
                    String tmp = datum.substring(6); // Jahr
                    tmp += "." + datum.substring(3, 5); // Monat
                    tmp += "." + datum.substring(0, 2); // Tag
                    ret = tmp;
                }
            } catch (Exception ex) {
                Log.fehlerMeldung(775421006, Log.FEHLER_ART_PROG, "DatenFilm.datumDrehen", ex, datum);
            }

        }
        return ret;
    }

    private static String datumDatumZeitReinigen(String datum) {
        String ret;
        ret = datum;
        ret = ret.replace(":", "");
        ret = ret.replace(".", "");
        return ret;
    }

    public static String replaceLeerDateiname(String pfad, boolean pfadtrennerEntfernen, boolean leerEntfernen) {
        //verbotene Zeichen entfernen
        String ret = pfad;
        boolean winPfad = false;
        if (pfad.length() >= 2) {
            if (pfad.charAt(1) == ':') {
                // damit auch "d:" als Pfad geht
                winPfad = true;
            }
//            if (pfad.substring(1, 3).equals(":\\")) {
//                winPfad = true;
//            }
        }
        if (pfadtrennerEntfernen) {
            ret = ret.replace("\\", "-");
            ret = ret.replace("/", "-");
        } else {
            String sl;
            if (File.separator.equals("\\")) {
                sl = "/";
            } else {
                sl = "\\";
            }
            ret = ret.replace(sl, "-");
        }
        if (leerEntfernen) {
            ret = ret.replace(" ", "_");
        }
        ret = ret.replace("\n", "_");
        ret = ret.replace("\"", "_");
        ret = ret.replace(",", "_");
        ret = ret.replace(";", "_");
        ret = ret.replace("(", "_");
        ret = ret.replace(")", "_");
        ret = ret.replace("*", "_");
        ret = ret.replace("?", "_");
        ret = ret.replace("<", "_");
        ret = ret.replace(">", "_");
        ret = ret.replace(":", "_");
        ret = ret.replace("'", "_");
        ret = ret.replace("|", "_");
        ret = getAscii(ret);
        if (winPfad) {
            if (ret.length() >= 3) {
                ret = ret.substring(0, 1) + ":" + ret.substring(2);
            } else if (ret.length() >= 2) {
                ret = ret.substring(0, 1) + ":";
            }
        }
        return ret;
    }

    private static String getAscii(String ret) {
        String r = "";
        if (!Boolean.parseBoolean(Daten.system[Konstanten.SYSTEM_NUR_ASCII_NR])) {
            return ret;
        } else {
            char c;
            ret = ret.replace("ä", "ae");
            ret = ret.replace("ö", "oe");
            ret = ret.replace("ü", "ue");
            ret = ret.replace("Ä", "Ae");
            ret = ret.replace("Ö", "Oe");
            ret = ret.replace("Ü", "Ue");
            for (int i = 0; i < ret.length(); ++i) {
                if ((c = ret.charAt(i)) < 127) {
                    r += c;
                } else {
                    r += "_";
                }
            }
        }
        return r;
    }

    public static String addsPfad(String pfad1, String pfad2) {
        String ret = "";
        if (pfad1 != null && pfad2 != null) {
            if (!pfad1.equals("") && !pfad2.equals("")) {
                if (pfad1.endsWith(File.separator)) {
                    ret = pfad1.substring(0, pfad1.length() - 1);
                } else {
                    ret = pfad1;
                }
                if (pfad2.charAt(0) == File.separatorChar) {
                    ret += pfad2;
                } else {
                    ret += File.separator + pfad2;
                }
            }
        }
        if (ret.equals("")) {
            Log.fehlerMeldung(283946015, Log.FEHLER_ART_PROG, "GuiFunktionen.addsPfad", pfad1 + " - " + pfad2);
        }
        return ret;
    }

    public static String addUrl(String u1, String u2) {
        if (u1.endsWith("/")) {
            return u1 + u2;
        } else {
            return u1 + "/" + u2;
        }
    }

    public static boolean istUrl(String dateiUrl) {
        return dateiUrl.startsWith("http") ? true : false || dateiUrl.startsWith("www") ? true : false;
    }

    public static String getDateiName(String pfad) {
        //Dateinamen einer URL extrahieren
        String ret = "";
        if (pfad != null) {
            if (!pfad.equals("")) {
                ret = pfad.substring(pfad.lastIndexOf("/") + 1);
            }
        }
        if (ret.contains("?")) {
            ret = ret.substring(0, ret.indexOf("?"));
        }
        if (ret.contains("&")) {
            ret = ret.substring(0, ret.indexOf("&"));
        }
        if (ret.equals("")) {
            Log.fehlerMeldung(395019631, Log.FEHLER_ART_PROG, "GuiFunktionen.getDateiName", pfad);
        }
        return ret;
    }

    public static void listeSort(LinkedList<String> liste) {
        //Stringliste alphabetisch sortieren
        GermanStringSorter sorter = GermanStringSorter.getInstance();
        Collections.sort(liste, sorter);
    }

    public static String getHomePath() {
        //lifert den Pfad zum Homeverzeichnis
        return System.getProperty("user.home");
    }

    public static String getStandardDownloadPath() {
        //lifert den Standardpfad für Downloads
        if (getOs() == OS_MAC) {
            return addsPfad(getHomePath(), "Desktop");
        }
        return addsPfad(getHomePath(), Konstanten.VERZEICNHISS_DOWNLOADS);
    }

    public static String[] addLeerListe(String[] str) {
        //ein Leerzeichen der Liste voranstellen
        int len = str.length + 1;
        String[] liste = new String[len];
        liste[0] = "";
        for (int i = 1; i < len; ++i) {
            liste[i] = str[i - 1];
        }
        return liste;
    }

    public static String textLaenge(int max, String text, boolean mitte, boolean addVorne) {
        if (text.length() > max) {
            if (mitte) {
                text = text.substring(0, 25) + " .... " + text.substring(text.length() - (max - 31));
            } else {
                text = text.substring(0, max - 1);
            }
        }
        while (text.length() < max) {
            if (addVorne) {
                text = " " + text;
            } else {
                text = text + " ";
            }
        }
        return text;
    }

    public static int getImportArtFilme() {
        int ret;
        try {
            ret = Integer.parseInt(DDaten.system[Konstanten.SYSTEM_IMPORT_ART_FILME_NR]);
        } catch (Exception ex) {
            Daten.system[Konstanten.SYSTEM_IMPORT_ART_FILME_NR] = String.valueOf(GuiKonstanten.UPDATE_FILME_AUTO);
            ret = GuiKonstanten.UPDATE_FILME_AUTO;
        }
        return ret;
    }
}
