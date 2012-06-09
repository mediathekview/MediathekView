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
import javax.swing.JTable;
import javax.swing.SwingUtilities;
import javax.swing.UIManager;
import mediathek.Daten;
import mediathek.Konstanten;
import mediathek.Log;
import mediathek.daten.*;

public class GuiFunktionen {

    public static boolean setLook(JFrame frame) {
        int look;
        if (DDaten.system[Konstanten.SYSTEM_LOOK_NR].equals("")) {
            DDaten.system[Konstanten.SYSTEM_LOOK_NR] = "1";
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
            Log.fehlerMeldung("GuiFunktionen.setLook", e, "Kann das Look and Feel nicht ändern!");
        }
        return ret;
    }

    public static void copyToClipboard(String s) {
        Toolkit.getDefaultToolkit().getSystemClipboard().setContents(new StringSelection(s), null);
    }

    public static void setProxy(DDaten daten) {
        if (Boolean.parseBoolean(DDaten.system[Konstanten.SYSTEM_PROXY_ON_NR])) {
            System.setProperty("proxySet", "true");
            System.setProperty("http.proxyHost", DDaten.system[Konstanten.SYSTEM_PROXY_SERVER_NR]);
            System.setProperty("http.proxyPort", DDaten.system[Konstanten.SYSTEM_PROXY_PORT_NR]);
            System.setProperty("http.proxyUser", DDaten.system[Konstanten.SYSTEM_PROXY_USER_NR]);
            System.setProperty("http.proxyPassword", DDaten.system[Konstanten.SYSTEM_PROXY_PWD_NR]);
        } else {
            System.setProperty("proxySet", "false");
        }
    }

    public static String replaceLeerDateiname(String pfad, boolean pfadtrennerEntfernen) {
        //verbotene Zeichen entfernen
        String ret = pfad;
        if (pfadtrennerEntfernen) {
            ret = ret.replace("/", "-");
            ret = ret.replace("\\", "-");
        }
        ret = ret.replace("\n", "_");
        ret = ret.replace("\"", "_");
        ret = ret.replace(",", "_");
        ret = ret.replace(";", "_");
        ret = ret.replace("(", "_");
        ret = ret.replace(")", "_");
        ret = ret.replace(" ", "_");
        ret = ret.replace("*", "_");
        ret = ret.replace("?", "_");
        ret = ret.replace("<", "_");
        ret = ret.replace(">", "_");
        ret = ret.replace(":", "_");
        ret = ret.replace("'", "_");
        ret = ret.replace("|", "_");
        return ret;
    }

    public static String addsPfad(String pfad1, String pfad2) {
        String ret = "";
        if (pfad1 != null && pfad2 != null) {
            if (!pfad1.equals("") && !pfad2.equals("")) {
                if (pfad1.charAt(pfad1.length() - 1) == File.separatorChar) {
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
            Log.fehlerMeldung("GuiFunktionen.addsPfad", pfad1 + " - " + pfad2);
        }
        return ret;
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
            Log.fehlerMeldung("GuiFunktionen.getDateiName", pfad);
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

    public static void spaltenFilmLoeschen(JTable tabelle, boolean ziel, boolean zeit, boolean datei) {
        //abo anzeigen
        //ziel Downloadziel anzeigen
        //prog passendes Programm anzeigen
        for (int i = 0; i < tabelle.getColumnCount(); ++i) {
            if (!DDaten.debug) {
                if (!zeit && i == DatenFilm.FILM_ZEIT_NR
                        || i == DatenFilm.FILM_URL_ORG_NR
                        || i == DatenFilm.FILM_URL_RTMP_NR
                        || i == DatenFilm.FILM_URL_AUTH_NR
                        || i == DatenFilm.FILM_URL_THEMA_NR) {
                    tabelle.getColumnModel().getColumn(i).setMinWidth(0);
                    tabelle.getColumnModel().getColumn(i).setPreferredWidth(0);
                    tabelle.getColumnModel().getColumn(i).setMaxWidth(0);
                }
            }
        }
    }

    public static void spaltenFilmSetzen(JTable tabelle, boolean ziel) {
        //ziel Downloadziel anzeigen
        //prog passendes Programm anzeigen
        for (int i = 0; i < tabelle.getColumnCount(); ++i) {
            tabelle.getColumnModel().getColumn(i).setMinWidth(10);
            tabelle.getColumnModel().getColumn(i).setMaxWidth(3000);
            tabelle.getColumnModel().getColumn(i).setPreferredWidth(200);
            if (i == DatenFilm.FILM_NR_NR
                    || i == DatenFilm.FILM_DATUM_NR
                    || i == DatenFilm.FILM_ZEIT_NR
                    || i == DatenFilm.FILM_SENDER_NR) {
                tabelle.getColumnModel().getColumn(i).setMinWidth(10);
                tabelle.getColumnModel().getColumn(i).setMaxWidth(3000);
                tabelle.getColumnModel().getColumn(i).setPreferredWidth(100);
            } else if (i == DatenFilm.FILM_TITEL_NR) {
                tabelle.getColumnModel().getColumn(i).setMinWidth(10);
                tabelle.getColumnModel().getColumn(i).setMaxWidth(3000);
                tabelle.getColumnModel().getColumn(i).setPreferredWidth(300);
            } else if (i == DatenFilm.FILM_URL_NR) {
                tabelle.getColumnModel().getColumn(i).setMinWidth(10);
                tabelle.getColumnModel().getColumn(i).setMaxWidth(3000);
                tabelle.getColumnModel().getColumn(i).setPreferredWidth(500);
            }
        }
        spaltenFilmLoeschen(tabelle, ziel, true, true);
    }

    public static void spaltenAboSetzen(JTable tabelle) {
        for (int i = 0; i < tabelle.getColumnCount(); ++i) {
            tabelle.getColumnModel().getColumn(i).setMinWidth(10);
            tabelle.getColumnModel().getColumn(i).setMaxWidth(3000);
            if (i == DatenAbo.ABO_NR_NR) {
                tabelle.getColumnModel().getColumn(i).setPreferredWidth(50);
            } else if (i == DatenAbo.ABO_EINGESCHALTET_NR
                    || i == DatenAbo.ABO_DOWN_DATUM_NR
                    || i == DatenAbo.ABO_SENDER_NR) {
                tabelle.getColumnModel().getColumn(i).setPreferredWidth(100);
            } else {
                tabelle.getColumnModel().getColumn(i).setPreferredWidth(200);
            }
        }
    }

    public static void spaltenDownloadSetzen(JTable tabelle) {
        for (int i = 0; i < tabelle.getColumnCount(); ++i) {
            if (i == DatenDownload.DOWNLOAD_URL_AUTH_NR
                    || i == DatenDownload.DOWNLOAD_URL_RTMP_NR
                    || i == DatenDownload.DOWNLOAD_ART_NR
                    || i == DatenDownload.DOWNLOAD_QUELLE_NR) {
                tabelle.getColumnModel().getColumn(i).setMinWidth(0);
                tabelle.getColumnModel().getColumn(i).setMaxWidth(0);
                tabelle.getColumnModel().getColumn(i).setPreferredWidth(0);
            } else {
                tabelle.getColumnModel().getColumn(i).setMinWidth(10);
                tabelle.getColumnModel().getColumn(i).setMaxWidth(3000);
                if (i == DatenDownload.DOWNLOAD_NR_NR) {
                    tabelle.getColumnModel().getColumn(i).setPreferredWidth(50);
                } else if (i == DatenDownload.DOWNLOAD_DATUM_NR
                        || i == DatenDownload.DOWNLOAD_SENDER_NR
                        || i == DatenDownload.DOWNLOAD_THEMA_NR
                        || i == DatenDownload.DOWNLOAD_ZEIT_NR
                        || i == DatenDownload.DOWNLOAD_PROGRAMM_RESTART_NR) {
                    tabelle.getColumnModel().getColumn(i).setPreferredWidth(100);
                } else {
                    tabelle.getColumnModel().getColumn(i).setPreferredWidth(200);
                }
            }
        }
    }

    public static String textLaenge(int max, String text, boolean mitte) {
        final int MAX = max;
        if (text.length() > MAX) {
            if (mitte) {
                text = text.substring(0, 25) + " .... " + text.substring(text.length() - (MAX - 31));
            } else {
                text = text.substring(0, MAX - 1);
            }
        }
        while (text.length() < MAX) {
            text = text + " ";
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

    public static boolean istUrl(String dateiUrl) {
        return dateiUrl.startsWith("http") ? true : false || dateiUrl.startsWith("www") ? true : false;
    }
}
