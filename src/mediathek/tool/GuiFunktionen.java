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

import com.jidesoft.utils.SystemInfo;
import java.awt.Dimension;
import java.awt.Frame;
import java.awt.Toolkit;
import java.awt.Window;
import java.awt.datatransfer.StringSelection;
import java.io.File;
import javax.swing.JDialog;
import javax.swing.JFrame;
import javax.swing.SwingUtilities;
import mediathek.MediathekGui;
import mediathek.controller.Log;
import mediathek.daten.Daten;

public class GuiFunktionen extends Funktionen {

    public static void updateGui(MediathekGui mediathekGui) {
        try {
            SwingUtilities.updateComponentTreeUI(mediathekGui);
            for (Frame f : Frame.getFrames()) {
                SwingUtilities.updateComponentTreeUI(f);
                for (Window w : f.getOwnedWindows()) {
                    SwingUtilities.updateComponentTreeUI(w);
                }
            }
        } catch (Exception ignored) {
        }

    }

    public static void copyToClipboard(String s) {
        Toolkit.getDefaultToolkit().getSystemClipboard().setContents(new StringSelection(s), null);
    }

    public static void getSize(String nr, JFrame jFrame) {
        Daten.mVConfig.add(nr, jFrame.getSize().width + ":"
                + jFrame.getSize().height + ":"
                + jFrame.getLocation().x + ":"
                + jFrame.getLocation().y);
    }

    public static void getSize(String nr, JDialog jDialog) {
        Daten.mVConfig.add(nr, jDialog.getSize().width + ":"
                + jDialog.getSize().height + ":"
                + jDialog.getLocation().x + ":"
                + jDialog.getLocation().y);
    }

    public static void setSize(String nr, JFrame jFrame, JFrame relativFrame) {
        int breite, hoehe, posX, posY;
        breite = 0;
        hoehe = 0;
        posX = 0;
        posY = 0;
        String[] arr = Daten.mVConfig.get(nr).split(":");
        try {
            if (arr.length == 4) {
                breite = Integer.parseInt(arr[0]);
                hoehe = Integer.parseInt(arr[1]);
                posX = Integer.parseInt(arr[2]);
                posY = Integer.parseInt(arr[3]);
            }
        } catch (Exception ex) {
            breite = 0;
            hoehe = 0;
            posX = 0;
            posY = 0;
        }
        if (breite > 0 && hoehe > 0) {
            jFrame.setSize(new Dimension(breite, hoehe));
        }
        if (posX > 0 && posY > 0) {
            jFrame.setLocation(posX, posY);
        } else if (relativFrame != null) {
            jFrame.setLocationRelativeTo(relativFrame);
        }
    }

    public static boolean setSize(String nr, JDialog jDialog, JFrame relativFrame) {
        boolean ret = false;
        int breite, hoehe, posX, posY;
        breite = 0;
        hoehe = 0;
        posX = 0;
        posY = 0;
        String[] arr = Daten.mVConfig.get(nr).split(":");
        try {
            if (arr.length == 4) {
                breite = Integer.parseInt(arr[0]);
                hoehe = Integer.parseInt(arr[1]);
                posX = Integer.parseInt(arr[2]);
                posY = Integer.parseInt(arr[3]);
            }
        } catch (Exception ex) {
            breite = 0;
            hoehe = 0;
            posX = 0;
            posY = 0;
        }
        if (breite > 0 && hoehe > 0) {
            jDialog.setSize(new Dimension(breite, hoehe));
            ret = true;
        }
        if (posX > 0 && posY > 0) {
            jDialog.setLocation(posX, posY);
        } else if (relativFrame != null) {
            jDialog.setLocationRelativeTo(relativFrame);
        }
        return ret;
    }

    public static String addsPfad(String pfad1, String pfad2) {
        String ret = concatPaths(pfad1, pfad2);
        if (ret.equals("")) {
            Log.fehlerMeldung(283946015, "GuiFunktionen.addsPfad", pfad1 + " - " + pfad2);
        }
        return ret;
    }

    public static String concatPaths(String pfad1, String pfad2) {
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
        return ret;
    }

    private final static int WIN_MAX_PATH_LENGTH = 250;
    private final static int X_MAX_NAME_LENGTH = 255;

    public static String[] checkLengthPath(String[] pathName) {
        if (SystemInfo.isWindows()) {
            // in Win dürfen die Pfade nicht länger als 255 Zeichen haben (für die Infodatei kommen noch ".txt" dazu)
            if ((pathName[0].length() + 10) > WIN_MAX_PATH_LENGTH) {
                // es sollen für den Dateinamen mind. 10 Zeichen bleiben
                Log.fehlerMeldung(102036598, "GuiFunktionen.checkLengthPath", "Pfad zu lange: " + pathName[0]);
                pathName[0] = GuiFunktionen.getHomePath();
            }
            if ((pathName[0].length() + pathName[1].length()) > WIN_MAX_PATH_LENGTH) {
                Log.fehlerMeldung(902367369, "GuiFunktionen.checkLengthPath", "Name zu lange: " + pathName[0]);
                int maxNameL = WIN_MAX_PATH_LENGTH - pathName[0].length();
                pathName[1] = cutName(pathName[1], maxNameL);
            }
        } else {
            // für X-Systeme
            if ((pathName[1].length()) > X_MAX_NAME_LENGTH) {
                Log.fehlerMeldung(823012012, "GuiFunktionen.checkLengthPath", "Name zu lange: " + pathName[1]);
                pathName[1] = cutName(pathName[1], X_MAX_NAME_LENGTH);
            }
        }
        return pathName;
    }

    public static String cutName(String name, int length) {
        if (name.length() > length) {
            name = name.substring(0, length - 4) + name.substring(name.length() - 4);
        }
        return name;
    }

    public static boolean istUrl(String dateiUrl) {
        //return dateiUrl.startsWith("http") ? true : false || dateiUrl.startsWith("www") ? true : false;
        return dateiUrl.startsWith("http") || dateiUrl.startsWith("www");
    }

    public static String getDateiName(String pfad) {
        //Dateinamen einer URL extrahieren
        String ret = "";
        if (pfad != null) {
            if (!pfad.equals("")) {
                ret = pfad.substring(pfad.lastIndexOf('/') + 1);
            }
        }
        if (ret.contains("?")) {
            ret = ret.substring(0, ret.indexOf('?'));
        }
        if (ret.contains("&")) {
            ret = ret.substring(0, ret.indexOf('&'));
        }
        if (ret.equals("")) {
            Log.fehlerMeldung(395019631, "GuiFunktionen.getDateiName", pfad);
        }
        return ret;
    }

    public static String getDateiSuffix(String pfad) {
        // Suffix einer URL extrahieren
        // "http://ios-ondemand.swr.de/i/swr-fernsehen/bw-extra/20130202/601676.,m,s,l,.mp4.csmil/index_2_av.m3u8?e=b471643725c47acd"
        String ret = "";
        if (pfad != null) {
            if (!pfad.equals("") && pfad.contains(".")) {
                ret = pfad.substring(pfad.lastIndexOf('.') + 1);
            }
        }
        if (ret.equals("")) {
            Log.fehlerMeldung(969871236, "GuiFunktionen.getDateiSuffix", pfad);
        }
        if (ret.contains("?")) {
            ret = ret.substring(0, ret.indexOf('?'));
        }
        if (ret.length() > 5) {
            // dann ist was faul
            ret = "---";
            Log.fehlerMeldung(821397046, "GuiFunktionen.getDateiSuffix", pfad);
        }
        return ret;
    }

    /**
     * Return the path to the user´s home directory.
     *
     * @return String to the user´s home directory.
     */
    public static String getHomePath() {
        return System.getProperty("user.home");
    }

    /**
     * Liefert den Standardpfad für Downloads.
     *
     * @return Standardpfad zu den Downloads.
     */
    public static String getStandardDownloadPath() {
        if (getOs() == OperatingSystemType.MAC) {
            return addsPfad(getHomePath(), "Downloads");
        } else {
            return addsPfad(getHomePath(), Konstanten.VERZEICHNIS_DOWNLOADS);
        }
    }

    public static String[] addLeerListe(String[] str) {
        //ein Leerzeichen der Liste voranstellen
        int len = str.length + 1;
        String[] liste = new String[len];
        liste[0] = "";
        System.arraycopy(str, 0, liste, 1, len - 1);
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
            ret = Integer.parseInt(Daten.mVConfig.get(MVConfig.SYSTEM_IMPORT_ART_FILME));
        } catch (Exception ex) {
            Daten.mVConfig.add(MVConfig.SYSTEM_IMPORT_ART_FILME, String.valueOf(Konstanten.UPDATE_FILME_AUTO));
            ret = Konstanten.UPDATE_FILME_AUTO;
        }
        return ret;
    }
}
