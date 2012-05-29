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
package mediathek;

import java.io.IOException;
import java.text.SimpleDateFormat;
import java.util.Date;
import javax.swing.event.EventListenerList;
import mediathek.controller.filme.filmeImportieren.MediathekListener;
import mediathek.tool.GuiFunktionenProgramme;

public class Log {

    public static boolean playerMeldungenAus = false;
    public static final String LOG_FEHLER = "fehler";
    public static final String LOG_SYSTEM = "system";
    public static final String LOG_PLAYER = "player";
    private static EventListenerList listeners = new EventListenerList();
    private static boolean prog = false;

    public static String getCompileDate() {
        String ret = "";
        try {
            //Version
            Date d = new Date(Main.class.getResource("Main.class").openConnection().getLastModified());
            ret = Konstanten.PROGRAMMNAME + " " + Konstanten.VERSION + " - Compiled: " + new SimpleDateFormat("dd.MM.yyyy, HH:mm").format(d);
        } catch (Exception ex) {
            Log.fehlerMeldung("Log.getCompileDate: ", ex);
        }
        return ret;
    }

    public static void addAdListener(MediathekListener listener) {
        listeners.add(MediathekListener.class, listener);
    }

    public static synchronized void versionsMeldungen(String classname) {
        Log.systemMeldung("###########################################################");
        long totalMem = Runtime.getRuntime().totalMemory();
        Log.systemMeldung("totalMemory: " + totalMem / (1024L * 1024L) + " MB");
        long maxMem = Runtime.getRuntime().maxMemory();
        Log.systemMeldung("maxMemory: " + maxMem / (1024L * 1024L) + " MB");
        long freeMem = Runtime.getRuntime().freeMemory();
        Log.systemMeldung("freeMemory: " + freeMem / (1024L * 1024L) + " MB");
        Log.systemMeldung("###########################################################");
        //Version
        Log.systemMeldung(getCompileDate());
        Log.systemMeldung("Klassenname: " + classname);
        Log.systemMeldung("###########################################################");
    }

    public static synchronized void startMeldungen(String classname) {
        versionsMeldungen(classname);
        Log.systemMeldung("Programmpfad: " + GuiFunktionenProgramme.getPathJar());
        Log.systemMeldung("Verzeichnis Einstellungen: " + Daten.getBasisVerzeichnis());
        Log.systemMeldung("Useragent: " + Daten.getUserAgent());
        Log.systemMeldung("###########################################################");
        Log.systemMeldung("");
        Log.systemMeldung("");
    }

    public static synchronized void fehlerMeldung(String klasse, Exception ex) {
        fehlermeldung(klasse, new String[]{ex.getMessage(), ""});
    }

    public static synchronized void fehlerMeldung(String klasse, Exception ex, String text) {
        String[] str = new String[2];
        str[0] = ex.getLocalizedMessage();
        str[1] = text;
        fehlermeldung(klasse, str);
    }

    public static synchronized void fehlerMeldung(String klasse, String text) {
        fehlermeldung(klasse, new String[]{text});
    }

    public static synchronized void fehlerMeldung(String klasse, String[] text) {
        fehlermeldung(klasse, text);
    }

    public static synchronized void systemMeldung(String[] text) {
        meldung(text);
    }

    public static synchronized void systemMeldung(String text) {
        meldung(new String[]{text});
    }

    public static synchronized void playerMeldung(String text) {
        if (!playerMeldungenAus) {
            playermeldung(new String[]{text});
        }
    }

    public static synchronized void progress(String texte) {
        prog = true;
        texte += "\r";
        System.out.print(texte);
    }

    private static void fehlermeldung(String klasse, String[] texte) {
        if (prog) {
            // dann brauchen wir erst eine Leerzeite um die Progresszeile zu löschen
            System.out.print("                                                                            \r");
            prog = false;
        }
        final String FEHLER = "Fehler: ";
        final String z = "*";
        System.out.println(z + z + z + z + z + z + z + z + z + z + z + z + z + z + z + z + z + z + z + z + z + z + z + z + z + z + z + z);
        System.out.println(z + " " + FEHLER + klasse);
        notifyMediathekListener(LOG_FEHLER, FEHLER + klasse);
        for (int i = 0; i < texte.length; ++i) {
            System.out.println(z + "           " + texte[i]);
            notifyMediathekListener(LOG_FEHLER, texte[i]);
        }
        System.out.println(z + z + z + z + z + z + z + z + z + z + z + z + z + z + z + z + z + z + z + z + z + z + z + z + z + z + z + z);
    }

    private static void meldung(String[] texte) {
        if (prog) {
            // dann brauchen wir erst eine Leerzeite um die Progresszeile zu löschen
            System.out.print("                                                                            \r");
            prog = false;
        }
        final String SYSTEMMELDUNG = "";
        final String z = ".";
        System.out.println(z + " " + SYSTEMMELDUNG + texte[0]);
        notifyMediathekListener(LOG_SYSTEM, texte[0]);
        for (int i = 1; i < texte.length; ++i) {
            System.out.println(z + " " + texte[i]);
            notifyMediathekListener(LOG_SYSTEM, texte[i]);
        }
    }

    private static void playermeldung(String[] texte) {
        if (prog) {
            // dann brauchen wir erst eine Leerzeite um die Progresszeile zu löschen
            System.out.print("                                                                            \r");
            prog = false;
        }
        final String z = "  >>";
        System.out.println(z + " " + texte[0]);
        notifyMediathekListener(LOG_PLAYER, texte[0]);
        for (int i = 1; i < texte.length; ++i) {
            System.out.println(z + " " + texte[i]);
            notifyMediathekListener(LOG_PLAYER, texte[i]);
        }
    }

    private static void notifyMediathekListener(String art, String zeile) {
        for (MediathekListener l : listeners.getListeners(MediathekListener.class)) {
            l.ping(art, zeile);
        }
    }
}
