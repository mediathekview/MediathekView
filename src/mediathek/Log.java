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

import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.ResourceBundle;
import javax.swing.event.EventListenerList;
import mediathek.controller.filme.filmeImportieren.MediathekListener;
import mediathek.tool.GuiFunktionenProgramme;

public class Log {

    public static boolean playerMeldungenAus = false;
    public static final String LOG_FEHLER = "fehler";
    public static final String LOG_SYSTEM = "system";
    public static final String LOG_PLAYER = "player";
    private static final int MAX_LAENGE_1 = 30000;
    private static final int MAX_LAENGE_2 = 20000;
    public static StringBuffer textSystem = new StringBuffer();
    public static StringBuffer textProgramm = new StringBuffer();
    public static StringBuffer textFehler = new StringBuffer();
    private static EventListenerList listeners = new EventListenerList();
    private static LinkedList<Integer[]> fehlerListe = new LinkedList<Integer[]>(); // [Fehlernummer, Anzahl]
    private static boolean prog = false;

    public static String getCompileDate() {
        String ret = "";
        try {
            //Version
            Date d = new Date(Main.class.getResource("Main.class").openConnection().getLastModified());
            ret = Konstanten.PROGRAMMNAME + " " + Konstanten.VERSION + " - Compiled: " + new SimpleDateFormat("dd.MM.yyyy, HH:mm").format(d);
        } catch (Exception ex) {
            Log.fehlerMeldung(569614756, "Log.getCompileDate: ", ex);
        }
        return ret;
    }

    public static String getBuildNr(String propToken) {
        final ResourceBundle rb;
        String msg = "";
        try {
            ResourceBundle.clearCache();
            rb = ResourceBundle.getBundle("version");
            msg = rb.getString(propToken);
        } catch (Exception e) {
            System.err.println(e.getMessage());
            System.err.println("Token " + propToken + " not in Propertyfile!");
        }
        return msg;
    }

    public void resetFehlerListe() {
        fehlerListe.clear();
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
        Log.systemMeldung("Buildnummer: " + getBuildNr("BUILD"));
        Log.systemMeldung("Klassenname: " + classname);
        Log.systemMeldung("###########################################################");
    }

    public static synchronized void startMeldungen(String classname) {
        Date startZeit = new Date(System.currentTimeMillis());
        SimpleDateFormat sdf = new SimpleDateFormat("dd.MM.yyyy HH:mm:ss");
        Log.systemMeldung("");
        Log.systemMeldung("");
        Log.systemMeldung("###########################################################");
        Log.systemMeldung("###########################################################");
        Log.systemMeldung("Programmstart: " + sdf.format(startZeit));
        Log.systemMeldung("###########################################################");
        versionsMeldungen(classname);
        Log.systemMeldung("Programmpfad: " + GuiFunktionenProgramme.getPathJar());
        Log.systemMeldung("Verzeichnis Einstellungen: " + Daten.getBasisVerzeichnis());
        Log.systemMeldung("Useragent: " + Daten.getUserAgent());
        Log.systemMeldung("###########################################################");
        Log.systemMeldung("");
        Log.systemMeldung("");
    }

    public static synchronized void debugMeldung(String text) {
        if (Daten.debug) {
            debugmeldung(text);
        }
    }

    public static synchronized void fehlerMeldung(int fehlerNummer, String klasse, Exception ex) {
        fehlermeldung_(fehlerNummer, klasse, new String[]{ex.getMessage(), ""});
    }

    public static synchronized void fehlerMeldung(int fehlerNummer, String klasse, Exception ex, String text) {
        String[] str = new String[2];
        str[0] = ex.getLocalizedMessage();
        str[1] = text;
        fehlermeldung_(fehlerNummer, klasse, str);
    }

    public static synchronized void fehlerMeldung(int fehlerNummer, String klasse, String text) {
        fehlermeldung_(fehlerNummer, klasse, new String[]{text});
    }

    public static synchronized void fehlerMeldung(int fehlerNummer, String klasse, String[] text) {
        fehlermeldung_(fehlerNummer, klasse, text);
    }

    public static synchronized void fehlerMeldungMReader(int fehlerNummer, String klasse, String text) {
        fehlermeldung_mReader(fehlerNummer, klasse, new String[]{text});
    }

    public static synchronized void fehlerMeldungGetUrl(int fehlerNummer, Exception ex, String sender, String text[]) {
        fehlermeldung_getUrl(fehlerNummer, sender, ex, text);
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
//        if (!prog) {
//            // erst wieder eine Leerzeile
//            System.out.println("                                                                           ");
        prog = true;
//        }
        texte += "\r";
        System.out.print(texte);
    }

    public static void printFehlerNummer() {
        final String z = "*";
        if (fehlerListe.size() == 0) {
            System.out.println(z + z + z + z + z + z + z + z + z + z + z + z + z + z + z + z + z + z + z + z + z + z + z + z + z + z + z + z);
            System.out.println(z + " " + "Keine Fehler :)");
            System.out.println(z + z + z + z + z + z + z + z + z + z + z + z + z + z + z + z + z + z + z + z + z + z + z + z + z + z + z + z);
            return;
        }
        int i_1;
        int i_2;
        for (int i = 1; i < fehlerListe.size(); ++i) {
            for (int k = i; k > 0; --k) {
                i_1 = fehlerListe.get(k - 1)[1];
                i_2 = fehlerListe.get(k)[1];
                // if (str1.compareToIgnoreCase(str2) > 0) {
                if (i_1 < i_2) {
                    fehlerListe.add(k - 1, fehlerListe.remove(k));
                } else {
                    break;
                }
            }
        }
        System.out.println(z + z + z + z + z + z + z + z + z + z + z + z + z + z + z + z + z + z + z + z + z + z + z + z + z + z + z + z);
        Iterator<Integer[]> it = fehlerListe.iterator();
        while (it.hasNext()) {
            Integer[] integers = it.next();
            if (integers[0] < 0) {
                System.out.println(z + " " + "Fehlernummer: " + integers[0] + " Anzahl: " + integers[1]);
            } else {
                System.out.println(z + " " + "Fehlernummer:  " + integers[0] + " Anzahl: " + integers[1]);
            }
        }
        System.out.println(z + z + z + z + z + z + z + z + z + z + z + z + z + z + z + z + z + z + z + z + z + z + z + z + z + z + z + z);
    }

    private static void addFehlerNummer(int nr) {
        Iterator<Integer[]> it = fehlerListe.iterator();
        while (it.hasNext()) {
            Integer[] i = it.next();
            if (i[0].intValue() == nr) {
                i[1]++;
                return;
            }
        }
        // dann gibts die Nummer noch nicht
        fehlerListe.add(new Integer[]{new Integer(nr), new Integer(1)});
    }

    private static void fehlermeldung_mReader(int fehlerNummer, String sender, String[] texte) {
        addFehlerNummer(fehlerNummer);
        if (Daten.debug) {
            if (prog) {
                // dann brauchen wir erst eine Leerzeite um die Progresszeile zu löschen
                System.out.print("                                                                            \r");
                prog = false;
            }
            final String FEHLER = "MReader: ";
            final String z = "  ==>";
            System.out.println(" Fehlernr: " + fehlerNummer);
            System.out.println(z + " " + FEHLER + sender);
            notifyMediathekListener(LOG_FEHLER, FEHLER + sender);
            for (int i = 0; i < texte.length; ++i) {
                System.out.println("                " + texte[i]);
                notifyMediathekListener(LOG_FEHLER, texte[i]);
            }
        }
    }

    private static void fehlermeldung_getUrl(int fehlerNummer, String sender, Exception ex, String text[]) {
        addFehlerNummer(fehlerNummer);
        if (prog) {
            // dann brauchen wir erst eine Leerzeite um die Progresszeile zu löschen
            System.out.print("                                                                            \r");
            prog = false;
        }
        final String FEHLER = "GetUrl: ";
        final String z = "  ++>";
        System.out.println(" Fehlernr: " + fehlerNummer);
        System.out.println(z + " " + FEHLER + sender + " " + ex.getMessage());
        notifyMediathekListener(LOG_FEHLER, FEHLER + sender + " - " + ex.getMessage());
        for (int i = 0; i < text.length; ++i) {
            System.out.println("                " + text[i]);
            notifyMediathekListener(LOG_FEHLER, text[i]);
        }
    }

    private static void fehlermeldung_(int fehlerNummer, String klasse, String[] texte) {
        addFehlerNummer(fehlerNummer);
        if (prog) {
            // dann brauchen wir erst eine Leerzeite um die Progresszeile zu löschen
            System.out.print("                                                                            \r");
            prog = false;
        }
        final String FEHLER = "Fehler: ";
        final String z = "*";
        System.out.println(z + z + z + z + z + z + z + z + z + z + z + z + z + z + z + z + z + z + z + z + z + z + z + z + z + z + z + z);
        System.out.println(z + " Fehlernr: " + fehlerNummer);
        System.out.println(z + " " + FEHLER + klasse);
        notifyMediathekListener(LOG_FEHLER, FEHLER + klasse);
        for (int i = 0; i < texte.length; ++i) {
            System.out.println(z + "           " + texte[i]);
            notifyMediathekListener(LOG_FEHLER, texte[i]);
        }
        System.out.println(z + z + z + z + z + z + z + z + z + z + z + z + z + z + z + z + z + z + z + z + z + z + z + z + z + z + z + z);
    }

    private static void debugmeldung(String texte) {
        if (prog) {
            // dann brauchen wir erst eine Leerzeite um die Progresszeile zu löschen
            System.out.print("                                                                            \r");
            prog = false;
        }
        System.out.println("|||| " + texte);
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

    public static void clearText(String art) {
        if (art.equals(LOG_FEHLER)) {
            textFehler.setLength(0);
        } else if (art.equals(LOG_SYSTEM)) {
            textSystem.setLength(0);
        } else if (art.equals(LOG_PLAYER)) {
            textProgramm.setLength(0);
        }
        for (MediathekListener l : listeners.getListeners(MediathekListener.class)) {
            l.ping(art);
        }
    }

    private static void notifyMediathekListener(String art, String zeile) {
        if (art.equals(LOG_FEHLER)) {
            addText(textFehler, zeile);
        } else if (art.equals(LOG_SYSTEM)) {
            addText(textSystem, zeile);
        } else if (art.equals(LOG_PLAYER)) {
            addText(textProgramm, zeile);
        }
        for (MediathekListener l : listeners.getListeners(MediathekListener.class)) {
            l.ping(art);
        }
    }

    private static void addText(StringBuffer text, String texte) {
        cut(text);
        text.append(texte);
        text.append(System.getProperty("line.separator"));
    }

    private static void cut(StringBuffer buffer) {
        if (buffer.length() > MAX_LAENGE_1) {
            buffer.delete(0, MAX_LAENGE_2);
        }
    }
}
