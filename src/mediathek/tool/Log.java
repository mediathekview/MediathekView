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

import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.ResourceBundle;
import mediathek.Main;
import mediathek.daten.Daten;

public class Log {

    public static boolean playerMeldungenAus = false;
    public static final int LOG_FEHLER = ListenerMediathekView.EREIGNIS_LOG_FEHLER;
    public static final int LOG_SYSTEM = ListenerMediathekView.EREIGNIS_LOG_SYSTEM;
    public static final int LOG_PLAYER = ListenerMediathekView.EREIGNIS_LOG_PLAYER;
    private static final int MAX_LAENGE_1 = 30000;
    private static final int MAX_LAENGE_2 = 20000;
    public static StringBuffer textSystem = new StringBuffer();
    public static StringBuffer textProgramm = new StringBuffer();
    public static StringBuffer textFehler = new StringBuffer();
//    private static EventListenerList listeners = new EventListenerList();
    private static LinkedList<Integer[]> fehlerListe = new LinkedList<Integer[]>(); // [Fehlernummer, Anzahl]
    private static boolean prog = false;
    private static Date startZeit = new Date(System.currentTimeMillis());
    private static Date stopZeit = null;

    public void resetFehlerListe() {
        fehlerListe.clear();
    }

//    public static void addAdListener(ListenerMediathekView listener) {
//        listeners.add(ListenerMediathekView.class, listener);
//    }
    public static synchronized void versionsMeldungen(String classname) {
        SimpleDateFormat sdf = new SimpleDateFormat("dd.MM.yyyy HH:mm:ss");
        Log.systemMeldung("");
        Log.systemMeldung("");
        Log.systemMeldung("###########################################################");
        Log.systemMeldung("###########################################################");
        Log.systemMeldung("Programmstart: " + sdf.format(startZeit));
        Log.systemMeldung("###########################################################");
        Log.systemMeldung("###########################################################");
        long totalMem = Runtime.getRuntime().totalMemory();
        Log.systemMeldung("totalMemory: " + totalMem / (1024L * 1024L) + " MB");
        long maxMem = Runtime.getRuntime().maxMemory();
        Log.systemMeldung("maxMemory: " + maxMem / (1024L * 1024L) + " MB");
        long freeMem = Runtime.getRuntime().freeMemory();
        Log.systemMeldung("freeMemory: " + freeMem / (1024L * 1024L) + " MB");
        Log.systemMeldung("###########################################################");
        //Version
        Log.systemMeldung(Funktionen.getProgVersionString());
        Log.systemMeldung("Buildnummer: " + Funktionen.getBuildNr());
        Log.systemMeldung("Klassenname: " + classname);
        Log.systemMeldung("###########################################################");
    }

    public static synchronized void startMeldungen(String classname) {
        versionsMeldungen(classname);
        Log.systemMeldung("Programmpfad: " + Funktionen.getPathJar());
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

    public static synchronized void fehlerMeldungMReader(int fehlerNummer, String klasse, String[] text) {
        fehlermeldung_mReader(fehlerNummer, klasse, text);
    }

    public static synchronized void fehlerMeldungGetUrl(int fehlerNummer, Exception ex, String sender, String text[]) {
        fehlermeldung_getUrl(fehlerNummer, sender, ex, text);
    }

    public static synchronized void systemMeldung(String[] text) {
        systemmeldung(text);
    }

    public static synchronized void systemMeldung(String text) {
        systemmeldung(new String[]{text});
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

    public static void printEndeMeldung() {
        if (fehlerListe.size() == 0) {
            systemMeldung("###########################################################");
            systemMeldung(" Keine Fehler :)");
            systemMeldung("###########################################################");
        } else {
            // Fehler ausgeben
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
            systemMeldung("###########################################################");
            Iterator<Integer[]> it = fehlerListe.iterator();
            while (it.hasNext()) {
                Integer[] integers = it.next();
                if (integers[0] < 0) {
                    systemMeldung(" Fehlernummer: " + integers[0] + " Anzahl: " + integers[1]);
                } else {
                    systemMeldung(" Fehlernummer:  " + integers[0] + " Anzahl: " + integers[1]);
                }
            }
            systemMeldung("###########################################################");
        }
        // Laufzeit ausgeben
        stopZeit = new Date(System.currentTimeMillis());
        SimpleDateFormat sdf = new SimpleDateFormat("dd.MM.yyyy HH:mm:ss");
        int minuten;
        try {
            minuten = Math.round((stopZeit.getTime() - startZeit.getTime()) / (1000 * 60));
        } catch (Exception ex) {
            minuten = -1;
        }
        systemMeldung("");
        systemMeldung("");
        systemMeldung("###########################################################");
        systemMeldung("   --> Beginn: " + sdf.format(startZeit));
        systemMeldung("   --> Fertig: " + sdf.format(stopZeit));
        systemMeldung("   --> Dauer[Min]: " + (minuten == 0 ? "<1" : minuten));
        systemMeldung("###########################################################");
        systemMeldung("");
        systemMeldung("   und Tschuess");
        systemMeldung("");
        systemMeldung("");
        systemMeldung("###########################################################");
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

    private static void systemmeldung(String[] texte) {
        if (prog) {
            // dann brauchen wir erst eine Leerzeite um die Progresszeile zu löschen
            System.out.print("                                                                            \r");
            prog = false;
        }
        final String z = ". ";
        if (texte.length <= 1) {
            System.out.println(z + " " + texte[0]);
            notifyMediathekListener(LOG_SYSTEM, texte[0]);
        } else {
            String zeile = "---------------------------------------";
            String txt;
            System.out.println(z + zeile);
            notifyMediathekListener(LOG_SYSTEM, zeile);
            for (int i = 0; i < texte.length; ++i) {
                txt = "| " + texte[i];
                System.out.println(z + txt);
                if (i == 0) {
                    notifyMediathekListener(LOG_SYSTEM, texte[i]);
                } else {
                    notifyMediathekListener(LOG_SYSTEM, "    " + texte[i]);
                }
            }
            notifyMediathekListener(LOG_SYSTEM, " ");
            System.out.println(z + zeile);
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

    public static void clearText(int art) {
        if (art == LOG_FEHLER) {
            textFehler.setLength(0);
        } else if (art == LOG_SYSTEM) {
            textSystem.setLength(0);
        } else if (art == LOG_PLAYER) {
            textProgramm.setLength(0);
        }
        ListenerMediathekView.notify(art, Log.class.getName());
//        for (ListenerMediathekView l : listeners.getListeners(ListenerMediathekView.class)) {
//            if (l.ereignis == art) {
//                l.ping();
//            }
//        }
    }

    private static void notifyMediathekListener(int art, String zeile) {
        if (art == LOG_FEHLER) {
            addText(textFehler, zeile);
        } else if (art == LOG_SYSTEM) {
            addText(textSystem, zeile);
        } else if (art == LOG_PLAYER) {
            addText(textProgramm, zeile);
        }
        ListenerMediathekView.notify(art, Log.class.getName());
//        for (ListenerMediathekView l : listeners.getListeners(ListenerMediathekView.class)) {
//            if (l.ereignis == art) {
//                l.ping();
//            }
//        }
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
