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
package mediathek.controller;

import java.io.PrintWriter;
import java.io.StringWriter;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.Iterator;
import java.util.LinkedList;
import javax.swing.SwingUtilities;
import mediathek.daten.Daten;
import mediathek.gui.dialogEinstellungen.PanelMeldungen;
import mediathek.tool.Funktionen;
import mediathek.tool.Konstanten;
import mediathek.tool.ListenerMediathekView;

public class Log {

    public static StringBuffer textSystem = new StringBuffer(10000);
    public static StringBuffer textProgramm = new StringBuffer(10000);
    public static StringBuffer textFehler = new StringBuffer();
    public static boolean playerMeldungenAus = false;
    public static final int LOG_FEHLER = ListenerMediathekView.EREIGNIS_LOG_FEHLER;
    public static final int LOG_SYSTEM = ListenerMediathekView.EREIGNIS_LOG_SYSTEM;
    public static final int LOG_PLAYER = ListenerMediathekView.EREIGNIS_LOG_PLAYER;

    private static final int MAX_LAENGE_1 = 50000;
    private static final int MAX_LAENGE_2 = 30000;
    private static int zeilenNrSystem = 0;
    private static int zeilenNrProgramm = 0;
    private static int zeilenNrFehler = 0;
    private static final LinkedList<Integer[]> fehlerListe = new LinkedList<>(); // [Fehlernummer, Anzahl, Exception(0,1 für ja, nein)]
    private static final boolean progress = false;
    private static final String progressText = "";
    public static PanelMeldungen panelMeldungenFehler = null; // unschön, gab aber sonst einen Deadlock mit notifyMediathekListener
    public static PanelMeldungen panelMeldungenSystem = null;
    public static PanelMeldungen panelMeldungenPlayer = null;
    private final static long BYTES_TO_MBYTE = (1024L * 1024L);
    private final static SimpleDateFormat sdf = new SimpleDateFormat("dd.MM.yyyy HH:mm:ss");

    public static synchronized void versionsMeldungen() {
        final Runtime rt = Runtime.getRuntime();
        final long totalMem = rt.totalMemory();
        final long maxMem = rt.maxMemory();
        final long freeMem = rt.freeMemory();

        //Version
        Log.systemMeldung(Funktionen.getProgVersionString());
        //dynamically get caller class name...
        final Throwable t = new Throwable();
        final StackTraceElement methodCaller = t.getStackTrace()[2];
        systemMeldung("Classname: " + methodCaller.getClassName());
    }

    public static synchronized void startMeldungen() {
        versionsMeldungen();
        Log.systemMeldung("Programmpfad: " + Funktionen.getPathJar());
        Log.systemMeldung("Verzeichnis Einstellungen: " + Daten.getSettingsDirectory_String());
    }

    // Fehlermeldung mit Exceptions
    public static synchronized void fehlerMeldung(int fehlerNummer, Exception ex) {
        fehlermeldung_(fehlerNummer, ex, new String[]{});
    }

    public static synchronized void fehlerMeldung(int fehlerNummer, Exception ex, String text) {
        fehlermeldung_(fehlerNummer, ex, new String[]{text});
    }

    public static synchronized void fehlerMeldung(int fehlerNummer, Exception ex, String text[]) {
        fehlermeldung_(fehlerNummer, ex, text);
    }

    public static synchronized void fehlerMeldung(int fehlerNummer, String text) {
        fehlermeldung_(fehlerNummer, null, new String[]{text});
    }

    public static synchronized void fehlerMeldung(int fehlerNummer, String[] text) {
        fehlermeldung_(fehlerNummer, null, text);
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

    public static void printEndeMeldung() {
        ArrayList<String> ret = printFehlerMeldung();
        for (String s : ret) {
            systemMeldung(s);
        }
    }

    public static ArrayList<String> printFehlerMeldung() {
        ArrayList<String> retList = new ArrayList<>();

        if (fehlerListe.size() == 0) {
            retList.add(" Keine Fehler :)");
        } else {
            // Fehler ausgeben
            int i_1;
            int i_2;
            for (int i = 1; i < fehlerListe.size(); ++i) {
                for (int k = i; k > 0; --k) {
                    i_1 = fehlerListe.get(k - 1)[0];
                    i_2 = fehlerListe.get(k)[0];
                    // if (str1.compareToIgnoreCase(str2) > 0) {
                    if (i_1 < i_2) {
                        fehlerListe.add(k - 1, fehlerListe.remove(k));
                    } else {
                        break;
                    }
                }
            }
            for (Integer[] integers : fehlerListe) {
                boolean ex = integers[2] == 1;
                String strEx;
                if (ex) {
                    strEx = "Ex! ";
                } else {
                    strEx = "    ";
                }
                if (integers[0] < 0) {
                    retList.add(strEx + " Fehlernummer: " + integers[0] + " Anzahl: " + integers[1]);
                } else {
                    retList.add(strEx + " Fehlernummer:  " + integers[0] + " Anzahl: " + integers[1]);
                }
            }
        }
        return retList;
    }

//    public static String printFehlerMeldung_() {
//        String ret = "\n";
//        ret += "-----------------------------------------------------------\n";
//        if (fehlerListe.size() == 0) {
//            ret += " Keine Fehler :)\n";
//        } else {
//            // Fehler ausgeben
//            int i_1;
//            int i_2;
//            for (int i = 1; i < fehlerListe.size(); ++i) {
//                for (int k = i; k > 0; --k) {
//                    i_1 = fehlerListe.get(k - 1)[0];
//                    i_2 = fehlerListe.get(k)[0];
//                    // if (str1.compareToIgnoreCase(str2) > 0) {
//                    if (i_1 < i_2) {
//                        fehlerListe.add(k - 1, fehlerListe.remove(k));
//                    } else {
//                        break;
//                    }
//                }
//            }
//            for (Integer[] integers : fehlerListe) {
//                boolean ex = integers[2] == 1;
//                String strEx;
//                if (ex) {
//                    strEx = "Ex! ";
//                } else {
//                    strEx = "    ";
//                }
//                if (integers[0] < 0) {
//                    ret += strEx + " Fehlernummer: " + integers[0] + " Anzahl: " + integers[1] + "\n";
//                } else {
//                    ret += strEx + " Fehlernummer:  " + integers[0] + " Anzahl: " + integers[1] + "\n";
//                }
//            }
//        }
//        ret += "-----------------------------------------------------------\n";
//        ret += "\n";
//        return ret;
//    }
    private static void addFehlerNummer(int nr, boolean exception) {
        Iterator<Integer[]> it = fehlerListe.iterator();
        int ex = exception ? 1 : 2;
        while (it.hasNext()) {
            Integer[] i = it.next();
            if (i[0] == nr) {
                i[1]++;
                i[2] = ex;
                return;
            }
        }
        // dann gibts die Nummer noch nicht
        fehlerListe.add(new Integer[]{nr, 1, ex});
    }

    private static void fehlermeldung_(int fehlerNummer, Exception ex, String[] texte) {
        addFehlerNummer(fehlerNummer, ex != null);
        final Throwable t = new Throwable();
        final StackTraceElement methodCaller = t.getStackTrace()[2];
        final String klasse = methodCaller.getClassName() + "." + methodCaller.getMethodName();

        if (ex != null || Daten.debug) {
            try {
                String s = getStackTrace(ex);
                System.out.println(s);
            } catch (Exception ignored) {
            }
            // Exceptions immer ausgeben
            if (progress) {
                // dann brauchen wir erst eine Leerzeite um die Progresszeile zu löschen
                System.out.print("                                                                            \r");
            }
            final String FEHLER = "Fehler(" + Konstanten.PROGRAMMNAME + "): ";
            String x, z;
            if (ex != null) {
                x = "!";
            } else {
                x = "=";
            }
            z = "*";
            System.out.println(x + x + x + x + x + x + x + x + x + x + x + x + x + x + x + x + x + x + x + x + x + x + x + x + x + x + x + x + x + x + x + x + x + x + x);
            System.out.println(z + " Fehlernr: " + fehlerNummer);
            if (ex != null) {
                System.out.println(z + " Exception: " + ex.getMessage());
            }
            System.out.println(z + " " + FEHLER + klasse);
            notifyPanelMeldung(LOG_FEHLER, FEHLER + klasse);
            for (String text : texte) {
                System.out.println(z + "           " + text);
                notifyPanelMeldung(LOG_FEHLER, text);
            }
            System.out.println("");
            if (progress) {
                System.out.print(progressText);
            }
        }
    }

    private static void systemmeldung(String[] texte) {
        if (progress) {
            // dann brauchen wir erst eine Leerzeite um die Progresszeile zu löschen
            System.out.print("                                                                            \r");
        }
        final String z = ". ";
        if (texte.length <= 1) {
            System.out.println(z + " " + texte[0]);
            notifyPanelMeldung(LOG_SYSTEM, texte[0]);
        } else {
            String zeile = "---------------------------------------";
            String txt;
            System.out.println(z + zeile);
            notifyPanelMeldung(LOG_SYSTEM, zeile);
            for (int i = 0; i < texte.length; ++i) {
                txt = "| " + texte[i];
                System.out.println(z + txt);
                if (i == 0) {
                    notifyPanelMeldung(LOG_SYSTEM, texte[i]);
                } else {
                    notifyPanelMeldung(LOG_SYSTEM, "    " + texte[i]);
                }
            }
            notifyPanelMeldung(LOG_SYSTEM, " ");
            System.out.println(z + zeile);
        }
        if (progress) {
            System.out.print(progressText);
        }
    }

    private static void playermeldung(String[] texte) {
        if (progress) {
            // dann brauchen wir erst eine Leerzeite um die Progresszeile zu löschen
            System.out.print("                                                                            \r");
        }
        final String z = "  >>";
        System.out.println(z + " " + texte[0]);
        notifyPanelMeldung(LOG_PLAYER, texte[0]);
        for (int i = 1; i < texte.length; ++i) {
            System.out.println(z + " " + texte[i]);
            notifyPanelMeldung(LOG_PLAYER, texte[i]);
        }
        if (progress) {
            System.out.print(progressText);
        }
    }

    public static void clearText(int art) {
        if (art == LOG_FEHLER) {
            zeilenNrFehler = 0;
            textFehler.setLength(0);
        } else if (art == LOG_SYSTEM) {
            zeilenNrSystem = 0;
            textSystem.setLength(0);
        } else if (art == LOG_PLAYER) {
            zeilenNrProgramm = 0;
            textProgramm.setLength(0);
        }
    }

    private static void notifyPanelMeldung(int art, String zeile) {
        if (art == LOG_FEHLER) {
            addText(textFehler, "[" + getNr(zeilenNrFehler++) + "]   " + zeile);
        } else if (art == LOG_SYSTEM) {
            addText(textSystem, "[" + getNr(zeilenNrSystem++) + "]   " + zeile);
        } else if (art == LOG_PLAYER) {
            addText(textProgramm, "[" + getNr(zeilenNrProgramm++) + "]   " + zeile);
        }
        notifyPanel(art);
    }

    private static void notifyPanel(int art) {
        final int a = art;
        try {
            if (SwingUtilities.isEventDispatchThread()) {
                // notify
                if (art == LOG_FEHLER && panelMeldungenFehler != null) {
                    panelMeldungenFehler.notifyPanel();
                } else if (art == LOG_SYSTEM && panelMeldungenSystem != null) {
                    panelMeldungenSystem.notifyPanel();
                } else if (art == LOG_PLAYER && panelMeldungenPlayer != null) {
                    panelMeldungenPlayer.notifyPanel();
                }
            } else {
                SwingUtilities.invokeLater(new Runnable() {
                    @Override
                    public void run() {
                        // notify
                        if (a == LOG_FEHLER && panelMeldungenFehler != null) {
                            panelMeldungenFehler.notifyPanel();
                        } else if (a == LOG_SYSTEM && panelMeldungenSystem != null) {
                            panelMeldungenSystem.notifyPanel();
                        } else if (a == LOG_PLAYER && panelMeldungenPlayer != null) {
                            panelMeldungenPlayer.notifyPanel();
                        }
                    }
                });
            }
        } catch (Exception ex) {
            Log.fehlerMeldung(698989743, ex);
        }
    }

    private static String getStackTrace(Throwable t) {
        StringWriter sw = new StringWriter();
        PrintWriter pw = new PrintWriter(sw, true);
        t.printStackTrace(pw);
        pw.flush();
        sw.flush();
        return sw.toString();
    }

    private static String getNr(int nr) {
        final int MAX_STELLEN = 5;
        final String FUELL_ZEICHEN = "0";
        String str = String.valueOf(nr);
        while (str.length() < MAX_STELLEN) {
            str = FUELL_ZEICHEN + str;
        }
        return str;
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
