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

import java.io.BufferedWriter;
import java.io.File;
import java.io.OutputStreamWriter;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import javax.swing.JFrame;
import javax.swing.JOptionPane;
import javax.swing.SwingUtilities;
import mSearch.tool.MSLog;
import mediathek.daten.Daten;
import mediathek.gui.dialog.DialogZiel;
import mediathek.gui.dialogEinstellungen.PanelMeldungen;
import mediathek.tool.GuiFunktionen;
import mSearch.tool.ListenerMediathekView;
import mediathek.tool.MVFunctionSys;
import mediathek.tool.MVMessageDialog;

public class MVLog {

    public static StringBuffer textSystem = new StringBuffer(10000);
    public static StringBuffer textProgramm = new StringBuffer(10000);
    public static boolean playerMeldungenAus = false;
    public static final int LOG_SYSTEM = ListenerMediathekView.EREIGNIS_LOG_SYSTEM;
    public static final int LOG_PLAYER = ListenerMediathekView.EREIGNIS_LOG_PLAYER;

    private static final int MAX_LAENGE_1 = 50000;
    private static final int MAX_LAENGE_2 = 30000;
    private static int zeilenNrSystem = 0;
    private static int zeilenNrProgramm = 0;
    public static PanelMeldungen panelMeldungenSystem = null;
    public static PanelMeldungen panelMeldungenPlayer = null;

    public static synchronized void startMeldungen() {
        MSLog.versionsMeldungen(MVFunctionSys.getProgName());
        MVLog.systemMeldung("Programmpfad: " + MVFunctionSys.getPathJar());
        MVLog.systemMeldung("Verzeichnis Einstellungen: " + Daten.getSettingsDirectory_String());
        MVLog.systemMeldung("");
        MVLog.systemMeldung("###########################################################");
        MVLog.systemMeldung("");
        MVLog.systemMeldung("");
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

    private static void systemmeldung(String[] texte) {
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
    }

    private static void playermeldung(String[] texte) {
        final String z = "  >>";
        System.out.println(z + " " + texte[0]);
        notifyPanelMeldung(LOG_PLAYER, texte[0]);
        for (int i = 1; i < texte.length; ++i) {
            System.out.println(z + " " + texte[i]);
            notifyPanelMeldung(LOG_PLAYER, texte[i]);
        }
    }

    public static void clearText(int art) {
        switch (art) {
            case LOG_SYSTEM:
                zeilenNrSystem = 0;
                textSystem.setLength(0);
                break;
            case LOG_PLAYER:
                zeilenNrProgramm = 0;
                textProgramm.setLength(0);
                break;
            default:
                break;
        }
    }

    private static void notifyPanelMeldung(final int art, String zeile) {
        switch (art) {
            case LOG_SYSTEM:
                addText(textSystem, "[" + getNr(zeilenNrSystem++) + "]   " + zeile);
                break;
            case LOG_PLAYER:
                addText(textProgramm, "[" + getNr(zeilenNrProgramm++) + "]   " + zeile);
                break;
            default:
                break;
        }
        try {
            if (SwingUtilities.isEventDispatchThread()) {
                // notify
                if (art == LOG_SYSTEM && panelMeldungenSystem != null) {
                    panelMeldungenSystem.notifyPanel();
                } else if (art == LOG_PLAYER && panelMeldungenPlayer != null) {
                    panelMeldungenPlayer.notifyPanel();
                }
            } else {
                SwingUtilities.invokeLater(() -> {
                    // notify
                    if (art == LOG_SYSTEM && panelMeldungenSystem != null) {
                        panelMeldungenSystem.notifyPanel();
                    } else if (art == LOG_PLAYER && panelMeldungenPlayer != null) {
                        panelMeldungenPlayer.notifyPanel();
                    }
                });
            }
        } catch (Exception ex) {
            MSLog.fehlerMeldung(698989743, ex);
        }
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
        if (text.length() > MAX_LAENGE_1) {
            text.delete(0, MAX_LAENGE_2);
        }
        text.append(texte);
        text.append(System.getProperty("line.separator"));
    }

    public static void LogDateiSchreiben(Daten ddaten, JFrame jFrame) {
        ArrayList<String> retList;
        DialogZiel dialog = new DialogZiel(jFrame, true, GuiFunktionen.getHomePath() + File.separator + "Mediathek.log", "Logdatei speichern");
        dialog.setVisible(true);
        if (!dialog.ok) {
            return;
        }
        Path logFilePath = Paths.get(dialog.ziel);
        try (final BufferedWriter bw = new BufferedWriter(new OutputStreamWriter(Files.newOutputStream(logFilePath)))) {
            // Programminfos
            bw.write("#####################################################");
            bw.newLine();
            bw.write("Erstellt: " + new SimpleDateFormat("dd.MM.yyyy, HH:mm").format(new Date()));
            bw.newLine();
            bw.write("#####################################################");
            bw.newLine();
            bw.newLine();
            bw.write(MVFunctionSys.getProgVersionString());
            bw.newLine();
            bw.write("Compiled: " + MVFunctionSys.getCompileDate());
            bw.newLine();
            bw.write("=====================================================");
            bw.newLine();
            bw.write("Java");
            bw.newLine();
            String[] java = MVFunctionSys.getJavaVersion();
            for (String ja : java) {
                bw.write(ja);
                bw.newLine();
            }
            bw.write("=====================================================");
            bw.newLine();
            bw.write("Betriebssystem: " + System.getProperty("os.name"));
            bw.newLine();
            bw.write("Bs-Version:     " + System.getProperty("os.version"));
            bw.newLine();
            bw.write("Bs-Architektur: " + System.getProperty("os.arch"));
            bw.newLine();
            bw.newLine();
            bw.write("Programmpfad: " + MVFunctionSys.getPathJar());
            bw.newLine();
            bw.write("Verzeichnis Einstellungen: " + Daten.getSettingsDirectory());
            bw.newLine();
            bw.newLine();
            bw.newLine();
            //
            bw.write("#####################################################");
            bw.newLine();
            bw.write("## Programmsets ##################################");
            bw.newLine();
            bw.write("#####################################################");
            bw.newLine();
            bw.newLine();
            for (int i = 0; i < Daten.listePset.size(); ++i) {
                bw.write(Daten.listePset.get(i).toString());
                bw.newLine();
            }
            bw.newLine();
            bw.newLine();
            bw.newLine();
            bw.newLine();
            //
            bw.write("#####################################################");
            bw.newLine();
            bw.write("## Systemmeldungen ##################################");
            bw.newLine();
            bw.write("#####################################################");
            bw.newLine();
            bw.newLine();
            bw.write(MVLog.textSystem.toString());
            bw.newLine();
            bw.newLine();
            bw.newLine();
            bw.newLine();
            //
            bw.write("#####################################################");
            bw.newLine();
            bw.write("## Programmausgabe ##################################");
            bw.newLine();
            bw.write("#####################################################");
            bw.newLine();
            bw.newLine();
            bw.write(MVLog.textProgramm.toString());
            bw.newLine();
            bw.newLine();
            bw.newLine();
            bw.newLine();
            //
            bw.write("#########################################################");
            bw.newLine();
            bw.write("## Fehlermeldungen                                       ");
            bw.newLine();
            retList = MSLog.fehlerMeldungen();
            for (String s : retList) {
                bw.write(s);
                bw.newLine();
            }
            bw.newLine();
            bw.newLine();
            //
            bw.close();
        } catch (Exception ex) {
            MSLog.fehlerMeldung(319865493, ex);
            MVMessageDialog.showMessageDialog(null, "Datei konnte nicht geschrieben werden!", "Fehler beim Schreiben", JOptionPane.ERROR_MESSAGE);
        }
    }

}
