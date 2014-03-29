/*
 * MediathekView
 * Copyright (C) 2012 W. Xaver
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

import java.io.BufferedWriter;
import java.io.File;
import java.io.OutputStreamWriter;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import javax.swing.JFrame;
import javax.swing.JOptionPane;
import mediathek.daten.Daten;
import mediathek.gui.dialog.DialogZiel;
import mediathek.tool.DatumZeit;
import mediathek.tool.Funktionen;
import mediathek.tool.GuiFunktionen;
import mediathek.tool.MVMessageDialog;

public class ProgrammLog {

    public static void LogDateiSchreiben(Daten ddaten, JFrame jFrame) {
        DialogZiel dialog = new DialogZiel(jFrame, ddaten, true, GuiFunktionen.getHomePath() + File.separator + "Mediathek.log", "Logdatei speichern");
        dialog.setVisible(true);
        if (!dialog.ok) {
            return;
        }

        Path logFilePath = Paths.get(dialog.ziel);
        try (BufferedWriter bw = new BufferedWriter(new OutputStreamWriter(Files.newOutputStream(logFilePath)))) {
            // Programminfos
            bw.write("#####################################################");
            bw.newLine();
            bw.write("Erstellt: " + DatumZeit.getJetzt_ddMMyyyy_HHmm());
            bw.newLine();
            bw.write("#####################################################");
            bw.newLine();
            bw.newLine();
            bw.write(Funktionen.getProgVersionString());
            bw.newLine();
            bw.write("Compiled: " + Funktionen.getCompileDate());
            bw.newLine();
            bw.write("=====================================================");
            bw.newLine();
            bw.write("Java");
            bw.newLine();
            String[] java = Funktionen.getJavaVersion();
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
            bw.write("Programmpfad: " + Funktionen.getPathJar());
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
            for (int i = 0; i < ddaten.listePset.size(); ++i) {
                bw.write(ddaten.listePset.get(i).toString());
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
            bw.write(Log.textSystem.toString());
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
            bw.write(Log.textProgramm.toString());
            bw.newLine();
            bw.flush();
            bw.close();
        } catch (Exception ex) {
            Log.fehlerMeldung(319865493, Log.FEHLER_ART_PROG, "ProgrammLog.zeileSchreiben-1", ex);
            MVMessageDialog.showMessageDialog(null, "Datei konnte nicht geschrieben werden!",
                    "Fehler beim Schreiben", JOptionPane.ERROR_MESSAGE);
        }
    }
}
