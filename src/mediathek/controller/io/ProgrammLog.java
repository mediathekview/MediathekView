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
package mediathek.controller.io;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileOutputStream;
import java.io.OutputStreamWriter;
import javax.swing.JOptionPane;
import mediathek.Daten;
import mediathek.Log;
import mediathek.daten.DDaten;
import mediathek.gui.dialog.DialogZiel;
import mediathek.tool.DatumZeit;
import mediathek.tool.GuiFunktionen;
import mediathek.tool.GuiFunktionenProgramme;

public class ProgrammLog {

    public static void LogDateiSchreiben(DDaten ddaten) {
        DialogZiel dialog = new DialogZiel(null, true, GuiFunktionen.getHomePath() + File.separator + "Mediathek.log");
        dialog.setVisible(true);
        if (dialog.ok) {
            File f = new File(dialog.ziel);
            if (f != null) {
                BufferedWriter bw = null;
                try {
                    bw = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(f)));
                    // Programminfos
                    bw.write("#####################################################");
                    bw.newLine();
                    bw.write("Erstellt: " + DatumZeit.getJetzt_ddMMyyyy_HHmm());
                    bw.newLine();
                    bw.write("#####################################################");
                    bw.newLine();
                    bw.newLine();
                    bw.write(Log.getCompileDate());
                    bw.newLine();
                    bw.write("Betriebssystem: " + System.getProperty("os.name"));
                    if (System.getProperty("os.name").toLowerCase().contains("windows")) {
                        if (System.getenv("ProgramFiles(x86)") != null) {
                            // dann 64Bit
                            bw.write("[64Bit]");
                        } else {
                            bw.write("[32Bit]");
                        }
                    }
                    bw.newLine();
                    bw.write("Programmpfad: " + GuiFunktionenProgramme.getPathJar());
                    bw.newLine();
                    bw.write("Verzeichnis Einstellungen: " + Daten.getBasisVerzeichnis());
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
                    bw.write(ddaten.textSystem.toString());
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
                    bw.write(ddaten.textProgramm.toString());
                    bw.newLine();
                    bw.flush();
                    bw.close();
                } catch (Exception ex) {
                    Log.fehlerMeldung("ProgrammLog.zeileSchreiben-1", ex);
                    JOptionPane.showMessageDialog(null, "Datei konnte nicht geschrieben werden!",
                            "Fehler beim Schreiben", JOptionPane.ERROR_MESSAGE);
                } finally {
                    try {
                        bw.close();
                    } catch (Exception ex) {
                    }
                }
            } else {
                JOptionPane.showMessageDialog(null, "Datei konnte nicht geschrieben werden!",
                        "Fehler beim Schreiben", JOptionPane.ERROR_MESSAGE);

            }
        }
    }
}
