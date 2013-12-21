/*    
 *    MediathekView
 *    Copyright (C) 2008   W. Xaver
 *    W.Xaver[at]googlemail.com
 *    http://zdfmediathk.sourceforge.net/
 *    
 *    This program is free software: you can redistribute it and/or modify
 *    it under the terms of the GNU General Public License as published by
 *    the Free Software Foundation, either version 3 of the License, or
 *    any later version.
 *
 *    This program is distributed in the hope that it will be useful,
 *    but WITHOUT ANY WARRANTY; without even the implied warranty of
 *    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *    GNU General Public License for more details.
 *
 *    You should have received a copy of the GNU General Public License
 *    along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */
package mediathek.tool;

import mediathek.controller.Log;
import java.awt.Desktop;
import java.awt.Frame;
import java.io.File;
import javax.swing.JOptionPane;
import mediathek.daten.Daten;
import mediathek.gui.GuiDownloads;
import mediathek.gui.dialog.DialogProgrammOrdnerOeffnen;

public class DirOpenAction {

    public static void zielordnerOeffnen(Frame parent, String ordner) {
        boolean gut = false;
        File sFile = null;
        if (ordner.isEmpty()) {
            return;
        }
        if (!ordner.endsWith(File.separator)) {
            ordner += File.separator;
        }
        try {
            sFile = new File(ordner);
            if (!sFile.exists()) {
                sFile = sFile.getParentFile();
            }
            if (!Daten.system[Konstanten.SYSTEM_ORDNER_OEFFNEN_NR].isEmpty()) {
                String programm = Daten.system[Konstanten.SYSTEM_ORDNER_OEFFNEN_NR];
                Runtime.getRuntime().exec(programm + " " + sFile.getAbsolutePath());
                gut = true;
            } else {
                if (Desktop.isDesktopSupported()) {
                    Desktop d = Desktop.getDesktop();
                    if (d.isSupported(Desktop.Action.OPEN)) {
                        d.open(sFile);
                        gut = true;
                    }
                }
            }
        } catch (Exception ex) {
            try {
                gut = false;
                String programm = "";
                if (Daten.system[Konstanten.SYSTEM_ORDNER_OEFFNEN_NR].equals("")) {
                    String text = "\n Der Dateimanager zum Anzeigen des Speicherordners wird nicht gefunden.\n Dateimanager selbst auswählen.";
                    DialogProgrammOrdnerOeffnen dialog = new DialogProgrammOrdnerOeffnen(parent, true, "", "Dateimanager suchen", text);
                    dialog.setVisible(true);
                    if (dialog.ok) {
                        programm = dialog.ziel;
                    }
                } else {
                    programm = Daten.system[Konstanten.SYSTEM_ORDNER_OEFFNEN_NR];
                }
                if (sFile != null) {
                    Runtime.getRuntime().exec(programm + " " + sFile.getAbsolutePath());
                    Daten.system[Konstanten.SYSTEM_ORDNER_OEFFNEN_NR] = programm;
                    ListenerMediathekView.notify(ListenerMediathekView.EREIGNIS_PROGRAMM_OEFFNEN, GuiDownloads.class.getSimpleName());
                    gut = true;
                }
            } catch (Exception eex) {
                Log.fehlerMeldung(306590789, Log.FEHLER_ART_PROG, GuiDownloads.class.getName(), ex, "Ordner öffnen: " + ordner);
            }
        } finally {
            if (!gut) {
                Daten.system[Konstanten.SYSTEM_ORDNER_OEFFNEN_NR] = "";
                ListenerMediathekView.notify(ListenerMediathekView.EREIGNIS_PROGRAMM_OEFFNEN, GuiDownloads.class.getSimpleName());
                MVMessageDialog.showMessageDialog(parent, "Kann den Dateimanager nicht öffnen!",
                        "Fehler", JOptionPane.ERROR_MESSAGE);
            }
        }
    }
}
