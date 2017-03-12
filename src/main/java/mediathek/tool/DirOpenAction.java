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

import de.mediathekview.mlib.tool.Listener;
import de.mediathekview.mlib.tool.Log;
import mediathek.config.MVConfig;
import mediathek.gui.GuiDownloads;
import mediathek.gui.dialog.DialogProgrammOrdnerOeffnen;

import javax.swing.*;
import java.awt.*;
import java.io.File;

public class DirOpenAction {

    public static void zielordnerOeffnen(Frame parent, String ordner) {
        boolean gut = false;
        File sFile = null;
        String[] arrProgCallArray = {"", ""};

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
            if (!MVConfig.get(MVConfig.Configs.SYSTEM_ORDNER_OEFFNEN).isEmpty()) {
                String programm = MVConfig.get(MVConfig.Configs.SYSTEM_ORDNER_OEFFNEN);
                arrProgCallArray[0] = programm;
                arrProgCallArray[1] = sFile.getAbsolutePath();
                Runtime.getRuntime().exec(arrProgCallArray);
                //Runtime.getRuntime().exec(programm + " " + sFile.getAbsolutePath());
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
                if (MVConfig.get(MVConfig.Configs.SYSTEM_ORDNER_OEFFNEN).isEmpty()) {
                    String text = "\n Der Dateimanager zum Anzeigen des Speicherordners wird nicht gefunden.\n Dateimanager selbst auswählen.";
                    DialogProgrammOrdnerOeffnen dialog = new DialogProgrammOrdnerOeffnen(parent, true, "", "Dateimanager suchen", text);
                    dialog.setVisible(true);
                    if (dialog.ok) {
                        programm = dialog.ziel;
                    }
                } else {
                    programm = MVConfig.get(MVConfig.Configs.SYSTEM_ORDNER_OEFFNEN);
                }
                if (sFile != null) {
                    arrProgCallArray[0] = programm;
                    arrProgCallArray[1] = sFile.getAbsolutePath();
                    Runtime.getRuntime().exec(arrProgCallArray);
                    //Runtime.getRuntime().exec(programm + " " + sFile.getAbsolutePath());
                    MVConfig.add(MVConfig.Configs.SYSTEM_ORDNER_OEFFNEN, programm);
                    Listener.notify(Listener.EREIGNIS_PROGRAMM_OEFFNEN, GuiDownloads.class.getSimpleName());
                    gut = true;
                }
            } catch (Exception eex) {
                Log.errorLog(306590789, ex, "Ordner öffnen: " + ordner);
            }
        } finally {
            if (!gut) {
                MVConfig.add(MVConfig.Configs.SYSTEM_ORDNER_OEFFNEN, "");
                Listener.notify(Listener.EREIGNIS_PROGRAMM_OEFFNEN, GuiDownloads.class.getSimpleName());
                MVMessageDialog.showMessageDialog(parent, "Kann den Dateimanager nicht öffnen!",
                        "Fehler", JOptionPane.ERROR_MESSAGE);
            }
        }
    }
}
