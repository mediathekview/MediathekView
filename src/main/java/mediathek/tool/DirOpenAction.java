package mediathek.tool;

import mSearch.tool.Log;
import mediathek.config.Daten;
import mediathek.config.MVConfig;
import mediathek.gui.dialog.DialogProgrammOrdnerOeffnen;
import mediathek.gui.messages.ProgramLocationChangedEvent;

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

                    MVConfig.add(MVConfig.Configs.SYSTEM_ORDNER_OEFFNEN, programm);
                    Daten.getInstance().getMessageBus().publishAsync(new ProgramLocationChangedEvent());
                    gut = true;
                }
            } catch (Exception eex) {
                Log.errorLog(306590789, ex, "Ordner öffnen: " + ordner);
            }
        } finally {
            if (!gut) {
                MVConfig.add(MVConfig.Configs.SYSTEM_ORDNER_OEFFNEN, "");
                Daten.getInstance().getMessageBus().publishAsync(new ProgramLocationChangedEvent());
                MVMessageDialog.showMessageDialog(parent, "Kann den Dateimanager nicht öffnen!",
                        "Fehler", JOptionPane.ERROR_MESSAGE);
            }
        }
    }
}
