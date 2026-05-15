/*
 * Copyright (c) 2026 derreisende77.
 * This code was developed as part of the MediathekView project https://github.com/mediathekview/MediathekView
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package mediathek.gui.actions;

import mediathek.config.MVConfig;
import mediathek.gui.dialog.DialogProgrammOrdnerOeffnen;
import mediathek.gui.messages.ProgramLocationChangedEvent;
import mediathek.tool.MessageBus;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import javax.swing.*;
import java.awt.*;
import java.io.File;

public class OpenPlayerAction {
    private static final Logger logger = LogManager.getLogger(OpenPlayerAction.class);

    public static void filmAbspielen(Frame parent, String datei) {
        boolean gut = false;
        File sFile;
        if (datei.isEmpty()) {
            return;
        }
        sFile = new File(datei);
        if (!sFile.exists()) {
            JOptionPane.showMessageDialog(parent, "Film existiert noch nicht!",
                    "Fehler", JOptionPane.ERROR_MESSAGE);
            return;
        }
        try {
            if (!MVConfig.get(MVConfig.Configs.SYSTEM_PLAYER_ABSPIELEN).isEmpty()) {
                String programm = MVConfig.get(MVConfig.Configs.SYSTEM_PLAYER_ABSPIELEN);
                String[] cmd = {programm, sFile.getAbsolutePath()};
                Runtime.getRuntime().exec(cmd);
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
                String text = "\n Ein Videoplayer zum Abspielen wird nicht gefunden.\n Videoplayer selbst auswählen.";
                DialogProgrammOrdnerOeffnen dialog = new DialogProgrammOrdnerOeffnen(parent, true, "", "Videoplayer suchen", text);
                dialog.setVisible(true);
                if (dialog.ok) {
                    programm = dialog.ziel;
                }
                String[] cmd = {programm, sFile.getAbsolutePath()};
                Runtime.getRuntime().exec(cmd);
                MVConfig.add(MVConfig.Configs.SYSTEM_PLAYER_ABSPIELEN, programm);
                MessageBus.getMessageBus().publishAsync(new ProgramLocationChangedEvent());
                gut = true;
            } catch (Exception eex) {
                logger.error("Ordner öffnen: {}", datei, ex);
            }
        } finally {
            if (!gut) {
                MVConfig.add(MVConfig.Configs.SYSTEM_PLAYER_ABSPIELEN, "");
                MessageBus.getMessageBus().publishAsync(new ProgramLocationChangedEvent());
                JOptionPane.showMessageDialog(parent, "Kann den Videoplayer nicht öffnen!",
                        "Fehler", JOptionPane.ERROR_MESSAGE);
            }
        }
    }
}
