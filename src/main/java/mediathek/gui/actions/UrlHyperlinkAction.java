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
package mediathek.gui.actions;

import mSearch.tool.Listener;
import mSearch.tool.Log;
import mediathek.config.MVConfig;
import mediathek.gui.dialog.DialogProgrammOrdnerOeffnen;

import javax.swing.*;
import java.awt.*;
import java.awt.event.ActionEvent;
import java.net.URI;
import java.net.URISyntaxException;

@SuppressWarnings("serial")
public class UrlHyperlinkAction extends AbstractAction {

    private final JFrame jFrameParent;

    public UrlHyperlinkAction(JFrame jjFrameParent, String url) throws URISyntaxException {
        jFrameParent = jjFrameParent;
        super.putValue(Action.NAME, url);
        super.putValue(SHORT_DESCRIPTION, url);
//        super.putValue(LONG_DESCRIPTION, url);
    }

    @Override
    public void actionPerformed(ActionEvent e) {
        try {
            openURL(jFrameParent, e.getActionCommand());
        } catch (URISyntaxException ignored) {
        }
    }

    /**
     * Try to open a browser window.
     * @param paFrame the parent window.
     * @param url URL to be opened. Here in string format.
     * @throws URISyntaxException when URL is malformed.
     */
    public static void openURL(JFrame paFrame, String url) throws URISyntaxException {
        if (Desktop.isDesktopSupported()) {
            Desktop d = Desktop.getDesktop();
            try {
                if (d.isSupported(Desktop.Action.BROWSE)) {
                    d.browse(new URI(url));
                    return;
                }
            } catch (Exception ignored) {
            }
            try {
                String programm = "";
                if (MVConfig.get(MVConfig.Configs.SYSTEM_URL_OEFFNEN).equals("")) {
                    String text = "\n Der Browser zum Anzeigen der URL wird nicht gefunden.\n Browser selbst auswählen.";
                    DialogProgrammOrdnerOeffnen dialog = new DialogProgrammOrdnerOeffnen(paFrame, true, "", "Browser suchen", text);
                    dialog.setVisible(true);
                    if (dialog.ok) {
                        programm = dialog.ziel;
                    }
                } else {
                    programm = MVConfig.get(MVConfig.Configs.SYSTEM_URL_OEFFNEN);
                }
                String[] cmd = {programm, url};
                Runtime.getRuntime().exec(cmd);
                MVConfig.add(MVConfig.Configs.SYSTEM_URL_OEFFNEN, programm);
                Listener.notify(Listener.EREIGNIS_PROGRAMM_OEFFNEN, UrlHyperlinkAction.class.getSimpleName());
            } catch (Exception ex) {
                MVConfig.add(MVConfig.Configs.SYSTEM_URL_OEFFNEN, ""); // dann wars wohl nix
                Log.errorLog(316497658, ex, "URL öffnen: " + url);
            }
        }
    }
}
