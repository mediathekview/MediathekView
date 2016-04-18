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

import java.awt.Desktop;
import java.awt.event.ActionEvent;
import java.net.URI;
import java.net.URISyntaxException;
import javax.swing.AbstractAction;
import javax.swing.Action;
import javax.swing.JFrame;
import mediathek.controller.Log;
import mediathek.daten.Daten;
import mediathek.gui.dialog.DialogProgrammOrdnerOeffnen;

public class UrlHyperlinkAction extends AbstractAction {

    String url;
    JFrame jFrameParent;

    public UrlHyperlinkAction(JFrame jjFrameParent, String uurl) throws URISyntaxException {
        url = uurl;
        jFrameParent = jjFrameParent;
        super.putValue(Action.NAME, uurl);
        super.putValue(SHORT_DESCRIPTION, url);
//        super.putValue(LONG_DESCRIPTION, url);
    }

    @Override
    public void actionPerformed(ActionEvent e) {
        try {
            urlOeffnen(jFrameParent, e.getActionCommand());
        } catch (URISyntaxException ignored) {
        }
    }

    public static void urlOeffnen(JFrame paFrame, String url) throws URISyntaxException {
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
                if (Daten.mVConfig.get(MVConfig.SYSTEM_URL_OEFFNEN).equals("")) {
                    String text = "\n Der Browser zum Anzeigen der URL wird nicht gefunden.\n Browser selbst auswählen.";
                    DialogProgrammOrdnerOeffnen dialog = new DialogProgrammOrdnerOeffnen(paFrame, true, "", "Browser suchen", text);
                    dialog.setVisible(true);
                    if (dialog.ok) {
                        programm = dialog.ziel;
                    }
                } else {
                    programm = Daten.mVConfig.get(MVConfig.SYSTEM_URL_OEFFNEN);
                }
                String[] cmd = {programm, url};
                Runtime.getRuntime().exec(cmd);
                Daten.mVConfig.add(MVConfig.SYSTEM_URL_OEFFNEN, programm);
                ListenerMediathekView.notify(ListenerMediathekView.EREIGNIS_PROGRAMM_OEFFNEN, UrlHyperlinkAction.class.getSimpleName());
            } catch (Exception ex) {
                Daten.mVConfig.add(MVConfig.SYSTEM_URL_OEFFNEN, ""); // dann wars wohl nix
                Log.fehlerMeldung(316497658, ex, "URL öffnen: " + url);
            }
        }
    }
}
