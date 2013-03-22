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
import java.net.URI;
import mediathek.daten.DDaten;
import mediathek.daten.Daten;
import mediathek.gui.dialog.DialogProgrammOrdnerOeffnen;

public class UrlOeffnen {

    public static boolean urlOeffnen(DDaten ddaten, String url) {
        if (Desktop.isDesktopSupported()) {
            Desktop d = Desktop.getDesktop();
            try {
                if (d.isSupported(Desktop.Action.BROWSE)) {
                    d.browse(new URI(url));
                    return true;
                }
            } catch (Exception ex) {
                try {
                    String programm = "";
                    if (Daten.system[Konstanten.SYSTEM_URL_OEFFNEN_NR].equals("")) {
                        String text = "\n Der Browser zum Anzeigen der URL wird nicht gefunden.\n Browser selbst auswählen.";
                        DialogProgrammOrdnerOeffnen dialog = new DialogProgrammOrdnerOeffnen(ddaten.mediathekGui, ddaten, true, "", "Browser suchen", text);
                        dialog.setVisible(true);
                        if (dialog.ok) {
                            programm = dialog.ziel;
                        }
                    } else {
                        programm = Daten.system[Konstanten.SYSTEM_URL_OEFFNEN_NR];
                    }
                    Runtime.getRuntime().exec(programm + " " + url);
                    Daten.system[Konstanten.SYSTEM_URL_OEFFNEN_NR] = programm;
                    ListenerMediathekView.notify(ListenerMediathekView.EREIGNIS_PROGRAMM_OEFFNEN, UrlOeffnen.class.getSimpleName());
                } catch (Exception eex) {
                    Daten.system[Konstanten.SYSTEM_URL_OEFFNEN_NR] = ""; // dann wars wohl nix
                    Log.fehlerMeldung(316497658, Log.FEHLER_ART_PROG, UrlOeffnen.class.getName(), eex, "URL öffnen: " + url);
                }
            }
        }
        return false;
    }
}
