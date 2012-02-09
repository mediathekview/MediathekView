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

import mediathek.Konstanten;
import mediathek.Daten;
import mediathek.gui.dialog.DialogHinweis;

public class Hinweis {

    private Daten daten;

    public Hinweis(Daten ddaten) {
        daten = ddaten;
    }

    public void hinweisFlash() {
        if (Boolean.parseBoolean(daten.system[Konstanten.SYSTEM_HINWEIS_ANZEIGEN_NR])) {
            new DialogHinweis(null, true, daten, "Der Film kann möglicherweise nicht mit dem Programm\n"
                + "abgespielt werden.\n"
                + "\n"
                + "Für Flashfilme wird der flvstreamer benötigt.",
                              "http://zdfmediathk.sourceforge.net/flvstreamer.html").setVisible(true);
        }
    }

}
