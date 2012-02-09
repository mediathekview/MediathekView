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
package mediathek.gui;

import java.util.LinkedList;
import java.util.ListIterator;

public class PanelListe {

    LinkedList<PanelVorlage> liste;

    public PanelListe() {
        liste = new LinkedList<PanelVorlage>();
    }

    public void addPanel(PanelVorlage p) {
        liste.add(p);
    }

    public void aendern() {
        PanelVorlage panelVorlage = null;
        ListIterator<PanelVorlage> it = liste.listIterator(0);
        while (it.hasNext()) {
            panelVorlage = it.next();
            panelVorlage.geaendert = true;
        }
    }

    public void aendernSofort() {
        PanelVorlage panelVorlage = null;
        ListIterator<PanelVorlage> it = liste.listIterator(0);
        while (it.hasNext()) {
            panelVorlage = it.next();
            panelVorlage.geaendert = true;
            panelVorlage.neuLadenSofort();
        }
    }
}
