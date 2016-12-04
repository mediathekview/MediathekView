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

import mediathek.config.Daten;
import mediathek.tool.MVTable;

import javax.swing.*;

@SuppressWarnings("serial")
public class PanelVorlage extends JPanel {
    public Daten daten;
    public boolean stopBeob = false;
    public JFrame parentComponent = null;
    MVTable tabelle = null;
    public boolean solo = false; // nicht in einem eigenem Frame

    public PanelVorlage(Daten d, JFrame pparentComponent) {
        daten = d;
        parentComponent = pparentComponent;
        addComponentListener(new java.awt.event.ComponentAdapter() {
            @Override
            public void componentShown(java.awt.event.ComponentEvent evt) {
                isShown();
            }
        });
    }

    public void isShown() {
        //FIXME Das kann nicht wirklich eine korrekte Sache sein hier...
        // immer wenn isShown
    }

    public void tabelleSpeichern() {
        if (tabelle != null) {
            tabelle.tabelleNachDatenSchreiben();
        }
    }
}
