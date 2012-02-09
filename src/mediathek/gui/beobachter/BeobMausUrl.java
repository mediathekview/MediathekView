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

package mediathek.gui.beobachter;

import java.awt.Point;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import javax.swing.JMenuItem;
import javax.swing.JPopupMenu;
import javax.swing.JTable;
import mediathek.tool.GuiFunktionen;
import mediathek.tool.GuiKonstanten;
import mediathek.daten.DatenFilm;

public class BeobMausUrl extends MouseAdapter {

    private BeobUrl beobUrl = new BeobUrl();
    private Point p;
    private JTable tabelle;

    public BeobMausUrl(JTable ttabelle) {
        tabelle = ttabelle;
    }

    private class BeobUrl implements ActionListener {

        @Override
        public void actionPerformed(ActionEvent e) {
            int nr = tabelle.rowAtPoint(p);
            if (nr >= 0) {
                GuiFunktionen.copyToClipboard(
                        tabelle.getModel().getValueAt(tabelle.convertRowIndexToModel(nr),
                        DatenFilm.FILM_URL_NR).toString());
            }
        }
    }

    @Override
    public void mouseClicked(MouseEvent arg0) {
        if (arg0.getButton() == MouseEvent.BUTTON1) {
        } else if (arg0.getButton() == MouseEvent.BUTTON3) {
            showMenu(arg0);
        }
    }

    private void showMenu(MouseEvent evt) {
        p = evt.getPoint();
        int nr = tabelle.rowAtPoint(p);
        if (nr >= 0) {
            tabelle.setRowSelectionInterval(nr, nr);
        }
        JPopupMenu menu = new JPopupMenu();
        JMenuItem itemVor = new JMenuItem("URL kopieren");
        itemVor.addActionListener(beobUrl);
        menu.add(itemVor);
        menu.show(evt.getComponent(), evt.getX(), evt.getY());
    }
}
