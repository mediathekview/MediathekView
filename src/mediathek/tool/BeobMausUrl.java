/*
 * MediathekView
 * Copyright (C) 2008 W. Xaver
 * W.Xaver[at]googlemail.com
 * http://zdfmediathk.sourceforge.net/
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */

package mediathek.tool;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import javax.swing.JButton;
import javax.swing.JMenuItem;
import javax.swing.JPopupMenu;

/**
 *  This class provides a "copy URL" popup menu for a given {@link javax.swing.JButton}.
 */
public class BeobMausUrl extends MouseAdapter {
    private final BeobUrl beobUrl = new BeobUrl();
    private final JButton link;

    public BeobMausUrl(JButton link) {
        this.link = link;
    }

    @Override
    public void mousePressed(MouseEvent arg0) {
        if (arg0.isPopupTrigger()) {
            showMenu(arg0);
        }
    }

    @Override
    public void mouseReleased(MouseEvent arg0) {
        if (arg0.isPopupTrigger()) {
            showMenu(arg0);
        }
    }

    private void showMenu(MouseEvent evt) {
        JPopupMenu jPopupMenu = new JPopupMenu();

        //Url
        JMenuItem item = new JMenuItem("URL kopieren");
        item.addActionListener(beobUrl);
        jPopupMenu.add(item);
        //anzeigen
        jPopupMenu.show(evt.getComponent(), evt.getX(), evt.getY());
    }

    private class BeobUrl implements ActionListener {

        @Override
        public void actionPerformed(ActionEvent e) {
            GuiFunktionen.copyToClipboard(link.getText());
        }
    }
}
