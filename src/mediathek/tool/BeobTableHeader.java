/*
 * MediathekView
 * Copyright (C) 2013 W. Xaver
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
import javax.swing.JCheckBoxMenuItem;
import javax.swing.JMenuItem;
import javax.swing.JPopupMenu;

public class BeobTableHeader extends MouseAdapter {
    //rechhte Maustaste in der Tabelle

    MVJTable tabelle;
    String[] columns;
    boolean[] spaltenAnzeigen;
    JCheckBoxMenuItem[] box;
    int[] immerAnzeigen;

    public BeobTableHeader(MVJTable tabelle, String[] columns, boolean[] spalten, int[] iimmerAnzeigen) {
        this.tabelle = tabelle;
        this.columns = columns;
        spaltenAnzeigen = spalten;
        immerAnzeigen = iimmerAnzeigen;
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

    private boolean immer(int i) {
        for (int ii : immerAnzeigen) {
            if (i == ii) {
                return true;
            }
        }
        return false;
    }

    private void showMenu(MouseEvent evt) {
        JPopupMenu jPopupMenu = new JPopupMenu();
        // Spalten ein-ausschalten
        box = new JCheckBoxMenuItem[this.columns.length];
        for (int i = 0; i < columns.length; ++i) {
            if (immer(i)) {
                continue;
            }
            box[i] = new JCheckBoxMenuItem(columns[i]);
            box[i].setSelected(anzeigen(i));
            box[i].addActionListener(new ActionListener() {
                @Override
                public void actionPerformed(ActionEvent e) {
                    setSpalten();
                }
            });
            jPopupMenu.add(box[i]);
        }
        //##Trenner##
        jPopupMenu.addSeparator();
        //##Trenner##
        // Tabellenspalten zurücksetzen
        JMenuItem item = new JMenuItem("Spalten zurücksetzen");
        item.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                tabelle.resetTabelle();
            }
        });
        jPopupMenu.add(item);
        //anzeigen
        jPopupMenu.show(evt.getComponent(), evt.getX(), evt.getY());
    }

    private boolean anzeigen(int i) {
        if (spaltenAnzeigen == null) {
            return true;
        } else {
            return spaltenAnzeigen[i];
        }
    }

    private void setSpalten() {
        for (int i = 0; i < box.length; ++i) {
            if (box[i] != null) {
                spaltenAnzeigen[i] = box[i].isSelected();
            }
        }
        tabelle.spaltenEinAus();
        tabelleLaden_();
    }

    public void tabelleLaden_() {
    }
}
