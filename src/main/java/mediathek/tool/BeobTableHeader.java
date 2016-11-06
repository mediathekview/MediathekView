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

import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import javax.swing.JCheckBoxMenuItem;
import javax.swing.JMenuItem;
import javax.swing.JPopupMenu;
import mediathek.config.MVConfig;

public class BeobTableHeader extends MouseAdapter {
    //rechhte Maustaste in der Tabelle

    MVTable tabelle;
    String[] columns;
    boolean[] spaltenAnzeigen;
    JCheckBoxMenuItem[] box;
    int[] ausblenden;
    int[] button;
    boolean icon = false;
    MVConfig.Configs configs;

    public BeobTableHeader(MVTable tabelle, String[] columns, boolean[] spalten, int[] aausblenden, int[] bbutton, boolean icon, MVConfig.Configs configs) {
        this.tabelle = tabelle;
        this.columns = columns;
        this.icon = icon;
        spaltenAnzeigen = spalten;
        this.ausblenden = aausblenden;
        this.configs = configs;
        button = bbutton;
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
        for (int ii : ausblenden) {
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
            box[i].addActionListener(e -> {
                setSpalten();
            });
            jPopupMenu.add(box[i]);
        }
        // jetzt evtl. noch die Button
        if (button.length > 0) {
            //##Trenner##
            jPopupMenu.addSeparator();
            //##Trenner##
            final JCheckBoxMenuItem item2 = new JCheckBoxMenuItem("Button anzeigen");
            item2.setSelected(anzeigen(button[0])); //entweder alle oder keiner!
            item2.addActionListener(e -> {
                for (int i : button) {
                    setSpalten(i, item2.isSelected());
                }
            });
            jPopupMenu.add(item2);
        }
        if (icon) {
            //##Trenner##
            jPopupMenu.addSeparator();
            final JCheckBoxMenuItem item3 = new JCheckBoxMenuItem("Icons anzeigen");
            item3.setSelected(tabelle.iconAnzeigen);
            item3.addActionListener(e -> {
                tabelle.iconAnzeigen = item3.isSelected();
                setSpalten();
            });
            jPopupMenu.add(item3);
            final JCheckBoxMenuItem item2 = new JCheckBoxMenuItem("kleine Icons anzeigen");
            item2.setSelected(tabelle.iconKlein);
            if (!tabelle.iconAnzeigen) {
                item2.setEnabled(false);
            } else {
                item2.addActionListener(e -> {
                    tabelle.iconKlein = item2.isSelected();
                    setSpalten();
                });
            }
            jPopupMenu.add(item2);
        }
        //##Trenner##
        jPopupMenu.addSeparator();
        // Tabellenspalten umbrechen
        JCheckBoxMenuItem itemBr = new JCheckBoxMenuItem("Zeilen umbrechen");
        itemBr.setSelected(tabelle.lineBreak);
        itemBr.addActionListener(e -> {
            tabelle.lineBreak = itemBr.isSelected();
            MVConfig.add(configs, Boolean.toString(itemBr.isSelected()));
            setSpalten();
        });
        jPopupMenu.add(itemBr);

        //##Trenner##
        jPopupMenu.addSeparator();
        // Tabellenspalten zurücksetzen
        JMenuItem item1 = new JMenuItem("Spalten zurücksetzen");
        item1.addActionListener(e -> tabelle.resetTabelle());
        jPopupMenu.add(item1);
        //anzeigen
        jPopupMenu.show(evt.getComponent(), evt.getX(), evt.getY());
    }

    private boolean anzeigen(int i) {
        return spaltenAnzeigen == null || spaltenAnzeigen[i];
    }

    private void setSpalten() {
        for (int i = 0; i < box.length; ++i) {
            if (box[i] != null) {
                spaltenAnzeigen[i] = box[i].isSelected();
            }
        }
        tabelle.spaltenEinAus();
        tabelle.setHeight();
    }

    private void setSpalten(int k, boolean anz) {
        spaltenAnzeigen[k] = anz;
        tabelle.spaltenEinAus();
    }

}
