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
package mediathek.tool.listener;

import mediathek.config.MVConfig;
import mediathek.tool.table.MVTable;

import javax.swing.*;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;

/**
 * Rechte Maustaste in der Tabelle (Kontextmenü)
 */
public class BeobTableHeader extends MouseAdapter {
    private final MVTable tabelle;
    private final String[] columns;
    private final boolean[] spaltenAnzeigen;
    private final int[] ausblenden;
    private final int[] button;
    private final boolean icon;
    private final MVConfig.Configs configs;
    private JCheckBoxMenuItem[] box;

    public BeobTableHeader(MVTable tabelle, boolean[] spalten, int[] aausblenden, int[] bbutton, boolean icon, MVConfig.Configs configs) {
        this.tabelle = tabelle;
        this.icon = icon;
        spaltenAnzeigen = spalten;
        this.ausblenden = aausblenden;
        this.configs = configs;
        button = bbutton;

        //dynamically query column names from table
        final var colModel = tabelle.getTableHeader().getColumnModel();
        final int colCount = colModel.getColumnCount();
        columns = new String[colCount];
        for (int index = 0; index < colCount; index++) {
            columns[index] = (String) colModel.getColumn(index).getHeaderValue();
        }
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
            box[i].addActionListener(e -> setSpalten());
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
            jPopupMenu.addSeparator();

            final JCheckBoxMenuItem item3 = new JCheckBoxMenuItem("Sendericons anzeigen");
            item3.setSelected(tabelle.showSenderIcons());
            item3.addActionListener(e -> {
                tabelle.setShowIcon(item3.isSelected());
                setSpalten();
            });
            jPopupMenu.add(item3);

            final JCheckBoxMenuItem item2 = new JCheckBoxMenuItem("kleine Sendericons anzeigen");
            item2.setSelected(tabelle.useSmallSenderIcons);
            if (!tabelle.showSenderIcons()) {
                item2.setEnabled(false);
            } else {
                item2.addActionListener(e -> {
                    tabelle.useSmallSenderIcons = item2.isSelected();
                    setSpalten();
                });
            }
            jPopupMenu.add(item2);
        }

        jPopupMenu.addSeparator();
        // Tabellenspalten umbrechen
        JCheckBoxMenuItem itemBr = new JCheckBoxMenuItem("Zeilen umbrechen");
        itemBr.setSelected(tabelle.isLineBreak());
        itemBr.addActionListener(e -> {
            tabelle.setLineBreak(itemBr.isSelected());
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
