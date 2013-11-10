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
package mediathek.gui;

import java.awt.Frame;
import java.awt.Point;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyEvent;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import javax.swing.AbstractAction;
import javax.swing.ActionMap;
import javax.swing.InputMap;
import javax.swing.JMenuItem;
import javax.swing.JOptionPane;
import javax.swing.JPopupMenu;
import javax.swing.KeyStroke;
import mediathek.MVStatusBar_Mac;
import mediathek.MediathekGui;
import mediathek.daten.Daten;
import mediathek.daten.DatenAbo;
import mediathek.gui.dialog.DialogEditAbo;
import mediathek.res.GetIcon;
import mediathek.tool.BeobTableHeader;
import mediathek.tool.CellRendererAbo;
import mediathek.tool.Datum;
import mediathek.tool.HinweisKeineAuswahl;
import mediathek.tool.ListenerMediathekView;
import mediathek.tool.MVJTable;
import mediathek.tool.TModelAbo;

public class GuiAbo extends PanelVorlage {

    public GuiAbo(Daten d, Frame parentComponent) {
        super(d, parentComponent);
        initComponents();
        tabelle = new MVJTable(MVJTable.TABELLE_TAB_ABOS);
        jScrollPane1.setViewportView(tabelle);
        initBeobachter();
        tabelleLaden();
        tabelle.initTabelle();
        if (tabelle.getRowCount() > 0) {
            tabelle.setRowSelectionInterval(0, 0);
        }
    }
    //===================================
    //public
    //===================================

    @Override
    public void isShown() {
        super.isShown();
        daten.mediathekGui.setToolbar(MediathekGui.UIButtonState.ABO);
        daten.mediathekGui.getStatusBar().setIndexForCenterDisplay(MVStatusBar_Mac.StatusbarIndex.ABO);
    }

    public void aendern() {
        aboAendern();
    }

    public void einAus(boolean ein) {
        aboEinAus(ein);
    }

    public void loeschen() {
        aboLoeschen();
    }

    //===================================
    //private
    //===================================
    private void initBeobachter() {
        ListenerMediathekView.addListener(new ListenerMediathekView(ListenerMediathekView.EREIGNIS_LISTE_ABOS, GuiAbo.class.getSimpleName()) {
            @Override
            public void ping() {
                tabelleLaden();
            }
        });
        tabelle.addMouseListener(new BeobMausTabelle1());
        tabelle.setDefaultRenderer(Object.class, new CellRendererAbo());
        tabelle.setDefaultRenderer(Datum.class, new CellRendererAbo());
        tabelle.setModel(new TModelAbo(new Object[][]{}, DatenAbo.COLUMN_NAMES));
        tabelle.getTableHeader().addMouseListener(new BeobTableHeader(tabelle, DatenAbo.COLUMN_NAMES, DatenAbo.spaltenAnzeigen) {
            @Override
            public void tabelleLaden_() {
                tabelleLaden();
            }
        });
        //aendern
        ActionMap am = tabelle.getActionMap();
        am.put("aendern", new BeobAbstractAction());
        InputMap im = tabelle.getInputMap();
        KeyStroke enter = KeyStroke.getKeyStroke(KeyEvent.VK_ENTER, 0);
        im.put(enter, "aendern");
    }

    private void tabelleLaden() {
        tabelle.getSpalten();
        daten.listeAbo.addObjectData((TModelAbo) tabelle.getModel());
        tabelle.setSpalten();
        setInfo();
    }

    private void aboLoeschen() {
        int rows[] = tabelle.getSelectedRows();
        if (rows.length > 0) {
            String text;
            if (rows.length == 1) {
                int delRow = tabelle.convertRowIndexToModel(rows[0]);
                text = "\"" + tabelle.getModel().getValueAt(delRow, DatenAbo.ABO_NAME_NR).toString() + "\" löschen?";
            } else {
                text = rows.length + " Abos löschen?";
            }
            int ret = JOptionPane.showConfirmDialog(parentComponent, text, "Löschen?", JOptionPane.YES_NO_OPTION);
            if (ret == JOptionPane.OK_OPTION) {
                for (int i = rows.length - 1; i >= 0; --i) {
                    int delRow = tabelle.convertRowIndexToModel(rows[i]);
                    ((TModelAbo) tabelle.getModel()).removeRow(delRow);
                    Daten.listeAbo.remove(delRow);
                }
            }
            tabelleLaden();
            zeileMarkieren(0);
            Daten.listeAbo.aenderungMelden();
        } else {
            new HinweisKeineAuswahl().zeigen(parentComponent);
        }
    }

    private void zeileMarkieren(int row) {
        if (tabelle.getRowCount() > 0) {
            // sonst ist schon eine Zeile markiert
            if (tabelle.getSelectedRow() == -1) {
                tabelle.requestFocus();
                tabelle.setRowSelectionInterval(row, row);
            }
        }
    }

    private void aboAendern() {
        int row = tabelle.getSelectedRow();
        if (row >= 0) {
            int delRow = tabelle.convertRowIndexToModel(row);
            DatenAbo akt = Daten.listeAbo.getAboNr(delRow);
            DatenAbo ret = akt.getCopy();
            DialogEditAbo dialog = new DialogEditAbo(null, true, daten, ret);
            dialog.setVisible(true);
            if (dialog.ok) {
                akt.aufMichKopieren(ret);
                tabelleLaden();
                Daten.listeAbo.aenderungMelden();
            }
            setInfo();
        } else {
            new HinweisKeineAuswahl().zeigen(parentComponent);
        }
    }

    private void aboEinAus() {
        int sel = tabelle.getSelectedRow();
        if (sel > 0) {
            int modelRow = tabelle.convertRowIndexToModel(sel);
            DatenAbo akt = Daten.listeAbo.getAboNr(modelRow);
            akt.arr[DatenAbo.ABO_EINGESCHALTET_NR] = Boolean.toString(!Boolean.parseBoolean(akt.arr[DatenAbo.ABO_EINGESCHALTET_NR]));
            tabelleLaden();
            tabelle.clearSelection();
            tabelle.addRowSelectionInterval(sel, sel);
            setInfo();
            Daten.listeAbo.aenderungMelden();
        } else {
            new HinweisKeineAuswahl().zeigen(parentComponent);
        }
    }

    private void aboEinAus(boolean ein) {
        int sel = tabelle.getSelectedRow();
        int[] rows = tabelle.getSelectedRows();
        if (rows.length > 0) {
            for (int row : rows) {
                int modelRow = tabelle.convertRowIndexToModel(row);
                DatenAbo akt = Daten.listeAbo.getAboNr(modelRow);
                akt.arr[DatenAbo.ABO_EINGESCHALTET_NR] = String.valueOf(ein);
            }
            tabelleLaden();
            tabelle.clearSelection();
            tabelle.requestFocus();
            for (int row : rows) {
                tabelle.addRowSelectionInterval(row, row);
            }
            setInfo();
            Daten.listeAbo.aenderungMelden();
        } else {
            new HinweisKeineAuswahl().zeigen(parentComponent);
        }
    }

    private void setInfo() {
        String textLinks;
        int ein = 0;
        int aus = 0;
        int gesamt = tabelle.getModel().getRowCount();
        for (int i = 0; i < tabelle.getModel().getRowCount(); ++i) {
            int modelRow = tabelle.convertRowIndexToModel(i);
            DatenAbo akt = daten.listeAbo.getAboNr(modelRow);
            if (akt.aboIstEingeschaltet()) {
                ++ein;
            } else {
                ++aus;
            }
        }
        if (gesamt == 1) {
            textLinks = "1 Abo, ";
        } else {
            textLinks = gesamt + " Abos, ";
        }
        textLinks += "(" + ein + " eingeschaltet, " + aus + " ausgeschaltet)";
        // Infopanel setzen
        daten.mediathekGui.getStatusBar().setTextLeft(MVStatusBar_Mac.StatusbarIndex.ABO, textLinks);
    }

    /**
     * This method is called from within the constructor to
     * initialize the form.
     * WARNING: Do NOT modify this code. The content of this method is
     * always regenerated by the Form Editor.
     */
    // <editor-fold defaultstate="collapsed" desc="Generated Code">//GEN-BEGIN:initComponents
    private void initComponents() {

        jLabel1 = new javax.swing.JLabel();
        jScrollPane1 = new javax.swing.JScrollPane();
        jTable1 = new javax.swing.JTable();

        jLabel1.setText("jLabel1");

        jTable1.setAutoCreateRowSorter(true);
        jTable1.setAutoResizeMode(javax.swing.JTable.AUTO_RESIZE_OFF);
        jScrollPane1.setViewportView(jTable1);

        javax.swing.GroupLayout layout = new javax.swing.GroupLayout(this);
        this.setLayout(layout);
        layout.setHorizontalGroup(
                layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                        .addGroup(layout.createSequentialGroup()
                                .addContainerGap()
                                .addComponent(jScrollPane1, javax.swing.GroupLayout.DEFAULT_SIZE, 576, Short.MAX_VALUE)
                                .addGap(14, 14, 14))
        );
        layout.setVerticalGroup(
                layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                        .addGroup(javax.swing.GroupLayout.Alignment.TRAILING, layout.createSequentialGroup()
                                .addContainerGap()
                                .addComponent(jScrollPane1, javax.swing.GroupLayout.DEFAULT_SIZE, 247, Short.MAX_VALUE)
                                .addContainerGap())
        );
    }// </editor-fold>//GEN-END:initComponents
    // Variables declaration - do not modify//GEN-BEGIN:variables
    private javax.swing.JLabel jLabel1;
    private javax.swing.JScrollPane jScrollPane1;
    private javax.swing.JTable jTable1;
    // End of variables declaration//GEN-END:variables

    private class BeobAbstractAction extends AbstractAction {

        @Override
        public void actionPerformed(ActionEvent e) {
            aendern();
        }
    }

    private class BeobMausTabelle1 extends MouseAdapter {

        private Point p;

        @Override
        public void mouseClicked(MouseEvent arg0) {
            if (arg0.getButton() == MouseEvent.BUTTON1) {
                if (arg0.getClickCount() == 1) {
                    p = arg0.getPoint();
                    int row = tabelle.rowAtPoint(p);
                    int column = tabelle.columnAtPoint(p);
                    if (row >= 0) {
                        buttonTable(row, column);
                    }
                } else if (arg0.getClickCount() > 1) {
                    aboAendern();
                }
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

        private void buttonTable(int row, int column) {
            if (row != -1) {
                if (column == DatenAbo.ABO_EINGESCHALTET_NR) {
                    aboEinAus();
                }
            }
        }

        private void showMenu(MouseEvent evt) {
            boolean ein = true;
            p = evt.getPoint();
            int nr = tabelle.rowAtPoint(p);
            if (nr >= 0) {
                tabelle.setRowSelectionInterval(nr, nr);
                int modelRow = tabelle.convertRowIndexToModel(nr);
                DatenAbo akt = daten.listeAbo.getAboNr(modelRow);
                ein = Boolean.parseBoolean(akt.arr[DatenAbo.ABO_EINGESCHALTET_NR]);
            }
            JPopupMenu jPopupMenu = new JPopupMenu();
            // Abo einschalten
            JMenuItem itemEinschalten = new JMenuItem("Abo einschalten");
            itemEinschalten.setIcon(GetIcon.getIcon("ja_16.png"));
            itemEinschalten.setEnabled(!ein);
            itemEinschalten.addActionListener(new ActionListener() {
                @Override
                public void actionPerformed(ActionEvent e) {
                    aboEinAus(true);
                }
            });
            jPopupMenu.add(itemEinschalten);
            // Abo deaktivieren
            JMenuItem itemDeaktivieren = new JMenuItem("Abo deaktivieren");
            itemDeaktivieren.setIcon(GetIcon.getIcon("nein_16.png"));
            itemDeaktivieren.setEnabled(ein);
            itemDeaktivieren.addActionListener(new ActionListener() {
                @Override
                public void actionPerformed(ActionEvent e) {
                    aboEinAus(false);
                }
            });
            jPopupMenu.add(itemDeaktivieren);
            //Abo lösschen
            JMenuItem itemLoeschen = new JMenuItem("Abo löschen");
            itemLoeschen.setIcon(GetIcon.getIcon("del_16.png"));
            itemLoeschen.addActionListener(new ActionListener() {
                @Override
                public void actionPerformed(ActionEvent e) {
                    aboLoeschen();
                }
            });
            jPopupMenu.add(itemLoeschen);
            //Abo ändern
            JMenuItem itemAendern = new JMenuItem("Abo ändern");
            itemAendern.setIcon(GetIcon.getIcon("configure_16.png"));
            itemAendern.addActionListener(new ActionListener() {
                @Override
                public void actionPerformed(ActionEvent e) {
                    aboAendern();
                }
            });
            jPopupMenu.add(itemAendern);

            //Menü anzeigen
            jPopupMenu.show(evt.getComponent(), evt.getX(), evt.getY());
        }
    }
}
