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

import java.awt.BorderLayout;
import java.awt.Point;
import java.awt.Toolkit;
import java.awt.event.ActionEvent;
import java.awt.event.KeyEvent;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import javax.swing.*;
import mSearch.tool.Datum;
import mSearch.tool.Listener;
import mSearch.tool.MVConfig;
import mediathek.daten.Daten;
import mediathek.daten.DatenAbo;
import mediathek.gui.dialog.DialogEditAbo;
import mediathek.res.GetIcon;
import mediathek.tool.*;

public class GuiAbo extends PanelVorlage {

    private ToolBar toolBar;

    public GuiAbo(Daten d, JFrame parentComponent) {
        super(d, parentComponent);
        initComponents();
        tabelle = new MVTable(MVTable.TableType.ABOS);
        jScrollPane1.setViewportView(tabelle);
        initBeobachter();
        tabelleLaden();
        tabelle.initTabelle();
        if (tabelle.getRowCount() > 0) {
            tabelle.setRowSelectionInterval(0, 0);
        }

        toolBar = new ToolBar(daten, ToolBar.TOOLBAR_TAB_ABOS);
        jPanelToolBar.setLayout(new BorderLayout());
        jPanelToolBar.add(toolBar, BorderLayout.CENTER);
        setToolbarVisible();
    }
    //===================================
    //public
    //===================================

    @Override
    public void isShown() {
        super.isShown();
        if (!solo) {
            daten.mediathekGui.setToolbar(ToolBar.TOOLBAR_TAB_ABOS);
            daten.mediathekGui.getStatusBar().setIndexForLeftDisplay(MVStatusBar.StatusbarIndex.ABO);
        }
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

    public void neu() {
        aboNeu();
    }

    //===================================
    //private
    //===================================
    private void setToolbarVisible() {
        toolBar.setVisible(Boolean.parseBoolean(MVConfig.get(MVConfig.SYSTEM_TOOLBAR_ALLES_ANZEIGEN)));
    }
    private void initBeobachter() {
        Listener.addListener(new Listener(Listener.EREIGNIS_TOOLBAR_VIS, GuiAbo.class.getSimpleName()) {
            @Override
            public void ping() {
                setToolbarVisible();
            }
        });
        Listener.addListener(new Listener(Listener.EREIGNIS_LISTE_ABOS, GuiAbo.class.getSimpleName()) {
            @Override
            public void ping() {
                tabelleLaden();
            }
        });
        tabelle.addMouseListener(new BeobMausTabelle1());
        tabelle.setDefaultRenderer(Object.class, new CellRendererAbo());
        tabelle.setDefaultRenderer(Datum.class, new CellRendererAbo());
        tabelle.setDefaultRenderer(Integer.class, new CellRendererAbo());
        tabelle.setModel(new TModelAbo(new Object[][]{}, DatenAbo.COLUMN_NAMES));
        tabelle.getTableHeader().addMouseListener(new BeobTableHeader(tabelle, DatenAbo.COLUMN_NAMES, DatenAbo.spaltenAnzeigen,
                new int[]{DatenAbo.ABO_EINGESCHALTET},
                new int[]{},
                true /*Icon*/));
        this.getInputMap(JComponent.WHEN_IN_FOCUSED_WINDOW).put(KeyStroke.getKeyStroke(KeyEvent.VK_T, Toolkit.getDefaultToolkit().getMenuShortcutKeyMask()), "tabelle");
        this.getActionMap().put("tabelle", new AbstractAction() {
            @Override
            public void actionPerformed(ActionEvent e) {
                tabelle.requestFocusSelelct(jScrollPane1);
            }
        });
        //aendern
        ActionMap am = tabelle.getActionMap();
        InputMap im = tabelle.getInputMap();
        im.put(KeyStroke.getKeyStroke(KeyEvent.VK_ENTER, 0), "aendern");
        am.put("aendern", new AbstractAction() {

            @Override
            public void actionPerformed(ActionEvent e) {
                aendern();
            }
        });
    }

    private void tabelleLaden() {
        tabelle.getSpalten();
        Daten.listeAbo.addObjectData((TModelAbo) tabelle.getModel());
        tabelle.setSpalten();
        setInfo();
    }

    private void aboLoeschen() {
        int rows[] = tabelle.getSelectedRows();
        if (rows.length > 0) {
            String text;
            if (rows.length == 1) {
                int delRow = tabelle.convertRowIndexToModel(rows[0]);
                text = "\"" + tabelle.getModel().getValueAt(delRow, DatenAbo.ABO_NAME).toString() + "\" löschen?";
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
            int modelRow = tabelle.convertRowIndexToModel(row);
            DatenAbo akt = Daten.listeAbo.getAboNr(modelRow);
            DialogEditAbo dialog = new DialogEditAbo(daten.mediathekGui, true, daten, akt);
            dialog.setVisible(true);
            if (dialog.ok) {
                tabelleLaden();
                Daten.listeAbo.aenderungMelden();
            }
            setInfo();
        } else {
            new HinweisKeineAuswahl().zeigen(parentComponent);
        }
    }

    private void aboNeu() {
        Daten.listeAbo.addAbo("Neu" /*Abonamer*/);
    }

    private void aboEinAus(boolean ein) {
        final int[] rows = tabelle.getSelectedRows();
        if (rows.length > 0) {
            for (int row : rows) {
                int modelRow = tabelle.convertRowIndexToModel(row);
                DatenAbo akt = Daten.listeAbo.getAboNr(modelRow);
                akt.arr[DatenAbo.ABO_EINGESCHALTET] = String.valueOf(ein);
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
        // Infopanel setzen
        daten.mediathekGui.getStatusBar().setTextForLeftDisplay();
    }

    /**
     * This method is called from within the constructor to
     * initialize the form.
     * WARNING: Do NOT modify this code. The content of this method is
     * always regenerated by the Form Editor.
     */
    // <editor-fold defaultstate="collapsed" desc="Generated Code">//GEN-BEGIN:initComponents
    private void initComponents() {

        jPanelToolBar = new javax.swing.JPanel();
        jScrollPane1 = new javax.swing.JScrollPane();
        javax.swing.JTable jTable1 = new javax.swing.JTable();

        javax.swing.GroupLayout jPanelToolBarLayout = new javax.swing.GroupLayout(jPanelToolBar);
        jPanelToolBar.setLayout(jPanelToolBarLayout);
        jPanelToolBarLayout.setHorizontalGroup(
            jPanelToolBarLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGap(0, 416, Short.MAX_VALUE)
        );
        jPanelToolBarLayout.setVerticalGroup(
            jPanelToolBarLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGap(0, 13, Short.MAX_VALUE)
        );

        jTable1.setAutoCreateRowSorter(true);
        jTable1.setAutoResizeMode(javax.swing.JTable.AUTO_RESIZE_OFF);
        jScrollPane1.setViewportView(jTable1);

        javax.swing.GroupLayout layout = new javax.swing.GroupLayout(this);
        this.setLayout(layout);
        layout.setHorizontalGroup(
            layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addComponent(jScrollPane1, javax.swing.GroupLayout.PREFERRED_SIZE, 0, Short.MAX_VALUE)
            .addComponent(jPanelToolBar, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
        );
        layout.setVerticalGroup(
            layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(layout.createSequentialGroup()
                .addComponent(jPanelToolBar, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                .addComponent(jScrollPane1, javax.swing.GroupLayout.DEFAULT_SIZE, 296, Short.MAX_VALUE))
        );
    }// </editor-fold>//GEN-END:initComponents
    // Variables declaration - do not modify//GEN-BEGIN:variables
    private javax.swing.JPanel jPanelToolBar;
    private javax.swing.JScrollPane jScrollPane1;
    // End of variables declaration//GEN-END:variables

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
                if (tabelle.convertColumnIndexToModel(column) == DatenAbo.ABO_EINGESCHALTET) {
                    DatenAbo akt = Daten.listeAbo.getAboNr(tabelle.convertRowIndexToModel(row));
                    akt.arr[DatenAbo.ABO_EINGESCHALTET] = Boolean.toString(!Boolean.parseBoolean(akt.arr[DatenAbo.ABO_EINGESCHALTET]));
                    tabelle.getSpalten();
                    tabelleLaden();
                    tabelle.setSpalten();
//                    tabelle.clearSelection();
//                    tabelle.addRowSelectionInterval(row, row);
                    setInfo();
                    Daten.listeAbo.aenderungMelden();
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
                DatenAbo akt = Daten.listeAbo.getAboNr(modelRow);
                ein = Boolean.parseBoolean(akt.arr[DatenAbo.ABO_EINGESCHALTET]);
            }
            JPopupMenu jPopupMenu = new JPopupMenu();
            // Abo einschalten
            JMenuItem itemEinschalten = new JMenuItem("Abo einschalten");
            itemEinschalten.setIcon(GetIcon.getProgramIcon("ja_16.png"));
            itemEinschalten.setEnabled(!ein);
            itemEinschalten.addActionListener(e -> aboEinAus(true));
            jPopupMenu.add(itemEinschalten);
            // Abo deaktivieren
            JMenuItem itemDeaktivieren = new JMenuItem("Abo ausschalten");
            itemDeaktivieren.setIcon(GetIcon.getProgramIcon("nein_16.png"));
            itemDeaktivieren.setEnabled(ein);
            itemDeaktivieren.addActionListener(e -> aboEinAus(false));
            jPopupMenu.add(itemDeaktivieren);
            //Abo lösschen
            JMenuItem itemLoeschen = new JMenuItem("Abo löschen");
            itemLoeschen.setIcon(GetIcon.getProgramIcon("del_16.png"));
            itemLoeschen.addActionListener(e -> aboLoeschen());
            jPopupMenu.add(itemLoeschen);
            //Abo ändern
            JMenuItem itemAendern = new JMenuItem("Abo ändern");
            itemAendern.setIcon(GetIcon.getProgramIcon("configure_16.png"));
            itemAendern.addActionListener(e -> aboAendern());
            jPopupMenu.add(itemAendern);

            //##Trenner##
            jPopupMenu.addSeparator();
            //Abo ändern
            JMenuItem itemNeu = new JMenuItem("Abo anlegen");
            itemNeu.setIcon(GetIcon.getProgramIcon("add_16.png"));
            itemNeu.addActionListener(e -> aboNeu());
            jPopupMenu.add(itemNeu);

            //Menü anzeigen
            jPopupMenu.show(evt.getComponent(), evt.getX(), evt.getY());
        }
    }
}
