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
import mediathek.MediathekGui;
import mediathek.config.Daten;
import mediathek.config.Icons;
import mediathek.config.MVConfig;
import mediathek.daten.DatenAbo;
import mediathek.gui.dialog.DialogEditAbo;
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

        toolBar = new ToolBar(daten, MediathekGui.TABS.TAB_ABOS);
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
//            Daten.mediathekGui.setTabShown(MediathekGui.TABS.TAB_ABOS);
            Daten.mediathekGui.getStatusBar().setIndexForLeftDisplay(MVStatusBar.StatusbarIndex.ABO);
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

    public void invertSelection() {
        tabelle.invertSelection();
    }

    //===================================
    //private
    //===================================
    private void setToolbarVisible() {
        toolBar.setVisible(Boolean.parseBoolean(MVConfig.get(MVConfig.Configs.SYSTEM_TOOLBAR_ALLES_ANZEIGEN)));
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
        tabelle.lineBreak = MVConfig.getBool(MVConfig.Configs.SYSTEM_TAB_ABO_LINEBREAK);
        tabelle.getTableHeader().addMouseListener(new BeobTableHeader(tabelle, DatenAbo.COLUMN_NAMES, DatenAbo.spaltenAnzeigen,
                new int[]{DatenAbo.ABO_EINGESCHALTET},
                new int[]{},
                true /*Icon*/, MVConfig.Configs.SYSTEM_TAB_ABO_LINEBREAK));
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
        //löschen
        im.put(KeyStroke.getKeyStroke(KeyEvent.VK_DELETE, 0), "loeschen");
        am.put("loeschen", new AbstractAction() {

            @Override
            public void actionPerformed(ActionEvent e) {
                aboLoeschen();
            }
        });

        //Filter
        final String[] sender = GuiFunktionen.addLeerListe(Daten.filmeLaden.getSenderNamen());
        jcbSender.setModel(new javax.swing.DefaultComboBoxModel<>(sender));
        jcbSender.addActionListener(l -> tabelleLaden());

        jSplitPane1.setDividerLocation(MVConfig.getInt(MVConfig.Configs.SYSTEM_PANEL_ABO_DIVIDER));
        jSplitPane1.addPropertyChangeListener(JSplitPane.DIVIDER_LOCATION_PROPERTY, pce -> {
            if (jScrollPaneFilter.isVisible()) {
                MVConfig.add(MVConfig.Configs.SYSTEM_PANEL_ABO_DIVIDER, String.valueOf(jSplitPane1.getDividerLocation()));
            }
        });
        jScrollPaneFilter.setVisible(MVConfig.getBool(MVConfig.Configs.SYSTEM_TAB_ABO_FILTER_VIS));
        Listener.addListener(new Listener(Listener.EREIGNIS_PANEL_ABO_FILTER_ANZEIGEN, GuiAbo.class.getSimpleName()) {
            @Override
            public void ping() {
                setFilter();
            }
        });
        btnDest.addActionListener(l -> abosAendern(DatenAbo.ABO_ZIELPFAD));
        btnSet.addActionListener(l -> abosAendern(DatenAbo.ABO_PSET));
        btnMinMax.addActionListener(l -> abosAendern(DatenAbo.ABO_MIN));
        btnTime.addActionListener(l -> abosAendern(DatenAbo.ABO_MINDESTDAUER));
    }

    private void setFilter() {
        // Panel anzeigen und die Filmliste anpassen
        jScrollPaneFilter.setVisible(MVConfig.getBool(MVConfig.Configs.SYSTEM_TAB_ABO_FILTER_VIS));
        if (jScrollPaneFilter.isVisible()) {
            jSplitPane1.setDividerLocation(MVConfig.getInt(MVConfig.Configs.SYSTEM_PANEL_ABO_DIVIDER));
        }
        updateUI();
    }

    private void tabelleLaden() {
        tabelle.getSpalten();
        Daten.listeAbo.addObjectData((TModelAbo) tabelle.getModel(), jcbSender.getSelectedItem().toString());
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
            DialogEditAbo dialog = new DialogEditAbo(Daten.mediathekGui, true, daten, akt, -1 /*onlyOne*/);
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

    private void abosAendern(int change) {
        int row = tabelle.getSelectedRow();
        int[] rows = tabelle.getSelectedRows();

        if (row < 0) {
            new HinweisKeineAuswahl().zeigen(parentComponent);
            return;
        }

        int modelRow = tabelle.convertRowIndexToModel(row);
        DatenAbo akt = Daten.listeAbo.getAboNr(modelRow);
        DialogEditAbo dialog = new DialogEditAbo(Daten.mediathekGui, true, daten, akt, change);
        dialog.setVisible(true);
        if (!dialog.ok) {
            return;
        }

        for (int i : rows) {
            if (i == row) {
                continue;
            }
            modelRow = tabelle.convertRowIndexToModel(i);
            DatenAbo sel = Daten.listeAbo.getAboNr(modelRow);
            sel.arr[change] = akt.arr[change];
            if (change == DatenAbo.ABO_MINDESTDAUER) {
                sel.setMindestDauerMinuten();
            }
            if (change == DatenAbo.ABO_MIN) {
                sel.min = Boolean.parseBoolean(sel.arr[DatenAbo.ABO_MIN]);
            }
        }

        tabelleLaden();
        Daten.listeAbo.aenderungMelden();
        setInfo();
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
        Daten.mediathekGui.getStatusBar().setTextForLeftDisplay();
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
        jSplitPane1 = new javax.swing.JSplitPane();
        jScrollPane1 = new javax.swing.JScrollPane();
        javax.swing.JTable jTable1 = new javax.swing.JTable();
        jScrollPaneFilter = new javax.swing.JScrollPane();
        jPanelFilter = new javax.swing.JPanel();
        jLabel1 = new javax.swing.JLabel();
        jcbSender = new javax.swing.JComboBox<>();
        btnDest = new javax.swing.JButton();
        jLabel2 = new javax.swing.JLabel();
        btnSet = new javax.swing.JButton();
        btnMinMax = new javax.swing.JButton();
        btnTime = new javax.swing.JButton();

        javax.swing.GroupLayout jPanelToolBarLayout = new javax.swing.GroupLayout(jPanelToolBar);
        jPanelToolBar.setLayout(jPanelToolBarLayout);
        jPanelToolBarLayout.setHorizontalGroup(
            jPanelToolBarLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGap(0, 758, Short.MAX_VALUE)
        );
        jPanelToolBarLayout.setVerticalGroup(
            jPanelToolBarLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGap(0, 13, Short.MAX_VALUE)
        );

        jSplitPane1.setDividerLocation(200);

        jTable1.setAutoCreateRowSorter(true);
        jTable1.setAutoResizeMode(javax.swing.JTable.AUTO_RESIZE_OFF);
        jScrollPane1.setViewportView(jTable1);

        jSplitPane1.setRightComponent(jScrollPane1);

        jLabel1.setText("Abos für Sender:");

        jcbSender.setMaximumRowCount(25);
        jcbSender.setModel(new javax.swing.DefaultComboBoxModel<>(new String[] { "Item 1", "Item 2", "Item 3", "Item 4" }));

        btnDest.setText("Zielpfad");

        jLabel2.setText("markierte Abos ändern");

        btnSet.setText("Set");

        btnMinMax.setText("Min/Max");

        btnTime.setText("Dauer");

        javax.swing.GroupLayout jPanelFilterLayout = new javax.swing.GroupLayout(jPanelFilter);
        jPanelFilter.setLayout(jPanelFilterLayout);
        jPanelFilterLayout.setHorizontalGroup(
            jPanelFilterLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanelFilterLayout.createSequentialGroup()
                .addContainerGap()
                .addGroup(jPanelFilterLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addComponent(jcbSender, 0, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                    .addComponent(btnDest, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                    .addComponent(btnSet, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                    .addComponent(btnMinMax, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                    .addGroup(jPanelFilterLayout.createSequentialGroup()
                        .addGroup(jPanelFilterLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                            .addComponent(jLabel1)
                            .addComponent(jLabel2))
                        .addGap(0, 0, Short.MAX_VALUE))
                    .addComponent(btnTime, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
                .addContainerGap())
        );
        jPanelFilterLayout.setVerticalGroup(
            jPanelFilterLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanelFilterLayout.createSequentialGroup()
                .addContainerGap()
                .addComponent(jLabel1)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(jcbSender, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED, 212, Short.MAX_VALUE)
                .addComponent(jLabel2)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(btnDest)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(btnSet)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(btnMinMax)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(btnTime)
                .addContainerGap())
        );

        jScrollPaneFilter.setViewportView(jPanelFilter);

        jSplitPane1.setLeftComponent(jScrollPaneFilter);

        javax.swing.GroupLayout layout = new javax.swing.GroupLayout(this);
        this.setLayout(layout);
        layout.setHorizontalGroup(
            layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addComponent(jPanelToolBar, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
            .addGroup(layout.createSequentialGroup()
                .addComponent(jSplitPane1)
                .addContainerGap())
        );
        layout.setVerticalGroup(
            layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(layout.createSequentialGroup()
                .addComponent(jPanelToolBar, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addGap(18, 18, 18)
                .addComponent(jSplitPane1)
                .addContainerGap())
        );
    }// </editor-fold>//GEN-END:initComponents
    // Variables declaration - do not modify//GEN-BEGIN:variables
    private javax.swing.JButton btnDest;
    private javax.swing.JButton btnMinMax;
    private javax.swing.JButton btnSet;
    private javax.swing.JButton btnTime;
    private javax.swing.JLabel jLabel1;
    private javax.swing.JLabel jLabel2;
    private javax.swing.JPanel jPanelFilter;
    private javax.swing.JPanel jPanelToolBar;
    private javax.swing.JScrollPane jScrollPane1;
    private javax.swing.JScrollPane jScrollPaneFilter;
    private javax.swing.JSplitPane jSplitPane1;
    private javax.swing.JComboBox<String> jcbSender;
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
            itemEinschalten.setIcon(Icons.ICON_MENUE_EIN);
            itemEinschalten.setEnabled(!ein);
            itemEinschalten.addActionListener(e -> aboEinAus(true));
            jPopupMenu.add(itemEinschalten);
            // Abo deaktivieren
            JMenuItem itemDeaktivieren = new JMenuItem("Abo ausschalten");
            itemDeaktivieren.setIcon(Icons.ICON_MENUE_AUS);
            itemDeaktivieren.setEnabled(ein);
            itemDeaktivieren.addActionListener(e -> aboEinAus(false));
            jPopupMenu.add(itemDeaktivieren);
            //Abo lösschen
            JMenuItem itemLoeschen = new JMenuItem("Abo löschen");
            itemLoeschen.setIcon(Icons.ICON_MENUE_ABO_LOESCHEN);
            itemLoeschen.addActionListener(e -> aboLoeschen());
            jPopupMenu.add(itemLoeschen);
            //Abo ändern
            JMenuItem itemAendern = new JMenuItem("Abo ändern");
            itemAendern.setIcon(Icons.ICON_MENUE_ABO_AENDERN);
            itemAendern.addActionListener(e -> aboAendern());
            jPopupMenu.add(itemAendern);

            //##Trenner##
            jPopupMenu.addSeparator();
            //Abo ändern
            JMenuItem itemNeu = new JMenuItem("Abo anlegen");
            itemNeu.setIcon(Icons.ICON_MENUE_ABO_NEU);
            itemNeu.addActionListener(e -> aboNeu());
            jPopupMenu.add(itemNeu);

            //Menü anzeigen
            jPopupMenu.show(evt.getComponent(), evt.getX(), evt.getY());
        }
    }
}
