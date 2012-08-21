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
import mediathek.Daten;
import mediathek.MediathekGui;
import mediathek.controller.filme.filmeImportieren.MediathekListener;
import mediathek.daten.DDaten;
import mediathek.daten.DatenAbo;
import mediathek.gui.beobachter.CellRendererAbo;
import mediathek.gui.dialog.DialogEditAbo;
import mediathek.tool.Datum;
import mediathek.tool.HinweisKeineAuswahl;
import mediathek.tool.JTableMed;
import mediathek.tool.TModelAbo;

public class GuiAbo extends PanelVorlage {

    private JTableMed tabelle;

    public GuiAbo(DDaten d) {
        super(d);
        initComponents();
        tabelle = new JTableMed(JTableMed.TABELLE_TAB_ABOS);
        jScrollPane1.setViewportView(tabelle);
        initBeobachter();
        load();
        tabelle.initTabelle();
    }
    //===================================
    //public
    //===================================

    @Override
    public void isShown() {
        super.isShown();
        ddaten.mediathekGui.setToolbar(MediathekGui.ButtonAbo);
        ddaten.infoPanel.setIdx(InfoPanel.IDX_GUI_ABO);
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
        Daten.addAdListener(new MediathekListener(MediathekListener.EREIGNIS_LISTE_ABOS, GuiAbo.class.getSimpleName()) {
            @Override
            public void ping() {
                load();
            }
        });
        tabelle.addMouseListener(new BeobMausTabelle1());
        tabelle.setDefaultRenderer(Object.class, new CellRendererAbo(ddaten));
        tabelle.setDefaultRenderer(Datum.class, new CellRendererAbo(ddaten));
        tabelle.setModel(new TModelAbo(new Object[][]{}, DatenAbo.ABO_COLUMN_NAMES));
        //aendern
        ActionMap am = tabelle.getActionMap();
        am.put("aendern", new BeobAbstractAction());
        InputMap im = tabelle.getInputMap();
        KeyStroke enter = KeyStroke.getKeyStroke(KeyEvent.VK_ENTER, 0);
        im.put(enter, "aendern");
    }

    private void load() {
        getSpalten(tabelle);
        ddaten.listeAbo.addObjectData((TModelAbo) tabelle.getModel());
        setSpalten(tabelle);
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
            int ret = JOptionPane.showConfirmDialog(null, text, "Löschen?", JOptionPane.YES_NO_OPTION);
            if (ret == JOptionPane.OK_OPTION) {
                for (int i = rows.length - 1; i >= 0; --i) {
                    int delRow = tabelle.convertRowIndexToModel(rows[i]);
                    ((TModelAbo) tabelle.getModel()).removeRow(delRow);
                    ddaten.listeAbo.remove(delRow);
                }
            }
            DDaten.setGeaendert();
            load();
            Daten.notifyMediathekListener(MediathekListener.EREIGNIS_LISTE_ABOS, GuiAbo.class.getSimpleName());
        } else {
            new HinweisKeineAuswahl().zeigen();
        }
    }

    private void aboAendern() {
        int row = tabelle.getSelectedRow();
        if (row >= 0) {
            int delRow = tabelle.convertRowIndexToModel(row);
            DatenAbo akt = ddaten.listeAbo.getAboNr(delRow);
            DatenAbo ret = akt.getCopy();
            DialogEditAbo dialog = new DialogEditAbo(null, true, ddaten, ret);
            dialog.setVisible(true);
            if (dialog.ok) {
                akt.aufMichKopieren(ret);
                DDaten.setGeaendert();
                load();
            }
            setInfo();
        } else {
            new HinweisKeineAuswahl().zeigen();
        }
    }

    private void aboEinAus(boolean ein) {
        int[] rows = tabelle.getSelectedRows();
        if (rows.length > 0) {
            for (int i = 0; i < rows.length; ++i) {
                int modelRow = tabelle.convertRowIndexToModel(rows[i]);
                DatenAbo akt = ddaten.listeAbo.getAboNr(modelRow);
                akt.arr[DatenAbo.ABO_EINGESCHALTET_NR] = String.valueOf(ein);
            }
            DDaten.setGeaendert();
            load();
            tabelle.clearSelection();
            for (int i = 0; i < rows.length; ++i) {
                tabelle.addRowSelectionInterval(rows[i], rows[i]);
            }
            setInfo();
            Daten.notifyMediathekListener(MediathekListener.EREIGNIS_LISTE_ABOS, GuiAbo.class.getSimpleName());
        } else {
            new HinweisKeineAuswahl().zeigen();
        }
    }

    private void setInfo() {
        String textLinks;
        int ein = 0;
        int aus = 0;
        int gesamt = tabelle.getModel().getRowCount();
        for (int i = 0; i < tabelle.getModel().getRowCount(); ++i) {
            int modelRow = tabelle.convertRowIndexToModel(i);
            DatenAbo akt = ddaten.listeAbo.getAboNr(modelRow);
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
        ddaten.infoPanel.setTextLinks(InfoPanel.IDX_GUI_ABO, textLinks);
    }

    /** This method is called from within the constructor to
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
        public void mousePressed(MouseEvent arg0) {
            if (arg0.getButton() == MouseEvent.BUTTON1) {
                if (arg0.getClickCount() > 1) {
                    aboAendern();
                }
            } else if (arg0.getButton() == MouseEvent.BUTTON3) {
                showMenu(arg0);
            }
        }

        private void showMenu(MouseEvent evt) {
            boolean ein = true;
            p = evt.getPoint();
            int nr = tabelle.rowAtPoint(p);
            if (nr >= 0) {
                tabelle.setRowSelectionInterval(nr, nr);
                int modelRow = tabelle.convertRowIndexToModel(nr);
                DatenAbo akt = ddaten.listeAbo.getAboNr(modelRow);
                ein = Boolean.parseBoolean(akt.arr[DatenAbo.ABO_EINGESCHALTET_NR]);
            }
            JPopupMenu jPopupMenu = new JPopupMenu();
            // Abo einschalten
            JMenuItem itemEinschalten = new JMenuItem("Abo einschalten");
            itemEinschalten.setIcon(new javax.swing.ImageIcon(getClass().getResource("/mediathek/res/ja_16.png")));
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
            itemDeaktivieren.setIcon(new javax.swing.ImageIcon(getClass().getResource("/mediathek/res/nein_16.png")));
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
            itemLoeschen.setIcon(new javax.swing.ImageIcon(getClass().getResource("/mediathek/res/del_16.png")));
            itemLoeschen.addActionListener(new ActionListener() {
                @Override
                public void actionPerformed(ActionEvent e) {
                    aboLoeschen();
                }
            });
            jPopupMenu.add(itemLoeschen);
            //Abo ändern
            JMenuItem itemAendern = new JMenuItem("Abo ändern");
            itemAendern.setIcon(new javax.swing.ImageIcon(getClass().getResource("/mediathek/res/configure_16.png")));
            itemAendern.addActionListener(new ActionListener() {
                @Override
                public void actionPerformed(ActionEvent e) {
                    aboAendern();
                }
            });
            jPopupMenu.add(itemAendern);

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

            //Menü anzeigen
            jPopupMenu.show(evt.getComponent(), evt.getX(), evt.getY());
        }
    }
}
