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
package mediathek.gui.dialogInfos;

import java.awt.Point;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import javax.swing.JMenuItem;
import javax.swing.JPopupMenu;
import mediathek.Log;
import mediathek.controller.io.starter.ListeStarts;
import mediathek.controller.io.starter.StartEvent;
import mediathek.controller.io.starter.StartListener;
import mediathek.controller.io.starter.Starts;
import mediathek.daten.DDaten;
import mediathek.daten.DatenFilm;
import mediathek.gui.PanelVorlage;
import mediathek.gui.beobachter.CellRendererFilme;
import mediathek.tool.TModel;

public class PanelInfoStarts extends PanelVorlage {

    TModel tModel;

    /**
     * Creates new form GuiFeed
     *
     * @param d
     */
    public PanelInfoStarts(DDaten d) {
        super(d);
        initComponents();
        ddaten = d;
        jButtonAuffrischen.addActionListener(new BeobLaden());
        jButtonStoppen.addActionListener(new BeobStoppen());
        ddaten.starterClass.addListener(new BeobStart());
        tModel = ListeStarts.getNewModel();
        jTable1.setModel(tModel);
        jTable1.setDefaultRenderer(Object.class, new CellRendererFilme(ddaten));
        jTable1.addMouseListener(new BeobMausTabelle());
        jTable1.setAutoResizeMode(javax.swing.JTable.AUTO_RESIZE_OFF);
        init();
    }
    //===================================
    // Public
    //===================================

    @Override
    public void neuLaden() {
    }
    //===================================
    // Private
    //===================================

    private synchronized void init() {
        tModel = ddaten.starterClass.getStarterModell(tModel);
//        if (tModel != null) {
//            jTable1.setModel(tModel);
//        } else {
//            int max = Konstanten.FILME_MAX_ELEM + 1;
//            String[] titel = new String[max];
//            String[] titel2 = new String[max];
//            for (int i = 0; i < max; ++i) {
//                if (i < Konstanten.FILME_MAX_ELEM) {
//                    titel[i] = Konstanten.FILME_COLUMN_NAMES[i];
//                    titel2[i] = "";
//                } else {
//                    titel[i] = "Art";
//                    titel2[i] = "";
//                }
//            }
//            tModel = new TModel(new Object[][]{titel2}, titel);
//            jTable1.setModel(tModel);
//        }
        for (int i = 0; i < jTable1.getColumnCount(); ++i) {
            if (i > DatenFilm.FILM_URL_NR) {
                jTable1.getColumnModel().getColumn(i).setMinWidth(0);
                jTable1.getColumnModel().getColumn(i).setMaxWidth(0);
                jTable1.getColumnModel().getColumn(i).setPreferredWidth(0);
            }
        }
        this.updateUI();
    }

    private void stoppen(String url) {
        if (!url.equals("")) {
            ddaten.starterClass.delStart(url);
        }
    }

    // <editor-fold defaultstate="collapsed" desc="Generated Code">//GEN-BEGIN:initComponents
    private void initComponents() {

        jButtonAuffrischen = new javax.swing.JButton();
        jScrollPane1 = new javax.swing.JScrollPane();
        jTable1 = new javax.swing.JTable();
        jButtonStoppen = new javax.swing.JButton();

        setBorder(javax.swing.BorderFactory.createEtchedBorder());

        jButtonAuffrischen.setText("Auffrischen");

        jTable1.setAutoCreateRowSorter(true);
        jTable1.setModel(new javax.swing.table.DefaultTableModel(
            new Object [][] {
                {null, null, null, null},
                {null, null, null, null},
                {null, null, null, null},
                {null, null, null, null}
            },
            new String [] {
                "Title 1", "Title 2", "Title 3", "Title 4"
            }
        ));
        jScrollPane1.setViewportView(jTable1);

        jButtonStoppen.setText("Stoppen");

        javax.swing.GroupLayout layout = new javax.swing.GroupLayout(this);
        this.setLayout(layout);
        layout.setHorizontalGroup(
            layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(layout.createSequentialGroup()
                .addContainerGap()
                .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addComponent(jScrollPane1, javax.swing.GroupLayout.DEFAULT_SIZE, 392, Short.MAX_VALUE)
                    .addGroup(javax.swing.GroupLayout.Alignment.TRAILING, layout.createSequentialGroup()
                        .addComponent(jButtonStoppen)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                        .addComponent(jButtonAuffrischen)))
                .addContainerGap())
        );

        layout.linkSize(javax.swing.SwingConstants.HORIZONTAL, new java.awt.Component[] {jButtonAuffrischen, jButtonStoppen});

        layout.setVerticalGroup(
            layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(layout.createSequentialGroup()
                .addContainerGap()
                .addComponent(jScrollPane1, javax.swing.GroupLayout.DEFAULT_SIZE, 203, Short.MAX_VALUE)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(jButtonAuffrischen)
                    .addComponent(jButtonStoppen))
                .addContainerGap())
        );
    }// </editor-fold>//GEN-END:initComponents
    // Variables declaration - do not modify//GEN-BEGIN:variables
    private javax.swing.JButton jButtonAuffrischen;
    private javax.swing.JButton jButtonStoppen;
    private javax.swing.JScrollPane jScrollPane1;
    private javax.swing.JTable jTable1;
    // End of variables declaration//GEN-END:variables

    private class BeobLaden implements ActionListener {

        @Override
        public void actionPerformed(ActionEvent e) {
            init();
        }
    }

    private class BeobStoppen implements ActionListener {

        @Override
        public void actionPerformed(ActionEvent e) {
            int row = jTable1.getSelectedRow();
            if (row >= 0) {
                int delRow = jTable1.convertRowIndexToModel(row);
                Starts s = ddaten.starterClass.getStart(jTable1.getValueAt(delRow, DatenFilm.FILM_URL_NR).toString());
                if (s != null) {
                    if (s.status == Starts.STATUS_RUN) {
                        stoppen(jTable1.getValueAt(delRow, DatenFilm.FILM_URL_NR).toString());
                    }
                }
            }
        }
    }

    private class BeobStart implements StartListener {

        @Override
        public void starter(StartEvent ev) {
            try {
                init();
            } catch (Exception ex) {
                Log.fehlerMeldung("PanelInfoStarts.BeobStart", ex);
            }
        }
    }

    public class BeobMausTabelle extends MouseAdapter {

        private BeobUrl beobUrl = new BeobUrl();
        private Point p;
        String url = null;

        public BeobMausTabelle() {
        }

        private class BeobUrl implements ActionListener {

            @Override
            public void actionPerformed(ActionEvent e) {
                stoppen(url);
            }
        }

        @Override
        public void mouseClicked(MouseEvent arg0) {
            if (arg0.getButton() == MouseEvent.BUTTON3) {
                showMenu(arg0);
            }
        }

        private void showMenu(MouseEvent evt) {
            url = "";
            p = evt.getPoint();
            int nr = jTable1.rowAtPoint(p);
            if (nr >= 0) {
                jTable1.setRowSelectionInterval(nr, nr);
            }
            JPopupMenu menu = new JPopupMenu();
            int row = jTable1.getSelectedRow();
            if (row >= 0) {
                int delRow = jTable1.convertRowIndexToModel(row);
                Starts s = ddaten.starterClass.getStart(jTable1.getValueAt(delRow, DatenFilm.FILM_URL_NR).toString());
                if (s != null) {
                    if (s.status == Starts.STATUS_RUN) {
                        //url
                        url = jTable1.getValueAt(delRow, DatenFilm.FILM_URL_NR).toString();
                        JMenuItem itemAbbrechen = new JMenuItem("Film abbrechen");
                        itemAbbrechen.addActionListener(beobUrl);
                        menu.add(itemAbbrechen);
                        //Menü anzeigen
                        menu.show(evt.getComponent(), evt.getX(), evt.getY());
                    }
                }
            }
        }
    }
}
