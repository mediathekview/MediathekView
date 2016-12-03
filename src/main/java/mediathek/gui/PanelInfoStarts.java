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
package mediathek.gui;

import mSearch.daten.DatenFilm;
import mediathek.config.Daten;
import mediathek.daten.DatenDownload;
import mediathek.tool.TModel;

import javax.swing.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

@SuppressWarnings("serial")
public class PanelInfoStarts extends JPanel {
    private TModel tModel;

    public PanelInfoStarts() {
        super();
        initComponents();
        jButtonAuffrischen.addActionListener(new BeobLaden());
        tModel = getEmptyModel();
        jTable1.setModel(tModel);
        jTable1.setAutoResizeMode(javax.swing.JTable.AUTO_RESIZE_OFF);
        init();
    }

    public static TModel getEmptyModel() {
        int max = DatenDownload.MAX_ELEM + 1;
        String[] titel = new String[max];
        for (int i = 0; i < max; ++i) {
            if (i < DatenDownload.MAX_ELEM) {
                titel[i] = DatenDownload.COLUMN_NAMES[i];
            } else {
                titel[i] = "Art";
            }
        }
        return new TModel(new Object[][]{}, titel);
    }

    private synchronized void init() {
        tModel = Daten.getInstance().getListeDownloads().getModelStarts(tModel);
        for (int i = 0; i < jTable1.getColumnCount(); ++i) {
            if (i > DatenFilm.FILM_URL) {
                jTable1.getColumnModel().getColumn(i).setMinWidth(0);
                jTable1.getColumnModel().getColumn(i).setMaxWidth(0);
                jTable1.getColumnModel().getColumn(i).setPreferredWidth(0);
            }
        }
        this.updateUI();
    }

    // <editor-fold defaultstate="collapsed" desc="Generated Code">//GEN-BEGIN:initComponents
    private void initComponents() {

        jButtonAuffrischen = new javax.swing.JButton();
        javax.swing.JScrollPane jScrollPane1 = new javax.swing.JScrollPane();
        jTable1 = new javax.swing.JTable();

        setBorder(javax.swing.BorderFactory.createEtchedBorder());

        jButtonAuffrischen.setText("Auffrischen");

        jTable1.setAutoCreateRowSorter(true);
        jTable1.setModel(new TModel());
        jScrollPane1.setViewportView(jTable1);

        javax.swing.GroupLayout layout = new javax.swing.GroupLayout(this);
        this.setLayout(layout);
        layout.setHorizontalGroup(
                layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                        .addGroup(layout.createSequentialGroup()
                                .addContainerGap()
                                .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                                        .addComponent(jScrollPane1, javax.swing.GroupLayout.DEFAULT_SIZE, 461, Short.MAX_VALUE)
                                        .addGroup(javax.swing.GroupLayout.Alignment.TRAILING, layout.createSequentialGroup()
                                                .addGap(0, 0, Short.MAX_VALUE)
                                                .addComponent(jButtonAuffrischen)))
                                .addContainerGap())
        );
        layout.setVerticalGroup(
                layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                        .addGroup(layout.createSequentialGroup()
                                .addContainerGap()
                                .addComponent(jScrollPane1, javax.swing.GroupLayout.DEFAULT_SIZE, 276, Short.MAX_VALUE)
                                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                                .addComponent(jButtonAuffrischen)
                                .addContainerGap())
        );
    }// </editor-fold>//GEN-END:initComponents

    // Variables declaration - do not modify//GEN-BEGIN:variables
    private javax.swing.JButton jButtonAuffrischen;
    private javax.swing.JTable jTable1;
    // End of variables declaration//GEN-END:variables

    private class BeobLaden implements ActionListener {

        @Override
        public void actionPerformed(ActionEvent e) {
            init();
        }
    }
}
