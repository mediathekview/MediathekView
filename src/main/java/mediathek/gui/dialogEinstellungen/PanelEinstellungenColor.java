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
package mediathek.gui.dialogEinstellungen;

import mediathek.config.Daten;
import mediathek.config.MVColor;
import mediathek.gui.PanelVorlage;
import mediathek.tool.*;

import javax.swing.*;
import java.awt.*;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;

import static mediathek.config.MVColor.*;

@SuppressWarnings("serial")
public class PanelEinstellungenColor extends PanelVorlage {
    public PanelEinstellungenColor(Daten d, JFrame pparentComponent) {
        super(d, pparentComponent);
        initComponents();
        daten = d;
        init();
    }

    private void init() {
        jTable1.addMouseListener(new BeobMausTabelle());
        jTable1.setDefaultRenderer(MVC.class, new CellRendererColor());
        jTable1.setModel(getModel());
        jButtonReset.addActionListener(e -> {
            Daten.mVColor.reset();
            GuiFunktionen.updateGui(daten.getMediathekGui());
            Daten.mVColor.save();
        });
    }

    private void getColor(MVC mvc) {
        DialogFarbe dialog = new DialogFarbe(parentComponent, true, mvc.color);
        dialog.setVisible(true);
        if (dialog.farbe != null) {
            if (!dialog.farbe.equals(mvc.color)) {
                mvc.set(dialog.farbe);
                jTable1.setModel(getModel());
                GuiFunktionen.updateGui(daten.getMediathekGui());
                Daten.mVColor.save();
            }
        }
    }

    private TModel getModel() {
        Object[] object;
        TModelColor tModel = new TModelColor(new Object[][]{}, new String[]{"Beschreibung", "Farbe"});
        tModel.setRowCount(0);
        for (MVC mvc : Daten.mVColor.liste) {
            object = new Object[MVC_MAX];
            object[MVC_TEXT] = mvc.text;
            object[MVC_COLOR] = mvc;
            tModel.addRow(object);
        }
        return tModel;
    }

    // <editor-fold defaultstate="collapsed" desc="Generated Code">//GEN-BEGIN:initComponents
    private void initComponents() {

        javax.swing.ButtonGroup buttonGroup1 = new javax.swing.ButtonGroup();
        javax.swing.JScrollPane jScrollPane1 = new javax.swing.JScrollPane();
        jTable1 = new javax.swing.JTable();
        jButtonReset = new javax.swing.JButton();

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

        jButtonReset.setText("Farben zurücksetzen");

        javax.swing.GroupLayout layout = new javax.swing.GroupLayout(this);
        this.setLayout(layout);
        layout.setHorizontalGroup(
                layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                        .addGroup(layout.createSequentialGroup()
                                .addContainerGap()
                                .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                                        .addComponent(jScrollPane1, javax.swing.GroupLayout.DEFAULT_SIZE, 413, Short.MAX_VALUE)
                                        .addGroup(javax.swing.GroupLayout.Alignment.TRAILING, layout.createSequentialGroup()
                                                .addGap(0, 0, Short.MAX_VALUE)
                                                .addComponent(jButtonReset)))
                                .addContainerGap())
        );
        layout.setVerticalGroup(
                layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                        .addGroup(layout.createSequentialGroup()
                                .addContainerGap()
                                .addComponent(jScrollPane1, javax.swing.GroupLayout.DEFAULT_SIZE, 221, Short.MAX_VALUE)
                                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                                .addComponent(jButtonReset)
                                .addContainerGap())
        );
    }// </editor-fold>//GEN-END:initComponents

    // Variables declaration - do not modify//GEN-BEGIN:variables
    private javax.swing.JButton jButtonReset;
    private javax.swing.JTable jTable1;
    // End of variables declaration//GEN-END:variables

    public class BeobMausTabelle extends MouseAdapter {

        private Point p;

        @Override
        public void mouseClicked(MouseEvent arg0) {
            if (arg0.getButton() == MouseEvent.BUTTON1) {
                if (arg0.getClickCount() == 1) {
                    p = arg0.getPoint();
                    int row = jTable1.rowAtPoint(p);
                    int column = jTable1.columnAtPoint(p);
                    if (row >= 0) {
                        MVC mvc = (MVC) jTable1.getModel().getValueAt(jTable1.convertRowIndexToModel(row), MVColor.MVC_COLOR);
                        if (jTable1.convertColumnIndexToModel(column) == MVColor.MVC_COLOR) {
                            getColor(mvc);
                        }
                    }
                }
            }
        }

    }
}
