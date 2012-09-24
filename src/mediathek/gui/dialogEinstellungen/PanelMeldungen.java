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
package mediathek.gui.dialogEinstellungen;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import mediathek.daten.DDaten;
import mediathek.daten.Daten;
import mediathek.gui.PanelVorlage;
import mediathek.tool.ListenerMediathekView;
import mediathek.tool.Log;

public class PanelMeldungen extends PanelVorlage {

    private StringBuffer text;
    private int logArt;

    /**
     * Creates new form GuiFeed
     *
     * @param d
     */
    public PanelMeldungen(DDaten d, StringBuffer ttext, int llogArt, String header) {
        super(d);
        initComponents();
        text = ttext;
        jLabelHeader.setText(header);
        logArt = llogArt;
        setText();
        //init
//        Log.addAdListener(new ListenerMediathekView() {
//
//            @Override
//            public void ping(String fromm) {
//                if (logArt.equals(fromm)) {
//                    setText();
//                }
//            }
//        });
        ListenerMediathekView.addListener(new ListenerMediathekView(logArt, PanelMeldungen.class.getSimpleName()) {
            @Override
            public void ping() {
                setText();
            }
        });


        jButtonLoeschen.addActionListener(new BeobLoeschen());
    }

    private void setText() {
        jTextArea.setText(text.toString());
        if (jCheckBoxAuto.isSelected()) {
            jTextArea.setCaretPosition(jTextArea.getDocument().getLength());
        }
    }
    // <editor-fold defaultstate="collapsed" desc="Generated Code">//GEN-BEGIN:initComponents
    private void initComponents() {

        jButtonLoeschen = new javax.swing.JButton();
        jScrollPane = new javax.swing.JScrollPane();
        jTextArea = new javax.swing.JTextArea();
        jLabelHeader = new javax.swing.JLabel();
        jCheckBoxAuto = new javax.swing.JCheckBox();

        jButtonLoeschen.setText("löschen");

        jTextArea.setColumns(20);
        jTextArea.setRows(5);
        jScrollPane.setViewportView(jTextArea);

        jLabelHeader.setText("jLabel1");

        jCheckBoxAuto.setSelected(true);
        jCheckBoxAuto.setText("Autoscroll");

        javax.swing.GroupLayout layout = new javax.swing.GroupLayout(this);
        this.setLayout(layout);
        layout.setHorizontalGroup(
            layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(layout.createSequentialGroup()
                .addContainerGap()
                .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addComponent(jScrollPane, javax.swing.GroupLayout.Alignment.TRAILING, javax.swing.GroupLayout.DEFAULT_SIZE, 478, Short.MAX_VALUE)
                    .addGroup(layout.createSequentialGroup()
                        .addComponent(jCheckBoxAuto)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                        .addComponent(jButtonLoeschen))
                    .addGroup(layout.createSequentialGroup()
                        .addComponent(jLabelHeader)
                        .addGap(0, 0, Short.MAX_VALUE)))
                .addContainerGap())
        );
        layout.setVerticalGroup(
            layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(javax.swing.GroupLayout.Alignment.TRAILING, layout.createSequentialGroup()
                .addContainerGap()
                .addComponent(jLabelHeader)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(jScrollPane, javax.swing.GroupLayout.DEFAULT_SIZE, 368, Short.MAX_VALUE)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(jButtonLoeschen)
                    .addComponent(jCheckBoxAuto))
                .addContainerGap())
        );
    }// </editor-fold>//GEN-END:initComponents
    // Variables declaration - do not modify//GEN-BEGIN:variables
    private javax.swing.JButton jButtonLoeschen;
    private javax.swing.JCheckBox jCheckBoxAuto;
    private javax.swing.JLabel jLabelHeader;
    private javax.swing.JScrollPane jScrollPane;
    private javax.swing.JTextArea jTextArea;
    // End of variables declaration//GEN-END:variables

    private class BeobLoeschen implements ActionListener {

        @Override
        public void actionPerformed(ActionEvent arg0) {
            Log.clearText(logArt);
        }
    }
}
