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
import mediathek.Log;
import mediathek.controller.filme.filmeImportieren.MediathekListener;
import mediathek.daten.DDaten;
import mediathek.gui.PanelVorlage;

public class PanelMeldungen extends PanelVorlage {

    private StringBuffer text = new StringBuffer();
    private final int MAX_LAENGE_1 = 30000;
    private final int MAX_LAENGE_2 = 20000;
    private String logArt;

    /**
     * Creates new form GuiFeed
     *
     * @param d
     */
    public PanelMeldungen(DDaten d, String llogArt) {
        super(d);
        initComponents();
        logArt = llogArt;
        //init
        Log.addAdListener(new MediathekListener() {

            @Override
            public void ping(String fromm, String meldung) {
                if (logArt.equals(fromm)) {
                    addText(meldung);
                }
            }
        });
        jButtonLoeschen.addActionListener(new BeobLoeschen());
    }

    public void addText(String texte) {
        cut(text);
        text.append(texte);
        text.append("\n");
        jTextArea.setText(text.toString());
        setTextArea();
    }

    private void setTextArea() {
        //pane.setViewportView(area);
        jScrollPane.getVerticalScrollBar().setValue(jScrollPane.getVerticalScrollBar().getMaximum());
        jScrollPane.repaint();
    }

    private void clear() {
        text.setLength(0);
        jTextArea.setText(text.toString());
    }

    private void cut(StringBuffer buffer) {
        if (buffer.length() > MAX_LAENGE_1) {
            buffer.delete(0, MAX_LAENGE_2);
        }
    }
    // <editor-fold defaultstate="collapsed" desc="Generated Code">//GEN-BEGIN:initComponents
    private void initComponents() {

        jButtonLoeschen = new javax.swing.JButton();
        jScrollPane = new javax.swing.JScrollPane();
        jTextArea = new javax.swing.JTextArea();

        jButtonLoeschen.setText("löschen");

        jTextArea.setColumns(20);
        jTextArea.setRows(5);
        jScrollPane.setViewportView(jTextArea);

        javax.swing.GroupLayout layout = new javax.swing.GroupLayout(this);
        this.setLayout(layout);
        layout.setHorizontalGroup(
            layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(layout.createSequentialGroup()
                .addContainerGap()
                .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addGroup(layout.createSequentialGroup()
                        .addGap(0, 246, Short.MAX_VALUE)
                        .addComponent(jButtonLoeschen))
                    .addComponent(jScrollPane))
                .addContainerGap())
        );
        layout.setVerticalGroup(
            layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(javax.swing.GroupLayout.Alignment.TRAILING, layout.createSequentialGroup()
                .addContainerGap()
                .addComponent(jScrollPane, javax.swing.GroupLayout.DEFAULT_SIZE, 196, Short.MAX_VALUE)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                .addComponent(jButtonLoeschen)
                .addGap(12, 12, 12))
        );
    }// </editor-fold>//GEN-END:initComponents
    // Variables declaration - do not modify//GEN-BEGIN:variables
    private javax.swing.JButton jButtonLoeschen;
    private javax.swing.JScrollPane jScrollPane;
    private javax.swing.JTextArea jTextArea;
    // End of variables declaration//GEN-END:variables

    private class BeobLoeschen implements ActionListener {

        @Override
        public void actionPerformed(ActionEvent arg0) {
            clear();
        }
    }
}
