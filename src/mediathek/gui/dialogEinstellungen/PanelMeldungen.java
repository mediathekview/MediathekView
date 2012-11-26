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
import javax.swing.text.DefaultCaret;
import mediathek.daten.DDaten;
import mediathek.gui.PanelVorlage;
import mediathek.tool.GuiFunktionen;
import mediathek.tool.ListenerMediathekView;
import mediathek.tool.Log;

public class PanelMeldungen extends PanelVorlage {

    private StringBuffer text;
    private int logArt;
    int zeilenNr = 1;

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
        jCheckBoxAuto.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                setAuto();
            }
        });
        jCheckBoxZeilen.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                setLineWrab();
            }
        });
        setAuto();
        setLineWrab();
    }

    private void setAuto() {
        if (jCheckBoxAuto.isSelected()) {
            DefaultCaret caret = (DefaultCaret) jTextArea.getCaret();
            caret.setUpdatePolicy(DefaultCaret.ALWAYS_UPDATE);
        } else {
            DefaultCaret caret = (DefaultCaret) jTextArea.getCaret();
            caret.setUpdatePolicy(DefaultCaret.NEVER_UPDATE);
        }
    }

    private void setLineWrab() {
        jTextArea.setLineWrap(jCheckBoxZeilen.isSelected());
        jTextArea.setWrapStyleWord(false);
        setText();
    }

    private void setText() {
        String s = text.toString();
//        if (jCheckBoxAuto.isSelected() && jTextArea.getLineWrap()) {
//            StringBuilder b = new StringBuilder(s.length() + 200);
//            for (int i = 0; i < s.length(); i++) {
//                String t = s.substring(i, i + 1);
//                if (t.equals("\n")) {
//                    b.append("\n").append(getNr(zeilenNr++)).append(" ");
//                } else {
//                    b.append(t);
//                }
//            }
//            s = b.toString();
//        }
        jTextArea.setText(s);
//        if (jCheckBoxAuto.isSelected()) {
//            jTextArea.setCaretPosition(jTextArea.getDocument().getLength());
//        }
    }


    // <editor-fold defaultstate="collapsed" desc="Generated Code">//GEN-BEGIN:initComponents
    private void initComponents() {

        jButtonLoeschen = new javax.swing.JButton();
        jScrollPane = new javax.swing.JScrollPane();
        jTextArea = new javax.swing.JTextArea();
        jLabelHeader = new javax.swing.JLabel();
        jCheckBoxAuto = new javax.swing.JCheckBox();
        jCheckBoxZeilen = new javax.swing.JCheckBox();
        jScrollPane1 = new javax.swing.JScrollPane();
        jTextPane1 = new javax.swing.JTextPane();

        jButtonLoeschen.setText("löschen");

        jTextArea.setEditable(false);
        jTextArea.setColumns(20);
        jTextArea.setRows(5);
        jScrollPane.setViewportView(jTextArea);

        jLabelHeader.setText("jLabel1");

        jCheckBoxAuto.setSelected(true);
        jCheckBoxAuto.setText("Autoscroll");

        jCheckBoxZeilen.setText("Zeilen umbrechen");

        jScrollPane1.setViewportView(jTextPane1);

        javax.swing.GroupLayout layout = new javax.swing.GroupLayout(this);
        this.setLayout(layout);
        layout.setHorizontalGroup(
            layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(javax.swing.GroupLayout.Alignment.TRAILING, layout.createSequentialGroup()
                .addContainerGap()
                .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.TRAILING)
                    .addComponent(jScrollPane1)
                    .addComponent(jScrollPane, javax.swing.GroupLayout.DEFAULT_SIZE, 478, Short.MAX_VALUE)
                    .addGroup(javax.swing.GroupLayout.Alignment.LEADING, layout.createSequentialGroup()
                        .addComponent(jCheckBoxAuto)
                        .addGap(18, 18, 18)
                        .addComponent(jCheckBoxZeilen)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                        .addComponent(jButtonLoeschen))
                    .addGroup(javax.swing.GroupLayout.Alignment.LEADING, layout.createSequentialGroup()
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
                .addComponent(jScrollPane, javax.swing.GroupLayout.PREFERRED_SIZE, 123, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(jScrollPane1, javax.swing.GroupLayout.DEFAULT_SIZE, 239, Short.MAX_VALUE)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(jButtonLoeschen)
                    .addComponent(jCheckBoxAuto)
                    .addComponent(jCheckBoxZeilen))
                .addContainerGap())
        );
    }// </editor-fold>//GEN-END:initComponents
    // Variables declaration - do not modify//GEN-BEGIN:variables
    private javax.swing.JButton jButtonLoeschen;
    private javax.swing.JCheckBox jCheckBoxAuto;
    private javax.swing.JCheckBox jCheckBoxZeilen;
    private javax.swing.JLabel jLabelHeader;
    private javax.swing.JScrollPane jScrollPane;
    private javax.swing.JScrollPane jScrollPane1;
    private javax.swing.JTextArea jTextArea;
    private javax.swing.JTextPane jTextPane1;
    // End of variables declaration//GEN-END:variables

    private class BeobLoeschen implements ActionListener {

        @Override
        public void actionPerformed(ActionEvent arg0) {
            Log.clearText(logArt);
        }
    }
}
