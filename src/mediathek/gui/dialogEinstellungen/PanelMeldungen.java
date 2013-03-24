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

import java.awt.Component;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import javax.swing.text.DefaultCaret;
import mediathek.daten.DDaten;
import mediathek.daten.Daten;
import mediathek.gui.PanelVorlage;
import mediathek.tool.Konstanten;
import mediathek.tool.ListenerMediathekView;
import mediathek.tool.Log;

public class PanelMeldungen extends PanelVorlage {

    private StringBuffer text;
    private int logArt;
    private static int PANEL_NR_MAX = 0;
    private int panelNr = 0;

    public PanelMeldungen(DDaten d, Component parentComponent, StringBuffer ttext, int llogArt, String header) {
        super(d, parentComponent);
        PANEL_NR_MAX++;
        panelNr = PANEL_NR_MAX;
        initComponents();
        text = ttext;
        jLabelHeader.setText(header);
        logArt = llogArt;
        setText();
        //init
        ListenerMediathekView.addListener(new ListenerMediathekView(logArt, PanelMeldungen.class.getName() + String.valueOf(panelNr)) {
            // + String.valueOf(PANEL_NR) damit die unterschiedlichen Panel unterschieden werden
            @Override
            public void ping() {
                setLineWrab(); //setText wir da auch gemacht
            }
        });
        jButtonLoeschen.addActionListener(new BeobLoeschen());
        jCheckBoxAuto.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                setAuto();
            }
        });
        jCheckBoxUmbrechen.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                if (!stopBeob) {
                    Daten.system[getNrSystem()] = String.valueOf(jCheckBoxUmbrechen.isSelected());
                    ListenerMediathekView.notify(logArt, PanelMeldungen.class.getName() + String.valueOf(panelNr));
                    setLineWrab();
                }
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
        stopBeob = true;
        jCheckBoxUmbrechen.setSelected(Boolean.parseBoolean(Daten.system[getNrSystem()]));
        jTextArea.setLineWrap(jCheckBoxUmbrechen.isSelected());
        jTextArea.setWrapStyleWord(false);
        setText();
        stopBeob = false;
    }

    private int getNrSystem() {
        int nr = Konstanten.SYSTEM_MEDUNGSFENSTER_UMBRECHEN_SYSTEMMELDUNGEN_NR;
        switch (logArt) {
            case ListenerMediathekView.EREIGNIS_LOG_FEHLER:
                nr = Konstanten.SYSTEM_MEDUNGSFENSTER_UMBRECHEN_FEHLERMELDUNGEN_NR;
                break;
            case ListenerMediathekView.EREIGNIS_LOG_SYSTEM:
                nr = Konstanten.SYSTEM_MEDUNGSFENSTER_UMBRECHEN_SYSTEMMELDUNGEN_NR;
                break;
            case ListenerMediathekView.EREIGNIS_LOG_PLAYER:
                nr = Konstanten.SYSTEM_MEDUNGSFENSTER_UMBRECHEN_PLAYERMELDUNGEN_NR;
                break;
        }
        return nr;
    }

    private void setText() {
        String s = text.toString();
        jTextArea.setText(s);
    }

    // <editor-fold defaultstate="collapsed" desc="Generated Code">//GEN-BEGIN:initComponents
    private void initComponents() {

        jButtonLoeschen = new javax.swing.JButton();
        javax.swing.JScrollPane jScrollPane = new javax.swing.JScrollPane();
        jTextArea = new javax.swing.JTextArea();
        jLabelHeader = new javax.swing.JLabel();
        jCheckBoxAuto = new javax.swing.JCheckBox();
        jCheckBoxUmbrechen = new javax.swing.JCheckBox();

        jButtonLoeschen.setText("löschen");

        jTextArea.setEditable(false);
        jTextArea.setColumns(20);
        jTextArea.setRows(5);
        jScrollPane.setViewportView(jTextArea);

        jLabelHeader.setText("jLabel1");

        jCheckBoxAuto.setSelected(true);
        jCheckBoxAuto.setText("Autoscroll");

        jCheckBoxUmbrechen.setText("Zeilen umbrechen");

        javax.swing.GroupLayout layout = new javax.swing.GroupLayout(this);
        this.setLayout(layout);
        layout.setHorizontalGroup(
            layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(layout.createSequentialGroup()
                .addContainerGap()
                .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addComponent(jScrollPane, javax.swing.GroupLayout.Alignment.TRAILING)
                    .addGroup(layout.createSequentialGroup()
                        .addComponent(jCheckBoxAuto)
                        .addGap(18, 18, 18)
                        .addComponent(jCheckBoxUmbrechen)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED, 122, Short.MAX_VALUE)
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
                .addComponent(jScrollPane, javax.swing.GroupLayout.DEFAULT_SIZE, 374, Short.MAX_VALUE)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(jButtonLoeschen)
                    .addComponent(jCheckBoxAuto)
                    .addComponent(jCheckBoxUmbrechen))
                .addContainerGap())
        );
    }// </editor-fold>//GEN-END:initComponents
    // Variables declaration - do not modify//GEN-BEGIN:variables
    private javax.swing.JButton jButtonLoeschen;
    private javax.swing.JCheckBox jCheckBoxAuto;
    private javax.swing.JCheckBox jCheckBoxUmbrechen;
    private javax.swing.JLabel jLabelHeader;
    private javax.swing.JTextArea jTextArea;
    // End of variables declaration//GEN-END:variables

    private class BeobLoeschen implements ActionListener {

        @Override
        public void actionPerformed(ActionEvent arg0) {
            Log.clearText(logArt);
        }
    }
}
