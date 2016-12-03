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

import javafx.beans.Observable;
import javafx.collections.ObservableList;
import mSearch.tool.Log;
import mSearch.tool.SysMsg;
import mediathek.config.Daten;
import mediathek.config.MVConfig;
import mediathek.gui.PanelVorlage;

import javax.swing.*;
import javax.swing.text.DefaultCaret;
import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

@SuppressWarnings("serial")
public class PanelMeldungen extends PanelVorlage {
    private final int logArt;
    private int firstScroll = 25;
    private static final Color cGruen = new Color(0, 153, 51);
    private static final Color cRot = new Color(255, 0, 0);

    public PanelMeldungen(Daten d, JFrame parentComponent, int logArt, String header) {
        super(d, parentComponent);
        initComponents();
        jLabelHeader.setText(header);
        this.logArt = logArt;
        ObservableList<String> text;
        if (logArt == SysMsg.LOG_SYSTEM) {
            text = SysMsg.textSystem;
            jCheckBoxAuto.setSelected(true);
        } else {
            text = SysMsg.textProgramm;
            jCheckBoxAuto.setSelected(false);
        }
        jCheckBoxAuto.setForeground(jCheckBoxAuto.isSelected() ? cGruen : cRot);
        jButtonLoeschen.addActionListener(new BeobLoeschen());
        jCheckBoxAuto.addActionListener((ActionEvent e) -> {
            jCheckBoxAuto.setForeground(jCheckBoxAuto.isSelected() ? cGruen : cRot);
            setAuto();
        });
        jCheckBoxUmbrechen.addActionListener((ActionEvent e) -> {
            if (!stopBeob) {
                MVConfig.add(getNrSystem(), String.valueOf(jCheckBoxUmbrechen.isSelected()));
                setLineWrab();
            }
        });
        setAuto();
        setLineWrab();
        text.addListener((Observable ll) -> {
            try {
                if (SwingUtilities.isEventDispatchThread()) {
                    notifyPanel();
                } else {
                    SwingUtilities.invokeLater(PanelMeldungen.this::notifyPanel);
                }
            } catch (Exception ex) {
                Log.errorLog(912365478, ex);
            }
        });
    }

    public void notifyPanel() {
        if (jCheckBoxAuto.isSelected() || firstScroll > 0) {
            if (firstScroll > 0) {
                --firstScroll;
            }
            setLineWrab(); //setText wir da auch gemacht
        }
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
        jCheckBoxUmbrechen.setSelected(Boolean.parseBoolean(MVConfig.get(getNrSystem())));
        jTextArea.setLineWrap(jCheckBoxUmbrechen.isSelected());
        jTextArea.setWrapStyleWord(false);
        setText();
        stopBeob = false;
    }

    private MVConfig.Configs getNrSystem() {
        MVConfig.Configs nr = MVConfig.Configs.SYSTEM_MEDUNGSFENSTER_UMBRECHEN_SYSTEMMELDUNGEN;
        switch (logArt) {
            case SysMsg.LOG_SYSTEM:
                nr = MVConfig.Configs.SYSTEM_MEDUNGSFENSTER_UMBRECHEN_SYSTEMMELDUNGEN;
                break;
            case SysMsg.LOG_PLAYER:
                nr = MVConfig.Configs.SYSTEM_MEDUNGSFENSTER_UMBRECHEN_PLAYERMELDUNGEN;
                break;
        }
        return nr;
    }

    private synchronized void setText() {
        jTextArea.setText(SysMsg.getText(logArt));
    }

    // <editor-fold defaultstate="collapsed" desc="Generated Code">//GEN-BEGIN:initComponents
    private void initComponents() {

        jButtonLoeschen = new javax.swing.JButton();
        javax.swing.JScrollPane jScrollPane = new javax.swing.JScrollPane();
        jTextArea = new javax.swing.JTextArea();
        jLabelHeader = new javax.swing.JLabel();
        jCheckBoxAuto = new javax.swing.JCheckBox();
        jCheckBoxUmbrechen = new javax.swing.JCheckBox();

        jButtonLoeschen.setText("Löschen");

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
                                        .addComponent(jScrollPane, javax.swing.GroupLayout.Alignment.TRAILING, javax.swing.GroupLayout.DEFAULT_SIZE, 429, Short.MAX_VALUE)
                                        .addGroup(layout.createSequentialGroup()
                                                .addComponent(jLabelHeader)
                                                .addGap(0, 0, Short.MAX_VALUE))
                                        .addGroup(layout.createSequentialGroup()
                                                .addComponent(jCheckBoxAuto)
                                                .addGap(18, 18, 18)
                                                .addComponent(jCheckBoxUmbrechen)
                                                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED, 69, Short.MAX_VALUE)
                                                .addComponent(jButtonLoeschen)))
                                .addContainerGap())
        );
        layout.setVerticalGroup(
                layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                        .addGroup(javax.swing.GroupLayout.Alignment.TRAILING, layout.createSequentialGroup()
                                .addContainerGap()
                                .addComponent(jLabelHeader)
                                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                                .addComponent(jScrollPane, javax.swing.GroupLayout.DEFAULT_SIZE, 328, Short.MAX_VALUE)
                                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                                .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.CENTER)
                                        .addComponent(jCheckBoxAuto)
                                        .addComponent(jCheckBoxUmbrechen)
                                        .addComponent(jButtonLoeschen))
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
            SysMsg.clearText(logArt);
            setText();
        }
    }
}
