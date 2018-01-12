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

import com.jidesoft.utils.SystemInfo;
import mediathek.MediathekGui;
import mediathek.config.Icons;

import javax.swing.*;
import javax.swing.event.ChangeListener;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyEvent;

@SuppressWarnings("serial")
public class MVFilterPanel extends JPanel implements MVFilter {

    public MVFilterPanel(MediathekGui aMediathekGui) {
        initComponents();

        if (SystemInfo.isWindows()) {
            // zum Abfangen der Win-F4 für comboboxen
            InputMap im = jComboBoxFilterSender.getInputMap();
            im.put(KeyStroke.getKeyStroke(KeyEvent.VK_F4, 0), "einstellungen");
            ActionMap am = jComboBoxFilterSender.getActionMap();
            am.put("einstellungen", new AbstractAction() {
                @Override
                public void actionPerformed(ActionEvent e) {
                    aMediathekGui.showSettingsDialog();
                }
            });
            im = jComboBoxFilterThema.getInputMap();
            im.put(KeyStroke.getKeyStroke(KeyEvent.VK_F4, 0), "einstellungen");
            am = jComboBoxFilterThema.getActionMap();
            am.put("einstellungen", new AbstractAction() {
                @Override
                public void actionPerformed(ActionEvent e) {
                    aMediathekGui.showSettingsDialog();
                }
            });
        }
        jButtonClearAll.setIcon(Icons.ICON_BUTTON_CLEAR);

        setVisible(true);
    }

    // <editor-fold defaultstate="collapsed" desc="Generated Code">//GEN-BEGIN:initComponents
    private void initComponents() {

        javax.swing.JPanel jPanel2 = new javax.swing.JPanel();
        javax.swing.JLabel jLabel1 = new javax.swing.JLabel();
        jSliderTage = new javax.swing.JSlider();
        jTextFieldFilterTage = new javax.swing.JTextField();
        javax.swing.JPanel jPanel3 = new javax.swing.JPanel();
        javax.swing.JPanel jPanel6 = new javax.swing.JPanel();
        javax.swing.JPanel jPanel5 = new javax.swing.JPanel();
        javax.swing.JPanel jPanel7 = new javax.swing.JPanel();
        javax.swing.JLabel jLabel2 = new javax.swing.JLabel();
        jComboBoxFilterSender = new javax.swing.JComboBox<>();
        javax.swing.JLabel jLabel4 = new javax.swing.JLabel();
        jComboBoxFilterThema = new javax.swing.JComboBox<>();
        jButtonClearAll = new javax.swing.JButton();

        jLabel1.setText("Zeitraum [Tage]:");

        jSliderTage.setMaximum(30);
        jSliderTage.setValue(15);

        jTextFieldFilterTage.setEditable(false);

        jPanel3.setLayout(new org.jdesktop.swingx.VerticalLayout());

        jPanel6.setLayout(new org.jdesktop.swingx.VerticalLayout());

        jPanel5.setLayout(new org.jdesktop.swingx.HorizontalLayout());
        jPanel6.add(jPanel5);

        jPanel7.setLayout(new org.jdesktop.swingx.VerticalLayout());

        jLabel2.setText("Sender:");
        jPanel7.add(jLabel2);

        jComboBoxFilterSender.setMaximumRowCount(25);
        jComboBoxFilterSender.setPreferredSize(new java.awt.Dimension(184, 24));
        jPanel7.add(jComboBoxFilterSender);

        jLabel4.setText("Thema:");
        jPanel7.add(jLabel4);

        jComboBoxFilterThema.setMaximumRowCount(20);
        jComboBoxFilterThema.setPreferredSize(new java.awt.Dimension(184, 24));
        jPanel7.add(jComboBoxFilterThema);

        jButtonClearAll.setIcon(new javax.swing.ImageIcon(getClass().getResource("/mediathek/res/muster/button-clear.png"))); // NOI18N
        jButtonClearAll.setToolTipText("Alles löschen");

        javax.swing.GroupLayout jPanel2Layout = new javax.swing.GroupLayout(jPanel2);
        jPanel2.setLayout(jPanel2Layout);
        jPanel2Layout.setHorizontalGroup(
            jPanel2Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel2Layout.createSequentialGroup()
                .addContainerGap()
                .addGroup(jPanel2Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addGroup(jPanel2Layout.createSequentialGroup()
                            .addComponent(jSliderTage, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                            .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                        .addComponent(jTextFieldFilterTage, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE))
                    .addGroup(jPanel2Layout.createSequentialGroup()
                            .addComponent(jLabel1)
                            .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                            .addComponent(jPanel6, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
                        .addComponent(jPanel7, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                    .addGroup(jPanel2Layout.createSequentialGroup()
                        .addGroup(jPanel2Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                                .addComponent(jPanel3, javax.swing.GroupLayout.PREFERRED_SIZE, 141, javax.swing.GroupLayout.PREFERRED_SIZE)
                                .addComponent(jButtonClearAll, javax.swing.GroupLayout.PREFERRED_SIZE, 141, javax.swing.GroupLayout.PREFERRED_SIZE))
                            .addGap(0, 0, Short.MAX_VALUE)))
                .addContainerGap())
        );
        jPanel2Layout.setVerticalGroup(
            jPanel2Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel2Layout.createSequentialGroup()
                    .addContainerGap()
                    .addComponent(jPanel7, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                    .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                    .addGroup(jPanel2Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                            .addComponent(jPanel6, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                            .addComponent(jLabel1))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                    .addGroup(jPanel2Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.TRAILING)
                            .addComponent(jTextFieldFilterTage, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                            .addComponent(jSliderTage, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE))
                    .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                    .addComponent(jButtonClearAll)
                    .addGap(41, 41, 41)
                    .addComponent(jPanel3, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE))
        );

        jPanel2Layout.linkSize(javax.swing.SwingConstants.VERTICAL, new java.awt.Component[]{jLabel1, jTextFieldFilterTage});

        javax.swing.GroupLayout layout = new javax.swing.GroupLayout(this);
        this.setLayout(layout);
        layout.setHorizontalGroup(
            layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addComponent(jPanel2, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
        );
        layout.setVerticalGroup(
            layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addGroup(layout.createSequentialGroup()
                            .addComponent(jPanel2, javax.swing.GroupLayout.PREFERRED_SIZE, 193, javax.swing.GroupLayout.PREFERRED_SIZE)
                            .addGap(0, 6, Short.MAX_VALUE))
        );
    }// </editor-fold>//GEN-END:initComponents


    // Variables declaration - do not modify//GEN-BEGIN:variables
    private javax.swing.JButton jButtonClearAll;
    public javax.swing.JComboBox<String> jComboBoxFilterSender;
    public javax.swing.JComboBox<String> jComboBoxFilterThema;
    private javax.swing.JSlider jSliderTage;
    private javax.swing.JTextField jTextFieldFilterTage;
    // End of variables declaration//GEN-END:variables

    @Override
    public void mvFdeleteFilter(int i) {
    }

    @Override
    public void mvFsaveFilter(int i) {
    }

    @Override
    public void mvFfilter(int i) {
    }

    @Override
    public JButton get_jButtonClearAll() {
        return jButtonClearAll;
    }

    @Override
    public JComboBox<String> get_jComboBoxFilterSender() {
        return jComboBoxFilterSender;
    }

    @Override
    public JComboBox<String> get_jComboBoxFilterThema() {
        return jComboBoxFilterThema;
    }

    @Override
    public JSlider get_jSliderTage() {
        return jSliderTage;
    }

    @Override
    public JTextField get_jTextFieldFilterTage() {
        return jTextFieldFilterTage;
    }

    @Override
    public void removeAllListener() {
        for (ActionListener a : jComboBoxFilterSender.getActionListeners()) {
            jComboBoxFilterSender.removeActionListener(a);
        }
        for (ActionListener a : jComboBoxFilterThema.getActionListeners()) {
            jComboBoxFilterThema.removeActionListener(a);
        }
        for (ActionListener a : jTextFieldFilterTage.getActionListeners()) {
            jTextFieldFilterTage.removeActionListener(a);
        }
        for (ChangeListener a : jSliderTage.getChangeListeners()) {
            jSliderTage.removeChangeListener(a);
        }
    }

}
