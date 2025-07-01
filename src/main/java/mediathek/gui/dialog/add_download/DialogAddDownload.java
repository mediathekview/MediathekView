/*
 * Copyright (c) 2025 derreisende77.
 * This code was developed as part of the MediathekView project https://github.com/mediathekview/MediathekView
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package mediathek.gui.dialog.add_download;

import org.jdesktop.swingx.JXBusyLabel;

import javax.swing.*;
import javax.swing.border.TitledBorder;
import java.awt.*;

/**
 * Base class for UI Designer.
 * Subclass uses kotlin coroutines for concurrent work.
 */
public class DialogAddDownload extends JDialog {
    public DialogAddDownload(Frame parent) {
        super(parent, true);
        initComponents();
    }

    // <editor-fold defaultstate="collapsed" desc="Generated Code">//GEN-BEGIN:initComponents
    // Generated using JFormDesigner non-commercial license
    private void initComponents() {
        var panel2 = new JPanel();
        var panel1 = new JPanel();
        jButtonOk = new JButton();
        jButtonAbbrechen = new JButton();
        jCheckBoxStarten = new JCheckBox();
        var jPanel1 = new JPanel();
        var jPanel2 = new JPanel();
        jCheckBoxInfodatei = new JCheckBox();
        jCheckBoxPfadSpeichern = new JCheckBox();
        jCheckBoxSubtitle = new JCheckBox();
        var jPanel7 = new JPanel();
        var jLabel1 = new JLabel();
        jTextFieldName = new JTextField();
        var jLabelSet = new JLabel();
        jComboBoxPset = new javax.swing.JComboBox<>();
        var jLabel4 = new JLabel();
        var jPanel4 = new JPanel();
        jComboBoxPfad = new javax.swing.JComboBox<>();
        jButtonZiel = new JButton();
        jButtonDelHistory = new JButton();
        jPanelSize = new JPanel();
        var jPanel3 = new JPanel();
        btnRequestLiveInfo = new JButton();
        lblBusyIndicator = new JXBusyLabel();
        var jPanel5 = new JPanel();
        lblStatus = new JLabel();
        lblAudioInfo = new JLabel();
        var jPanel6 = new JPanel();
        jRadioButtonAufloesungHd = new JRadioButton();
        jRadioButtonAufloesungHoch = new JRadioButton();
        jRadioButtonAufloesungKlein = new JRadioButton();
        jTextFieldSender = new JTextField();

        //======== this ========
        setDefaultCloseOperation(WindowConstants.DISPOSE_ON_CLOSE);
        setTitle("Film speichern");
        setMinimumSize(new Dimension(660, 420));
        setPreferredSize(new Dimension(660, 420));
        var contentPane = getContentPane();

        //======== panel2 ========
        {

            //======== panel1 ========
            {

                //---- jButtonOk ----
                jButtonOk.setText("Speichern");

                //---- jButtonAbbrechen ----
                jButtonAbbrechen.setText("Abbrechen");

                GroupLayout panel1Layout = new GroupLayout(panel1);
                panel1.setLayout(panel1Layout);
                panel1Layout.setHorizontalGroup(
                    panel1Layout.createParallelGroup()
                        .addGroup(panel1Layout.createSequentialGroup()
                            .addContainerGap()
                            .addComponent(jButtonOk, GroupLayout.PREFERRED_SIZE, 93, GroupLayout.PREFERRED_SIZE)
                            .addPreferredGap(LayoutStyle.ComponentPlacement.RELATED)
                            .addComponent(jButtonAbbrechen)
                            .addContainerGap())
                );
                panel1Layout.linkSize(SwingConstants.HORIZONTAL, new Component[] {jButtonAbbrechen, jButtonOk});
                panel1Layout.setVerticalGroup(
                    panel1Layout.createParallelGroup()
                        .addGroup(panel1Layout.createSequentialGroup()
                            .addContainerGap()
                            .addGroup(panel1Layout.createParallelGroup(GroupLayout.Alignment.BASELINE)
                                .addComponent(jButtonOk)
                                .addComponent(jButtonAbbrechen))
                            .addContainerGap())
                );
            }

            //---- jCheckBoxStarten ----
            jCheckBoxStarten.setSelected(true);
            jCheckBoxStarten.setText("Download sofort starten");

            GroupLayout panel2Layout = new GroupLayout(panel2);
            panel2.setLayout(panel2Layout);
            panel2Layout.setHorizontalGroup(
                panel2Layout.createParallelGroup()
                    .addGroup(panel2Layout.createSequentialGroup()
                        .addContainerGap()
                        .addComponent(jCheckBoxStarten)
                        .addPreferredGap(LayoutStyle.ComponentPlacement.RELATED, GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                        .addComponent(panel1, GroupLayout.PREFERRED_SIZE, GroupLayout.DEFAULT_SIZE, GroupLayout.PREFERRED_SIZE)
                        .addContainerGap())
            );
            panel2Layout.setVerticalGroup(
                panel2Layout.createParallelGroup()
                    .addGroup(panel2Layout.createSequentialGroup()
                        .addContainerGap()
                        .addGroup(panel2Layout.createParallelGroup()
                            .addComponent(jCheckBoxStarten)
                            .addComponent(panel1, GroupLayout.PREFERRED_SIZE, GroupLayout.DEFAULT_SIZE, GroupLayout.PREFERRED_SIZE))
                        .addContainerGap())
            );
        }

        //======== jPanel1 ========
        {

            //======== jPanel2 ========
            {
                jPanel2.setLayout(new GridLayout(2, 2));

                //---- jCheckBoxInfodatei ----
                jCheckBoxInfodatei.setText("Lege Infodatei an");
                jCheckBoxInfodatei.setToolTipText("Erzeugt eine Infodatei im Format \"Infodatei.txt\"");
                jPanel2.add(jCheckBoxInfodatei);

                //---- jCheckBoxPfadSpeichern ----
                jCheckBoxPfadSpeichern.setText("Zielpfad speichern");
                jPanel2.add(jCheckBoxPfadSpeichern);

                //---- jCheckBoxSubtitle ----
                jCheckBoxSubtitle.setText("Untertitel speichern: \"Filmname.xxx\"");
                jPanel2.add(jCheckBoxSubtitle);
            }

            //======== jPanel7 ========
            {
                jPanel7.setMaximumSize(new Dimension(606, 32767));

                //---- jLabel1 ----
                jLabel1.setText("Zielpfad:");

                //---- jLabelSet ----
                jLabelSet.setText("Set:");

                //---- jLabel4 ----
                jLabel4.setText("Dateiname:");

                //======== jPanel4 ========
                {
                    jPanel4.setLayout(new BoxLayout(jPanel4, BoxLayout.X_AXIS));

                    //---- jComboBoxPfad ----
                    jComboBoxPfad.setEditable(true);
                    jPanel4.add(jComboBoxPfad);

                    //---- jButtonZiel ----
                    jButtonZiel.setText("F");
                    jButtonZiel.setToolTipText("Zielpfad ausw\u00e4hlen");
                    jPanel4.add(jButtonZiel);

                    //---- jButtonDelHistory ----
                    jButtonDelHistory.setText("H");
                    jButtonDelHistory.setToolTipText("History l\u00f6schen");
                    jPanel4.add(jButtonDelHistory);
                }

                GroupLayout jPanel7Layout = new GroupLayout(jPanel7);
                jPanel7.setLayout(jPanel7Layout);
                jPanel7Layout.setHorizontalGroup(
                    jPanel7Layout.createParallelGroup()
                        .addGroup(jPanel7Layout.createSequentialGroup()
                            .addContainerGap()
                            .addGroup(jPanel7Layout.createParallelGroup()
                                .addGroup(jPanel7Layout.createSequentialGroup()
                                    .addGroup(jPanel7Layout.createParallelGroup()
                                        .addComponent(jLabelSet)
                                        .addComponent(jLabel1))
                                    .addGap(20, 20, 20)
                                    .addGroup(jPanel7Layout.createParallelGroup()
                                        .addComponent(jPanel4, GroupLayout.DEFAULT_SIZE, GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                                        .addComponent(jComboBoxPset)))
                                .addGroup(jPanel7Layout.createSequentialGroup()
                                    .addComponent(jLabel4)
                                    .addPreferredGap(LayoutStyle.ComponentPlacement.RELATED)
                                    .addComponent(jTextFieldName)))
                            .addContainerGap())
                );
                jPanel7Layout.setVerticalGroup(
                    jPanel7Layout.createParallelGroup()
                        .addGroup(jPanel7Layout.createSequentialGroup()
                            .addContainerGap()
                            .addGroup(jPanel7Layout.createParallelGroup(GroupLayout.Alignment.BASELINE)
                                .addComponent(jLabelSet)
                                .addComponent(jComboBoxPset, GroupLayout.PREFERRED_SIZE, GroupLayout.DEFAULT_SIZE, GroupLayout.PREFERRED_SIZE))
                            .addPreferredGap(LayoutStyle.ComponentPlacement.UNRELATED)
                            .addGroup(jPanel7Layout.createParallelGroup(GroupLayout.Alignment.TRAILING)
                                .addComponent(jPanel4, GroupLayout.PREFERRED_SIZE, GroupLayout.DEFAULT_SIZE, GroupLayout.PREFERRED_SIZE)
                                .addComponent(jLabel1))
                            .addPreferredGap(LayoutStyle.ComponentPlacement.RELATED)
                            .addGroup(jPanel7Layout.createParallelGroup(GroupLayout.Alignment.CENTER)
                                .addComponent(jLabel4)
                                .addComponent(jTextFieldName, GroupLayout.PREFERRED_SIZE, GroupLayout.DEFAULT_SIZE, GroupLayout.PREFERRED_SIZE))
                            .addContainerGap())
                );
            }

            GroupLayout jPanel1Layout = new GroupLayout(jPanel1);
            jPanel1.setLayout(jPanel1Layout);
            jPanel1Layout.setHorizontalGroup(
                jPanel1Layout.createParallelGroup()
                    .addGroup(jPanel1Layout.createSequentialGroup()
                        .addContainerGap()
                        .addGroup(jPanel1Layout.createParallelGroup()
                            .addComponent(jPanel2, GroupLayout.DEFAULT_SIZE, 824, Short.MAX_VALUE)
                            .addComponent(jPanel7, GroupLayout.DEFAULT_SIZE, GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
                        .addContainerGap())
            );
            jPanel1Layout.setVerticalGroup(
                jPanel1Layout.createParallelGroup()
                    .addGroup(jPanel1Layout.createSequentialGroup()
                        .addContainerGap()
                        .addComponent(jPanel7, GroupLayout.PREFERRED_SIZE, GroupLayout.DEFAULT_SIZE, GroupLayout.PREFERRED_SIZE)
                        .addGap(12, 12, 12)
                        .addComponent(jPanel2, GroupLayout.PREFERRED_SIZE, GroupLayout.DEFAULT_SIZE, GroupLayout.PREFERRED_SIZE)
                        .addContainerGap(GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
            );
        }

        //======== jPanelSize ========
        {
            jPanelSize.setBorder(new TitledBorder("Download-Qualit\u00e4t"));

            //======== jPanel3 ========
            {
                jPanel3.setLayout(new FlowLayout(FlowLayout.LEFT));

                //---- btnRequestLiveInfo ----
                btnRequestLiveInfo.setText("Codec-Details abrufen...");
                jPanel3.add(btnRequestLiveInfo);
                jPanel3.add(lblBusyIndicator);

                //======== jPanel5 ========
                {
                    jPanel5.setLayout(new GridLayout(2, 1));

                    //---- lblStatus ----
                    lblStatus.setText("status");
                    jPanel5.add(lblStatus);

                    //---- lblAudioInfo ----
                    lblAudioInfo.setText("audio");
                    jPanel5.add(lblAudioInfo);
                }
                jPanel3.add(jPanel5);
            }

            //======== jPanel6 ========
            {
                jPanel6.setLayout(new FlowLayout());

                //---- jRadioButtonAufloesungHd ----
                jRadioButtonAufloesungHd.setText("H\u00f6chste/Hoch");
                jPanel6.add(jRadioButtonAufloesungHd);

                //---- jRadioButtonAufloesungHoch ----
                jRadioButtonAufloesungHoch.setText("Mittel");
                jPanel6.add(jRadioButtonAufloesungHoch);

                //---- jRadioButtonAufloesungKlein ----
                jRadioButtonAufloesungKlein.setText("Niedrig");
                jPanel6.add(jRadioButtonAufloesungKlein);
            }

            GroupLayout jPanelSizeLayout = new GroupLayout(jPanelSize);
            jPanelSize.setLayout(jPanelSizeLayout);
            jPanelSizeLayout.setHorizontalGroup(
                jPanelSizeLayout.createParallelGroup()
                    .addGroup(jPanelSizeLayout.createSequentialGroup()
                        .addContainerGap()
                        .addGroup(jPanelSizeLayout.createParallelGroup()
                            .addComponent(jPanel3, GroupLayout.DEFAULT_SIZE, 814, Short.MAX_VALUE)
                            .addGroup(jPanelSizeLayout.createSequentialGroup()
                                .addComponent(jPanel6, GroupLayout.PREFERRED_SIZE, GroupLayout.DEFAULT_SIZE, GroupLayout.PREFERRED_SIZE)
                                .addGap(0, 0, Short.MAX_VALUE)))
                        .addContainerGap())
            );
            jPanelSizeLayout.setVerticalGroup(
                jPanelSizeLayout.createParallelGroup()
                    .addGroup(jPanelSizeLayout.createSequentialGroup()
                        .addContainerGap()
                        .addComponent(jPanel6, GroupLayout.PREFERRED_SIZE, GroupLayout.DEFAULT_SIZE, GroupLayout.PREFERRED_SIZE)
                        .addPreferredGap(LayoutStyle.ComponentPlacement.RELATED)
                        .addComponent(jPanel3, GroupLayout.PREFERRED_SIZE, GroupLayout.DEFAULT_SIZE, GroupLayout.PREFERRED_SIZE)
                        .addContainerGap(9, Short.MAX_VALUE))
            );
        }

        //---- jTextFieldSender ----
        jTextFieldSender.setEditable(false);
        jTextFieldSender.setFont(jTextFieldSender.getFont().deriveFont(jTextFieldSender.getFont().getStyle() | Font.BOLD));
        jTextFieldSender.setText(" ARD: Tatort, ...");
        jTextFieldSender.setBorder(new TitledBorder("Film"));

        GroupLayout contentPaneLayout = new GroupLayout(contentPane);
        contentPane.setLayout(contentPaneLayout);
        contentPaneLayout.setHorizontalGroup(
            contentPaneLayout.createParallelGroup()
                .addGroup(contentPaneLayout.createSequentialGroup()
                    .addContainerGap()
                    .addGroup(contentPaneLayout.createParallelGroup()
                        .addComponent(jPanel1, GroupLayout.Alignment.TRAILING, GroupLayout.DEFAULT_SIZE, GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                        .addComponent(jTextFieldSender)
                        .addComponent(panel2, GroupLayout.DEFAULT_SIZE, GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                        .addComponent(jPanelSize, GroupLayout.DEFAULT_SIZE, GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
                    .addContainerGap())
        );
        contentPaneLayout.setVerticalGroup(
            contentPaneLayout.createParallelGroup()
                .addGroup(contentPaneLayout.createSequentialGroup()
                    .addContainerGap()
                    .addComponent(jTextFieldSender, GroupLayout.PREFERRED_SIZE, GroupLayout.DEFAULT_SIZE, GroupLayout.PREFERRED_SIZE)
                    .addPreferredGap(LayoutStyle.ComponentPlacement.RELATED)
                    .addComponent(jPanel1, GroupLayout.PREFERRED_SIZE, GroupLayout.DEFAULT_SIZE, GroupLayout.PREFERRED_SIZE)
                    .addPreferredGap(LayoutStyle.ComponentPlacement.RELATED)
                    .addComponent(jPanelSize, GroupLayout.DEFAULT_SIZE, GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                    .addPreferredGap(LayoutStyle.ComponentPlacement.RELATED)
                    .addComponent(panel2, GroupLayout.PREFERRED_SIZE, GroupLayout.DEFAULT_SIZE, GroupLayout.PREFERRED_SIZE)
                    .addContainerGap())
        );
        pack();
        setLocationRelativeTo(getOwner());

        //---- buttonGroup1 ----
        var buttonGroup1 = new ButtonGroup();
        buttonGroup1.add(jRadioButtonAufloesungHd);
        buttonGroup1.add(jRadioButtonAufloesungHoch);
        buttonGroup1.add(jRadioButtonAufloesungKlein);
    }// </editor-fold>//GEN-END:initComponents

    // Variables declaration - do not modify//GEN-BEGIN:variables
    // Generated using JFormDesigner non-commercial license
    protected JButton jButtonOk;
    protected JButton jButtonAbbrechen;
    protected JCheckBox jCheckBoxStarten;
    protected JCheckBox jCheckBoxInfodatei;
    protected JCheckBox jCheckBoxPfadSpeichern;
    protected JCheckBox jCheckBoxSubtitle;
    protected JTextField jTextFieldName;
    protected JComboBox<String> jComboBoxPset;
    protected JComboBox<String> jComboBoxPfad;
    protected JButton jButtonZiel;
    protected JButton jButtonDelHistory;
    protected JPanel jPanelSize;
    protected JButton btnRequestLiveInfo;
    protected JXBusyLabel lblBusyIndicator;
    protected JLabel lblStatus;
    protected JLabel lblAudioInfo;
    protected JRadioButton jRadioButtonAufloesungHd;
    protected JRadioButton jRadioButtonAufloesungHoch;
    protected JRadioButton jRadioButtonAufloesungKlein;
    protected JTextField jTextFieldSender;
    // End of variables declaration//GEN-END:variables
}
