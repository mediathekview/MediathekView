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

package mediathek.gui.dialogEinstellungen.pset;

import mediathek.tool.SVGIconUtilities;

import javax.swing.*;
import javax.swing.border.LineBorder;
import javax.swing.border.TitledBorder;
import java.awt.*;

public class PanelPsetKurzBase extends JPanel {
    protected PanelPsetKurzBase() {
        initComponents();
    }

    // <editor-fold defaultstate="collapsed" desc="Generated Code">//GEN-BEGIN:initComponents
    // Generated using JFormDesigner non-commercial license
    private void initComponents() {
        var jPanel2 = new JPanel();
        jPanelExtra = new JPanel();
        var jPanel1 = new JPanel();
        var jLabel1 = new JLabel();
        var jLabel2 = new JLabel();
        jTextFieldZiel = new JTextField();
        jTextFieldName = new JTextField();
        jButtonZiel = new JButton();
        var jScrollPane2 = new JScrollPane();
        jTextArea1 = new JTextArea();
        var jScrollPane1 = new JScrollPane();
        jListPset = new JList<>();

        //======== this ========

        //======== jPanel2 ========
        {
            jPanel2.setBorder(new TitledBorder("<html><b>Programme</b></html>"));

            //======== jPanelExtra ========
            {

                GroupLayout jPanelExtraLayout = new GroupLayout(jPanelExtra);
                jPanelExtra.setLayout(jPanelExtraLayout);
                jPanelExtraLayout.setHorizontalGroup(
                    jPanelExtraLayout.createParallelGroup()
                        .addGap(0, 491, Short.MAX_VALUE)
                );
                jPanelExtraLayout.setVerticalGroup(
                    jPanelExtraLayout.createParallelGroup()
                        .addGap(0, 277, Short.MAX_VALUE)
                );
            }

            GroupLayout jPanel2Layout = new GroupLayout(jPanel2);
            jPanel2.setLayout(jPanel2Layout);
            jPanel2Layout.setHorizontalGroup(
                jPanel2Layout.createParallelGroup()
                    .addComponent(jPanelExtra, GroupLayout.DEFAULT_SIZE, GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
            );
            jPanel2Layout.setVerticalGroup(
                jPanel2Layout.createParallelGroup()
                    .addGroup(GroupLayout.Alignment.TRAILING, jPanel2Layout.createSequentialGroup()
                        .addContainerGap()
                        .addComponent(jPanelExtra, GroupLayout.DEFAULT_SIZE, GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
            );
        }

        //======== jPanel1 ========
        {
            jPanel1.setBorder(new TitledBorder("<html><b>Set</b></html>"));

            //---- jLabel1 ----
            jLabel1.setText("Set Name:");

            //---- jLabel2 ----
            jLabel2.setText("Zielpfad:");

            //---- jButtonZiel ----
            jButtonZiel.setIcon(SVGIconUtilities.createSVGIcon("icons/fontawesome/folder-open.svg"));
            jButtonZiel.setToolTipText("Pfad ausw\u00e4hlen");

            //======== jScrollPane2 ========
            {
                jScrollPane2.setBorder(new LineBorder(UIManager.getColor("TextField.selectionBackground")));

                //---- jTextArea1 ----
                jTextArea1.setEditable(false);
                jTextArea1.setBackground(UIManager.getColor("TextField.inactiveBackground"));
                jTextArea1.setColumns(20);
                jTextArea1.setRows(4);
                jScrollPane2.setViewportView(jTextArea1);
            }

            //======== jScrollPane1 ========
            {
                jScrollPane1.setViewportView(jListPset);
            }

            GroupLayout jPanel1Layout = new GroupLayout(jPanel1);
            jPanel1.setLayout(jPanel1Layout);
            jPanel1Layout.setHorizontalGroup(
                jPanel1Layout.createParallelGroup()
                    .addGroup(jPanel1Layout.createSequentialGroup()
                        .addContainerGap()
                        .addGroup(jPanel1Layout.createParallelGroup()
                            .addComponent(jScrollPane2, GroupLayout.DEFAULT_SIZE, 479, Short.MAX_VALUE)
                            .addGroup(jPanel1Layout.createSequentialGroup()
                                .addGroup(jPanel1Layout.createParallelGroup()
                                    .addComponent(jLabel1)
                                    .addComponent(jLabel2))
                                .addPreferredGap(LayoutStyle.ComponentPlacement.RELATED)
                                .addGroup(jPanel1Layout.createParallelGroup()
                                    .addGroup(jPanel1Layout.createSequentialGroup()
                                        .addComponent(jTextFieldZiel)
                                        .addPreferredGap(LayoutStyle.ComponentPlacement.RELATED)
                                        .addComponent(jButtonZiel))
                                    .addComponent(jTextFieldName)))
                            .addComponent(jScrollPane1))
                        .addContainerGap())
            );
            jPanel1Layout.setVerticalGroup(
                jPanel1Layout.createParallelGroup()
                    .addGroup(jPanel1Layout.createSequentialGroup()
                        .addContainerGap()
                        .addComponent(jScrollPane1, GroupLayout.PREFERRED_SIZE, 116, GroupLayout.PREFERRED_SIZE)
                        .addPreferredGap(LayoutStyle.ComponentPlacement.RELATED)
                        .addGroup(jPanel1Layout.createParallelGroup(GroupLayout.Alignment.BASELINE)
                            .addComponent(jLabel1)
                            .addComponent(jTextFieldName, GroupLayout.PREFERRED_SIZE, GroupLayout.DEFAULT_SIZE, GroupLayout.PREFERRED_SIZE))
                        .addPreferredGap(LayoutStyle.ComponentPlacement.RELATED)
                        .addGroup(jPanel1Layout.createParallelGroup()
                            .addComponent(jButtonZiel)
                            .addGroup(jPanel1Layout.createParallelGroup(GroupLayout.Alignment.BASELINE)
                                .addComponent(jLabel2)
                                .addComponent(jTextFieldZiel, GroupLayout.PREFERRED_SIZE, GroupLayout.DEFAULT_SIZE, GroupLayout.PREFERRED_SIZE)))
                        .addPreferredGap(LayoutStyle.ComponentPlacement.UNRELATED)
                        .addComponent(jScrollPane2, GroupLayout.PREFERRED_SIZE, GroupLayout.DEFAULT_SIZE, GroupLayout.PREFERRED_SIZE)
                        .addContainerGap(GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
            );
            jPanel1Layout.linkSize(SwingConstants.VERTICAL, new Component[] {jButtonZiel, jTextFieldName, jTextFieldZiel});
        }

        GroupLayout layout = new GroupLayout(this);
        setLayout(layout);
        layout.setHorizontalGroup(
            layout.createParallelGroup()
                .addGroup(layout.createSequentialGroup()
                    .addContainerGap()
                    .addGroup(layout.createParallelGroup()
                        .addComponent(jPanel1, GroupLayout.DEFAULT_SIZE, GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                        .addComponent(jPanel2, GroupLayout.DEFAULT_SIZE, GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
                    .addContainerGap())
        );
        layout.setVerticalGroup(
            layout.createParallelGroup()
                .addGroup(layout.createSequentialGroup()
                    .addContainerGap()
                    .addComponent(jPanel1, GroupLayout.PREFERRED_SIZE, GroupLayout.DEFAULT_SIZE, GroupLayout.PREFERRED_SIZE)
                    .addPreferredGap(LayoutStyle.ComponentPlacement.UNRELATED)
                    .addComponent(jPanel2, GroupLayout.DEFAULT_SIZE, GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                    .addContainerGap())
        );
    }// </editor-fold>//GEN-END:initComponents

    // Variables declaration - do not modify//GEN-BEGIN:variables
    // Generated using JFormDesigner non-commercial license
    protected JPanel jPanelExtra;
    protected JTextField jTextFieldZiel;
    protected JTextField jTextFieldName;
    protected JButton jButtonZiel;
    protected JTextArea jTextArea1;
    protected JList<String> jListPset;
    // End of variables declaration//GEN-END:variables
}
