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
package mediathek.gui.dialog;

import java.util.Date;
import mediathek.Main;
import mediathek.daten.DDaten;
import mediathek.gui.PanelVorlage;
import mediathek.tool.BeobWeb;
import mediathek.tool.Funktionen;
import mediathek.tool.Konstanten;

public class PanelAbout extends PanelVorlage {

    /**
     * Creates new form GuiFeed
     *
     * @param d
     */
    public PanelAbout(DDaten dd) {
        super(dd);
        initComponents();
        //init
        try {
            Date d = new Date(Main.class.getResource("Main.class").openConnection().getLastModified());
            jTextFieldVersion.setText(Funktionen.getProgVersionString() + " vom: " + Funktionen.getCompileDate());
        } catch (Exception e) {
            jTextFieldVersion.setText(Konstanten.VERSION);
        }
        jTextFieldPfad.setText(Funktionen.getPathJar());
        jButtonWebsite.addActionListener(new BeobWeb(jTextFieldWebsite.getText()));
        jButtonAnleitung.addActionListener(new BeobWeb(jTextFieldAnleitung.getText()));
        jButtonForum.addActionListener(new BeobWeb(jTextFieldForum.getText()));
    }
    // <editor-fold defaultstate="collapsed" desc="Generated Code">//GEN-BEGIN:initComponents
    private void initComponents() {

        jLabel1 = new javax.swing.JLabel();
        jTextFieldVersion = new javax.swing.JTextField();
        jPanel4 = new javax.swing.JPanel();
        jLabel5 = new javax.swing.JLabel();
        jButtonWebsite = new javax.swing.JButton();
        jLabel7 = new javax.swing.JLabel();
        jButtonAnleitung = new javax.swing.JButton();
        jLabel9 = new javax.swing.JLabel();
        jButtonForum = new javax.swing.JButton();
        jTextFieldWebsite = new javax.swing.JTextField();
        jTextFieldAnleitung = new javax.swing.JTextField();
        jTextFieldForum = new javax.swing.JTextField();
        jLabel2 = new javax.swing.JLabel();
        jTextFieldPfad = new javax.swing.JTextField();

        jLabel1.setText("Programmversion:");

        jTextFieldVersion.setEditable(false);
        jTextFieldVersion.setBorder(null);

        jPanel4.setBorder(javax.swing.BorderFactory.createLineBorder(new java.awt.Color(0, 0, 0)));

        jLabel5.setText("Website:");

        jButtonWebsite.setText("Browser");

        jLabel7.setText("Anleitung:");

        jButtonAnleitung.setText("Browser");
        jButtonAnleitung.setToolTipText("http://zdfmediathk.sourceforge.net/");

        jLabel9.setText("Forum:");

        jButtonForum.setText("Browser");
        jButtonForum.setToolTipText("http://zdfmediathk.sourceforge.net/");

        jTextFieldWebsite.setEditable(false);
        jTextFieldWebsite.setFont(new java.awt.Font("Dialog", 1, 12)); // NOI18N
        jTextFieldWebsite.setForeground(new java.awt.Color(0, 51, 204));
        jTextFieldWebsite.setText("http://zdfmediathk.sourceforge.net/");

        jTextFieldAnleitung.setEditable(false);
        jTextFieldAnleitung.setFont(new java.awt.Font("Dialog", 1, 12)); // NOI18N
        jTextFieldAnleitung.setForeground(new java.awt.Color(0, 51, 204));
        jTextFieldAnleitung.setText("https://sourceforge.net/p/zdfmediathk/wiki/Home/");

        jTextFieldForum.setEditable(false);
        jTextFieldForum.setFont(new java.awt.Font("Dialog", 1, 12)); // NOI18N
        jTextFieldForum.setForeground(new java.awt.Color(0, 51, 204));
        jTextFieldForum.setText("http://sourceforge.net/apps/phpbb/zdfmediathk/");

        javax.swing.GroupLayout jPanel4Layout = new javax.swing.GroupLayout(jPanel4);
        jPanel4.setLayout(jPanel4Layout);
        jPanel4Layout.setHorizontalGroup(
            jPanel4Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel4Layout.createSequentialGroup()
                .addContainerGap()
                .addGroup(jPanel4Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addComponent(jLabel5)
                    .addComponent(jLabel7)
                    .addComponent(jLabel9))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addGroup(jPanel4Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.TRAILING)
                    .addComponent(jTextFieldWebsite, javax.swing.GroupLayout.Alignment.LEADING)
                    .addComponent(jTextFieldAnleitung, javax.swing.GroupLayout.Alignment.LEADING)
                    .addComponent(jTextFieldForum, javax.swing.GroupLayout.Alignment.LEADING, javax.swing.GroupLayout.PREFERRED_SIZE, 1, Short.MAX_VALUE))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED, 12, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addGroup(jPanel4Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addComponent(jButtonWebsite, javax.swing.GroupLayout.Alignment.TRAILING)
                    .addComponent(jButtonAnleitung, javax.swing.GroupLayout.Alignment.TRAILING)
                    .addComponent(jButtonForum, javax.swing.GroupLayout.Alignment.TRAILING))
                .addContainerGap())
        );
        jPanel4Layout.setVerticalGroup(
            jPanel4Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(javax.swing.GroupLayout.Alignment.TRAILING, jPanel4Layout.createSequentialGroup()
                .addContainerGap()
                .addGroup(jPanel4Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(jLabel5)
                    .addComponent(jTextFieldWebsite, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                    .addComponent(jButtonWebsite))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                .addGroup(jPanel4Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(jTextFieldAnleitung, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                    .addComponent(jLabel7)
                    .addComponent(jButtonAnleitung))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                .addGroup(jPanel4Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(jTextFieldForum, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                    .addComponent(jLabel9)
                    .addComponent(jButtonForum))
                .addContainerGap(javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
        );

        jPanel4Layout.linkSize(javax.swing.SwingConstants.VERTICAL, new java.awt.Component[] {jButtonAnleitung, jButtonForum, jButtonWebsite, jTextFieldAnleitung, jTextFieldForum, jTextFieldWebsite});

        jLabel2.setText("Programmpfad:");

        jTextFieldPfad.setEditable(false);
        jTextFieldPfad.setBorder(null);

        javax.swing.GroupLayout layout = new javax.swing.GroupLayout(this);
        this.setLayout(layout);
        layout.setHorizontalGroup(
            layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(layout.createSequentialGroup()
                .addContainerGap()
                .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addComponent(jPanel4, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                    .addGroup(layout.createSequentialGroup()
                        .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                            .addComponent(jLabel1)
                            .addComponent(jLabel2))
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                        .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                            .addComponent(jTextFieldPfad, javax.swing.GroupLayout.PREFERRED_SIZE, 490, javax.swing.GroupLayout.PREFERRED_SIZE)
                            .addComponent(jTextFieldVersion, javax.swing.GroupLayout.PREFERRED_SIZE, 490, javax.swing.GroupLayout.PREFERRED_SIZE))
                        .addGap(0, 0, Short.MAX_VALUE)))
                .addContainerGap())
        );

        layout.linkSize(javax.swing.SwingConstants.HORIZONTAL, new java.awt.Component[] {jTextFieldPfad, jTextFieldVersion});

        layout.setVerticalGroup(
            layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(layout.createSequentialGroup()
                .addContainerGap()
                .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(jLabel1)
                    .addComponent(jTextFieldVersion, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(jLabel2)
                    .addComponent(jTextFieldPfad, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                .addComponent(jPanel4, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addContainerGap(javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
        );
    }// </editor-fold>//GEN-END:initComponents
    // Variables declaration - do not modify//GEN-BEGIN:variables
    private javax.swing.JButton jButtonAnleitung;
    private javax.swing.JButton jButtonForum;
    private javax.swing.JButton jButtonWebsite;
    private javax.swing.JLabel jLabel1;
    private javax.swing.JLabel jLabel2;
    private javax.swing.JLabel jLabel5;
    private javax.swing.JLabel jLabel7;
    private javax.swing.JLabel jLabel9;
    private javax.swing.JPanel jPanel4;
    private javax.swing.JTextField jTextFieldAnleitung;
    private javax.swing.JTextField jTextFieldForum;
    private javax.swing.JTextField jTextFieldPfad;
    private javax.swing.JTextField jTextFieldVersion;
    private javax.swing.JTextField jTextFieldWebsite;
    // End of variables declaration//GEN-END:variables
}
