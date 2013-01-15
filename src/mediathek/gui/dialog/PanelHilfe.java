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

import java.awt.Component;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.Date;
import mediathek.Main;
import mediathek.controller.io.ProgrammLog;
import mediathek.daten.DDaten;
import mediathek.gui.PanelVorlage;
import mediathek.tool.BeobWeb;
import mediathek.tool.Funktionen;
import mediathek.tool.Konstanten;

public class PanelHilfe extends PanelVorlage {


    /**
     * Creates new form GuiFeed
     *
     * @param d
     */
    public PanelHilfe(DDaten dd, Component parentComponent) {
        super(dd, parentComponent);
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
        jButtonLogErstellen.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                ProgrammLog.LogDateiSchreiben(ddaten);
            }
        });
    }
    // <editor-fold defaultstate="collapsed" desc="Generated Code">//GEN-BEGIN:initComponents
    private void initComponents() {

        jLabel1 = new javax.swing.JLabel();
        jTextFieldVersion = new javax.swing.JTextField();
        jScrollPane1 = new javax.swing.JScrollPane();
        jTextArea1 = new javax.swing.JTextArea();
        jTextField1 = new javax.swing.JTextField();
        jButtonLogErstellen = new javax.swing.JButton();
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

        jTextArea1.setEditable(false);
        jTextArea1.setColumns(20);
        jTextArea1.setRows(5);
        jTextArea1.setText("Bei Problemen sollten die Anleitung oder die FAQ \ndie erste Anlaufstelle sein. Führt das zu keiner Lösung\nkann auch eine Suche im Forum weiterhelfen.\n\nWenn auch das nicht weiterhilft, sollte man eine Anfrage im Forum\nstellen. Damit die Frage auch beantwortet werden kann, sind\nein paar Infos wichtig:\n * Möglichst genaue Beschreibung des Problems (was geht nicht,\n\twelcher Film, ..)\n * Infos über das Betriebssystem und die Programmversion\n\tODER BESSER:\n\tdas generierte Logfile an den Post im Forum anhängen\n");
        jScrollPane1.setViewportView(jTextArea1);

        jTextField1.setEditable(false);
        jTextField1.setBackground(new java.awt.Color(204, 204, 255));
        jTextField1.setFont(new java.awt.Font("Dialog", 1, 14)); // NOI18N
        jTextField1.setHorizontalAlignment(javax.swing.JTextField.CENTER);
        jTextField1.setText("Hilfe zum Programm finden");

        jButtonLogErstellen.setText("Logfile erstellen");

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
                    .addComponent(jScrollPane1)
                    .addComponent(jTextField1)
                    .addComponent(jPanel4, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                    .addGroup(layout.createSequentialGroup()
                        .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                            .addComponent(jButtonLogErstellen)
                            .addGroup(layout.createSequentialGroup()
                                .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                                    .addComponent(jLabel1)
                                    .addComponent(jLabel2))
                                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                                .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                                    .addComponent(jTextFieldPfad, javax.swing.GroupLayout.PREFERRED_SIZE, 490, javax.swing.GroupLayout.PREFERRED_SIZE)
                                    .addComponent(jTextFieldVersion, javax.swing.GroupLayout.PREFERRED_SIZE, 490, javax.swing.GroupLayout.PREFERRED_SIZE))))
                        .addGap(0, 0, Short.MAX_VALUE)))
                .addContainerGap())
        );

        layout.linkSize(javax.swing.SwingConstants.HORIZONTAL, new java.awt.Component[] {jTextFieldPfad, jTextFieldVersion});

        layout.setVerticalGroup(
            layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(layout.createSequentialGroup()
                .addContainerGap()
                .addComponent(jTextField1, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                .addComponent(jScrollPane1, javax.swing.GroupLayout.DEFAULT_SIZE, 274, Short.MAX_VALUE)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(jLabel1)
                    .addComponent(jTextFieldVersion, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(jLabel2)
                    .addComponent(jTextFieldPfad, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                .addComponent(jPanel4, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                .addComponent(jButtonLogErstellen)
                .addContainerGap())
        );
    }// </editor-fold>//GEN-END:initComponents
    // Variables declaration - do not modify//GEN-BEGIN:variables
    private javax.swing.JButton jButtonAnleitung;
    private javax.swing.JButton jButtonForum;
    private javax.swing.JButton jButtonLogErstellen;
    private javax.swing.JButton jButtonWebsite;
    private javax.swing.JLabel jLabel1;
    private javax.swing.JLabel jLabel2;
    private javax.swing.JLabel jLabel5;
    private javax.swing.JLabel jLabel7;
    private javax.swing.JLabel jLabel9;
    private javax.swing.JPanel jPanel4;
    private javax.swing.JScrollPane jScrollPane1;
    private javax.swing.JTextArea jTextArea1;
    private javax.swing.JTextField jTextField1;
    private javax.swing.JTextField jTextFieldAnleitung;
    private javax.swing.JTextField jTextFieldForum;
    private javax.swing.JTextField jTextFieldPfad;
    private javax.swing.JTextField jTextFieldVersion;
    private javax.swing.JTextField jTextFieldWebsite;
    // End of variables declaration//GEN-END:variables
}
