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
import java.net.URISyntaxException;
import java.nio.file.Path;
import javax.swing.JFrame;
import javax.swing.Timer;
import mSearch.Const;
import mSearch.tool.Functions;
import mediathek.config.Daten;
import mediathek.config.Konstanten;
import mediathek.gui.PanelVorlage;
import mediathek.tool.MVFunctionSys;
import mediathek.tool.UrlHyperlinkAction;

public class PanelProgrammInfos extends PanelVorlage {

    private static final long serialVersionUID = 1L;
    
    public PanelProgrammInfos(Daten dd, JFrame parentComponent) {
        super(dd, parentComponent);
        initComponents();

        try {
            lblProgrammversion.setText(MVFunctionSys.getProgVersionString() + " vom: " + Functions.getCompileDate());
        } catch (Exception ignored) {
            lblProgrammversion.setText(Const.VERSION);
        }

        lblProgramPath.setText(MVFunctionSys.getPathJar());
        try {
            final Path xmlFilePath = Daten.getMediathekXmlFilePath();
            lblSettingsPath.setText(xmlFilePath.toAbsolutePath().toString());
            lblFilmlistpath.setText(Daten.getDateiFilmliste());
            jXHyperlinkWebsite.setText(Konstanten.ADRESSE_WEBSITE);
            jXHyperlinkWebsite.addActionListener(new UrlHyperlinkAction(parentComponent, Konstanten.ADRESSE_WEBSITE));
            jXHyperlinkAnleitung.setText(Konstanten.ADRESSE_ANLEITUNG);
            jXHyperlinkAnleitung.addActionListener(new UrlHyperlinkAction(parentComponent, Konstanten.ADRESSE_ANLEITUNG));
            jXHyperlinkForum.setText(Konstanten.ADRESSE_FORUM);
            jXHyperlinkForum.addActionListener(new UrlHyperlinkAction(parentComponent, Konstanten.ADRESSE_FORUM));
        } catch (URISyntaxException ignored) {
        }

        updateMemory();

        Timer memUpdateTimer = new Timer(1000, new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                updateMemory();
            }
        });
        memUpdateTimer.setRepeats(true);
        memUpdateTimer.start();
    }

    private void updateMemory() {
        final Runtime rt = Runtime.getRuntime();
        final long totalMem = rt.totalMemory() / (1000L * 1000L);
        final long maxMem = rt.maxMemory() / (1000L * 1000L);
        final long freeMem = rt.freeMemory() / (1000L * 1000L);

        lblVmMemory.setText(totalMem + " MB");
        lblMaxVmMemory.setText(maxMem + " MB");
        lblFreeMemory.setText(freeMem + " MB");
    }

    // <editor-fold defaultstate="collapsed" desc="Generated Code">//GEN-BEGIN:initComponents
    private void initComponents() {

        javax.swing.JLabel jLabel3 = new javax.swing.JLabel();
        javax.swing.JTextField jTextField1 = new javax.swing.JTextField();
        javax.swing.JPanel jPanel1 = new javax.swing.JPanel();
        javax.swing.JPanel jPanel2 = new javax.swing.JPanel();
        javax.swing.JLabel jLabelSpeicher1 = new javax.swing.JLabel();
        javax.swing.JLabel jLabelSpeicher2 = new javax.swing.JLabel();
        javax.swing.JLabel jLabelSpeicher3 = new javax.swing.JLabel();
        lblVmMemory = new javax.swing.JLabel();
        lblMaxVmMemory = new javax.swing.JLabel();
        lblFreeMemory = new javax.swing.JLabel();
        javax.swing.JPanel jPanel4 = new javax.swing.JPanel();
        javax.swing.JLabel jLabel5 = new javax.swing.JLabel();
        javax.swing.JLabel jLabel7 = new javax.swing.JLabel();
        javax.swing.JLabel jLabel9 = new javax.swing.JLabel();
        jXHyperlinkWebsite = new org.jdesktop.swingx.JXHyperlink();
        jXHyperlinkAnleitung = new org.jdesktop.swingx.JXHyperlink();
        jXHyperlinkForum = new org.jdesktop.swingx.JXHyperlink();
        javax.swing.JPanel jPanel3 = new javax.swing.JPanel();
        javax.swing.JLabel jLabel1 = new javax.swing.JLabel();
        javax.swing.JLabel jLabel6 = new javax.swing.JLabel();
        javax.swing.JLabel jLabel2 = new javax.swing.JLabel();
        javax.swing.JLabel jLabel4 = new javax.swing.JLabel();
        lblProgrammversion = new javax.swing.JLabel();
        lblFilmlistpath = new javax.swing.JLabel();
        lblProgramPath = new javax.swing.JLabel();
        lblSettingsPath = new javax.swing.JLabel();

        jLabel3.setIcon(new javax.swing.ImageIcon(getClass().getResource("/mediathek/res/MediathekView_k.png"))); // NOI18N

        jTextField1.setEditable(false);
        jTextField1.setBackground(new java.awt.Color(204, 204, 255));
        jTextField1.setFont(new java.awt.Font("Dialog", 1, 14)); // NOI18N
        jTextField1.setHorizontalAlignment(javax.swing.JTextField.CENTER);
        jTextField1.setText("MediathekView");

        jPanel2.setBorder(javax.swing.BorderFactory.createLineBorder(new java.awt.Color(0, 0, 0)));

        jLabelSpeicher1.setFont(jLabelSpeicher1.getFont().deriveFont(jLabelSpeicher1.getFont().getStyle() | java.awt.Font.BOLD));
        jLabelSpeicher1.setText("Speicher der VM:");

        jLabelSpeicher2.setFont(jLabelSpeicher2.getFont().deriveFont(jLabelSpeicher2.getFont().getStyle() | java.awt.Font.BOLD));
        jLabelSpeicher2.setText("Max. Speicher der VM:");

        jLabelSpeicher3.setFont(jLabelSpeicher3.getFont().deriveFont(jLabelSpeicher3.getFont().getStyle() | java.awt.Font.BOLD));
        jLabelSpeicher3.setText("Freier Speicher:");

        javax.swing.GroupLayout jPanel2Layout = new javax.swing.GroupLayout(jPanel2);
        jPanel2.setLayout(jPanel2Layout);
        jPanel2Layout.setHorizontalGroup(
            jPanel2Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel2Layout.createSequentialGroup()
                .addContainerGap()
                .addGroup(jPanel2Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.TRAILING)
                    .addComponent(jLabelSpeicher1)
                    .addComponent(jLabelSpeicher2)
                    .addComponent(jLabelSpeicher3))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addGroup(jPanel2Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addComponent(lblVmMemory, javax.swing.GroupLayout.DEFAULT_SIZE, 507, Short.MAX_VALUE)
                    .addComponent(lblMaxVmMemory, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                    .addComponent(lblFreeMemory, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
                .addContainerGap())
        );
        jPanel2Layout.setVerticalGroup(
            jPanel2Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel2Layout.createSequentialGroup()
                .addContainerGap()
                .addGroup(jPanel2Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(jLabelSpeicher1)
                    .addComponent(lblVmMemory))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addGroup(jPanel2Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(jLabelSpeicher2)
                    .addComponent(lblMaxVmMemory))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addGroup(jPanel2Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(jLabelSpeicher3)
                    .addComponent(lblFreeMemory))
                .addContainerGap(javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
        );

        jPanel4.setBorder(javax.swing.BorderFactory.createLineBorder(new java.awt.Color(0, 0, 0)));

        jLabel5.setFont(jLabel5.getFont().deriveFont(jLabel5.getFont().getStyle() | java.awt.Font.BOLD));
        jLabel5.setText("Website:");

        jLabel7.setFont(jLabel7.getFont().deriveFont(jLabel7.getFont().getStyle() | java.awt.Font.BOLD));
        jLabel7.setText("Anleitung:");

        jLabel9.setFont(jLabel9.getFont().deriveFont(jLabel9.getFont().getStyle() | java.awt.Font.BOLD));
        jLabel9.setText("Forum:");

        jXHyperlinkWebsite.setText("http://zdfmediathk.sourceforge.net/");

        jXHyperlinkAnleitung.setText("http://sourceforge.net/p/zdfmediathk/wiki/Home/");

        jXHyperlinkForum.setText("http://zdfmediathk.sourceforge.net/forum/");

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
                .addGroup(jPanel4Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addComponent(jXHyperlinkWebsite, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                    .addComponent(jXHyperlinkAnleitung, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                    .addComponent(jXHyperlinkForum, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE))
                .addContainerGap(253, Short.MAX_VALUE))
        );
        jPanel4Layout.setVerticalGroup(
            jPanel4Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(javax.swing.GroupLayout.Alignment.TRAILING, jPanel4Layout.createSequentialGroup()
                .addContainerGap()
                .addGroup(jPanel4Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(jLabel5)
                    .addComponent(jXHyperlinkWebsite, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                .addGroup(jPanel4Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(jLabel9)
                    .addComponent(jXHyperlinkForum, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addGroup(jPanel4Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(jLabel7)
                    .addComponent(jXHyperlinkAnleitung, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE))
                .addContainerGap(javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
        );

        jPanel3.setBorder(javax.swing.BorderFactory.createLineBorder(new java.awt.Color(0, 0, 0)));

        jLabel1.setFont(jLabel1.getFont().deriveFont(jLabel1.getFont().getStyle() | java.awt.Font.BOLD));
        jLabel1.setText("Programmversion:");

        jLabel6.setFont(jLabel6.getFont().deriveFont(jLabel6.getFont().getStyle() | java.awt.Font.BOLD));
        jLabel6.setText("Filmeliste:");

        jLabel2.setFont(jLabel2.getFont().deriveFont(jLabel2.getFont().getStyle() | java.awt.Font.BOLD));
        jLabel2.setText("Programmpfad:");

        jLabel4.setFont(jLabel4.getFont().deriveFont(jLabel4.getFont().getStyle() | java.awt.Font.BOLD));
        jLabel4.setText("Einstellungen:");

        javax.swing.GroupLayout jPanel3Layout = new javax.swing.GroupLayout(jPanel3);
        jPanel3.setLayout(jPanel3Layout);
        jPanel3Layout.setHorizontalGroup(
            jPanel3Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel3Layout.createSequentialGroup()
                .addContainerGap()
                .addGroup(jPanel3Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.TRAILING)
                    .addComponent(jLabel6)
                    .addComponent(jLabel2)
                    .addComponent(jLabel4)
                    .addComponent(jLabel1))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addGroup(jPanel3Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addComponent(lblProgrammversion, javax.swing.GroupLayout.Alignment.TRAILING, javax.swing.GroupLayout.DEFAULT_SIZE, 531, Short.MAX_VALUE)
                    .addComponent(lblFilmlistpath, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                    .addComponent(lblProgramPath, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                    .addComponent(lblSettingsPath, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
                .addContainerGap())
        );
        jPanel3Layout.setVerticalGroup(
            jPanel3Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel3Layout.createSequentialGroup()
                .addContainerGap()
                .addGroup(jPanel3Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(jLabel1)
                    .addComponent(lblProgrammversion))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addGroup(jPanel3Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(jLabel6)
                    .addComponent(lblFilmlistpath))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addGroup(jPanel3Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(jLabel2)
                    .addComponent(lblProgramPath))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addGroup(jPanel3Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(jLabel4)
                    .addComponent(lblSettingsPath))
                .addContainerGap(javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
        );

        javax.swing.GroupLayout jPanel1Layout = new javax.swing.GroupLayout(jPanel1);
        jPanel1.setLayout(jPanel1Layout);
        jPanel1Layout.setHorizontalGroup(
            jPanel1Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel1Layout.createSequentialGroup()
                .addContainerGap()
                .addGroup(jPanel1Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addComponent(jPanel3, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                    .addComponent(jPanel2, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                    .addComponent(jPanel4, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
                .addContainerGap())
        );
        jPanel1Layout.setVerticalGroup(
            jPanel1Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel1Layout.createSequentialGroup()
                .addContainerGap()
                .addComponent(jPanel3, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(jPanel2, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(jPanel4, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addContainerGap(javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
        );

        javax.swing.GroupLayout layout = new javax.swing.GroupLayout(this);
        this.setLayout(layout);
        layout.setHorizontalGroup(
            layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(layout.createSequentialGroup()
                .addContainerGap()
                .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addGroup(javax.swing.GroupLayout.Alignment.TRAILING, layout.createSequentialGroup()
                        .addComponent(jLabel3)
                        .addGap(4, 4, 4)
                        .addComponent(jPanel1, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
                    .addComponent(jTextField1, javax.swing.GroupLayout.DEFAULT_SIZE, 756, Short.MAX_VALUE))
                .addContainerGap())
        );
        layout.setVerticalGroup(
            layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(layout.createSequentialGroup()
                .addContainerGap()
                .addComponent(jTextField1, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addGroup(layout.createSequentialGroup()
                        .addComponent(jLabel3)
                        .addGap(0, 0, Short.MAX_VALUE))
                    .addComponent(jPanel1, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
                .addContainerGap())
        );
    }// </editor-fold>//GEN-END:initComponents
    // Variables declaration - do not modify//GEN-BEGIN:variables
    private org.jdesktop.swingx.JXHyperlink jXHyperlinkAnleitung;
    private org.jdesktop.swingx.JXHyperlink jXHyperlinkForum;
    private org.jdesktop.swingx.JXHyperlink jXHyperlinkWebsite;
    private javax.swing.JLabel lblFilmlistpath;
    private javax.swing.JLabel lblFreeMemory;
    private javax.swing.JLabel lblMaxVmMemory;
    private javax.swing.JLabel lblProgramPath;
    private javax.swing.JLabel lblProgrammversion;
    private javax.swing.JLabel lblSettingsPath;
    private javax.swing.JLabel lblVmMemory;
    // End of variables declaration//GEN-END:variables
}
