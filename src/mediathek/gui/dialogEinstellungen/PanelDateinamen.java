/*    
 *    MediathekView
 *    Copyright (C) 2008   W. Xaver
 *    W.Xaver[at]googlemail.com
 *    http://zdfmediathk.sourceforge.net/
 *    
 *    This program is free software: you can redistribute it and/or modify
 *    it under the terms of the GNU General Public License as published by
 *    the Free Software Foundation, either version 3 of the License, or
 *    any later version.
 *
 *    This program is distributed in the hope that it will be useful,
 *    but WITHOUT ANY WARRANTY; without even the implied warranty of
 *    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *    GNU General Public License for more details.
 *
 *    You should have received a copy of the GNU General Public License
 *    along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */
package mediathek.gui.dialogEinstellungen;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import javax.swing.JFrame;
import mediathek.daten.Daten;
import mediathek.file.GetFile;
import mediathek.gui.PanelVorlage;
import mediathek.gui.dialog.DialogHilfe;
import mediathek.res.GetIcon;
import mediathek.tool.Konstanten;
import mediathek.tool.MVConfig;

public class PanelDateinamen extends PanelVorlage {

    public PanelDateinamen(Daten d, JFrame pparentComponent) {
        super(d, pparentComponent);
        initComponents();
        daten = d;
        jButtonHilfeZielname.setIcon(GetIcon.getProgramIcon("help_16.png"));
        jButtonHilfeZielname.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                new DialogHilfe(parentComponent, true, new GetFile().getHilfeSuchen(GetFile.PFAD_HILFETEXT_UNICODE)).setVisible(true);
            }
        });
        // ============================
        // Zeildateiname
        switch (Daten.mVConfig.get(MVConfig.SYSTEM_ZIELNAMEN_ANPASSEN)) {
            case Konstanten.ZIELNAMEN_ANPASSEN_NORMAL:
                jRadioButtonNamenNormal.setSelected(true);
                break;
            case Konstanten.ZIELNAMEN_ANPASSEN_ASCII:
                jRadioButtonNameAscii.setSelected(true);
                break;
            case Konstanten.ZIELNAMEN_ANPASSEN_NIX:
                jRadioButtonNameNix.setSelected(true);
                break;
            default:
                // nach einem Versionswechsel
                jRadioButtonNamenNormal.setSelected(true);
                Daten.mVConfig.add(MVConfig.SYSTEM_ZIELNAMEN_ANPASSEN, Konstanten.ZIELNAMEN_ANPASSEN_NORMAL);
                Daten.mVConfig.add(MVConfig.SYSTEM_ZIELNAMEN_UNICODE, Boolean.TRUE.toString());
        }
        jButtonBearbeiten.setEnabled(jRadioButtonNamenNormal.isSelected() || jRadioButtonNameAscii.isSelected());
        jCheckBoxUnicode.setEnabled(jRadioButtonNamenNormal.isSelected() || jRadioButtonNameAscii.isSelected());
        jRadioButtonNamenNormal.addActionListener(new ActionListener() {

            @Override
            public void actionPerformed(ActionEvent e) {
                Daten.mVConfig.add(MVConfig.SYSTEM_ZIELNAMEN_ANPASSEN, Konstanten.ZIELNAMEN_ANPASSEN_NORMAL);
                jButtonBearbeiten.setEnabled(jRadioButtonNamenNormal.isSelected() || jRadioButtonNameAscii.isSelected());
                jCheckBoxUnicode.setEnabled(jRadioButtonNamenNormal.isSelected() || jRadioButtonNameAscii.isSelected());
            }
        });
        jRadioButtonNameAscii.addActionListener(new ActionListener() {

            @Override
            public void actionPerformed(ActionEvent e) {
                Daten.mVConfig.add(MVConfig.SYSTEM_ZIELNAMEN_ANPASSEN, Konstanten.ZIELNAMEN_ANPASSEN_ASCII);
                jButtonBearbeiten.setEnabled(jRadioButtonNamenNormal.isSelected() || jRadioButtonNameAscii.isSelected());
                jCheckBoxUnicode.setEnabled(jRadioButtonNamenNormal.isSelected() || jRadioButtonNameAscii.isSelected());
            }
        });
        jRadioButtonNameNix.addActionListener(new ActionListener() {

            @Override
            public void actionPerformed(ActionEvent e) {
                Daten.mVConfig.add(MVConfig.SYSTEM_ZIELNAMEN_ANPASSEN, Konstanten.ZIELNAMEN_ANPASSEN_NIX);
                jButtonBearbeiten.setEnabled(jRadioButtonNamenNormal.isSelected() || jRadioButtonNameAscii.isSelected());
                jCheckBoxUnicode.setEnabled(jRadioButtonNamenNormal.isSelected() || jRadioButtonNameAscii.isSelected());
            }
        });
        jCheckBoxUnicode.addActionListener(new ActionListener() {

            @Override
            public void actionPerformed(ActionEvent e) {
                Daten.mVConfig.add(MVConfig.SYSTEM_ZIELNAMEN_UNICODE, Boolean.toString(jCheckBoxUnicode.isSelected()));
            }
        });
        // ====================================
        jButtonBearbeiten.addActionListener(new ActionListener() {

            @Override
            public void actionPerformed(ActionEvent e) {
                new DialogErsetzungstabelle(parentComponent, true, daten).setVisible(true);
            }
        });
    }

    /** This method is called from within the constructor to
     * initialize the form.
     * WARNING: Do NOT modify this code. The content of this method is
     * always regenerated by the Form Editor.
     */
    // <editor-fold defaultstate="collapsed" desc="Generated Code">//GEN-BEGIN:initComponents
    private void initComponents() {

        javax.swing.ButtonGroup buttonGroup1 = new javax.swing.ButtonGroup();
        buttonGroup2 = new javax.swing.ButtonGroup();
        javax.swing.JPanel jPanel3 = new javax.swing.JPanel();
        jRadioButtonNamenNormal = new javax.swing.JRadioButton();
        jRadioButtonNameAscii = new javax.swing.JRadioButton();
        jCheckBoxUnicode = new javax.swing.JCheckBox();
        jButtonBearbeiten = new javax.swing.JButton();
        jPanel1 = new javax.swing.JPanel();
        jRadioButtonNameNix = new javax.swing.JRadioButton();
        jButtonHilfeZielname = new javax.swing.JButton();

        jPanel3.setBorder(javax.swing.BorderFactory.createTitledBorder("Datei- und Pfadnamen der gespeicherten Filme anpassen"));

        buttonGroup2.add(jRadioButtonNamenNormal);
        jRadioButtonNamenNormal.setText("einfache Prüfung");

        buttonGroup2.add(jRadioButtonNameAscii);
        jRadioButtonNameAscii.setText("nur ASCII-Zeichen erlauben");

        jCheckBoxUnicode.setText("Unicode-Zeichen \"vereinfachen\"");

        jButtonBearbeiten.setText("Ersetzungstabelle bearbeiten");

        javax.swing.GroupLayout jPanel3Layout = new javax.swing.GroupLayout(jPanel3);
        jPanel3.setLayout(jPanel3Layout);
        jPanel3Layout.setHorizontalGroup(
            jPanel3Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel3Layout.createSequentialGroup()
                .addContainerGap()
                .addGroup(jPanel3Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addGroup(jPanel3Layout.createSequentialGroup()
                        .addComponent(jRadioButtonNamenNormal)
                        .addGap(18, 18, 18)
                        .addComponent(jRadioButtonNameAscii))
                    .addComponent(jCheckBoxUnicode)
                    .addComponent(jButtonBearbeiten))
                .addContainerGap(javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
        );
        jPanel3Layout.setVerticalGroup(
            jPanel3Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(javax.swing.GroupLayout.Alignment.TRAILING, jPanel3Layout.createSequentialGroup()
                .addContainerGap()
                .addGroup(jPanel3Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(jRadioButtonNamenNormal)
                    .addComponent(jRadioButtonNameAscii))
                .addGap(18, 18, 18)
                .addComponent(jCheckBoxUnicode)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                .addComponent(jButtonBearbeiten)
                .addContainerGap(javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
        );

        jPanel1.setBorder(javax.swing.BorderFactory.createTitledBorder("Nichts verändern"));

        buttonGroup2.add(jRadioButtonNameNix);
        jRadioButtonNameNix.setForeground(new java.awt.Color(153, 0, 51));
        jRadioButtonNameNix.setText("Keine Veränderung vornehmen (dann selbst prüfen!)");

        javax.swing.GroupLayout jPanel1Layout = new javax.swing.GroupLayout(jPanel1);
        jPanel1.setLayout(jPanel1Layout);
        jPanel1Layout.setHorizontalGroup(
            jPanel1Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel1Layout.createSequentialGroup()
                .addContainerGap()
                .addComponent(jRadioButtonNameNix)
                .addContainerGap(javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
        );
        jPanel1Layout.setVerticalGroup(
            jPanel1Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel1Layout.createSequentialGroup()
                .addContainerGap()
                .addComponent(jRadioButtonNameNix)
                .addContainerGap(11, Short.MAX_VALUE))
        );

        jButtonHilfeZielname.setIcon(new javax.swing.ImageIcon(getClass().getResource("/mediathek/res/programm/help_16.png"))); // NOI18N

        javax.swing.GroupLayout layout = new javax.swing.GroupLayout(this);
        this.setLayout(layout);
        layout.setHorizontalGroup(
            layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(layout.createSequentialGroup()
                .addContainerGap()
                .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addComponent(jPanel3, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                    .addComponent(jPanel1, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                    .addGroup(javax.swing.GroupLayout.Alignment.TRAILING, layout.createSequentialGroup()
                        .addGap(0, 0, Short.MAX_VALUE)
                        .addComponent(jButtonHilfeZielname)))
                .addContainerGap())
        );
        layout.setVerticalGroup(
            layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(layout.createSequentialGroup()
                .addContainerGap()
                .addComponent(jPanel3, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(jPanel1, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                .addComponent(jButtonHilfeZielname)
                .addContainerGap(javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
        );
    }// </editor-fold>//GEN-END:initComponents
    // Variables declaration - do not modify//GEN-BEGIN:variables
    private javax.swing.ButtonGroup buttonGroup2;
    private javax.swing.JButton jButtonBearbeiten;
    private javax.swing.JButton jButtonHilfeZielname;
    private javax.swing.JCheckBox jCheckBoxUnicode;
    private javax.swing.JPanel jPanel1;
    private javax.swing.JRadioButton jRadioButtonNameAscii;
    private javax.swing.JRadioButton jRadioButtonNameNix;
    private javax.swing.JRadioButton jRadioButtonNamenNormal;
    // End of variables declaration//GEN-END:variables

}
