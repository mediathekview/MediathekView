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

import java.awt.Insets;
import mediathek.Daten;
import mediathek.Konstanten;
import mediathek.daten.DDaten;
import mediathek.gui.PanelVorlage;
import mediathek.gui.beobachter.BeobWeb;

public class PanelInfo extends PanelVorlage {

    /**
     * Creates new form GuiFeed
     *
     * @param d
     */
    public PanelInfo(DDaten d) {
        super(d);
        initComponents();
        ddaten = d;
        setText();
        //init
        jButtonAnleitung.addActionListener(new BeobWeb(Konstanten.ADRESSE_ANLEITUNG));
    }
    //===================================
    // Public
    //===================================

    @Override
    public void neuLaden() {
        setText();
    }
    //===================================
    // Private
    //===================================

    private void setText() {
        String abos = Daten.getBasisVerzeichnis(false) + Konstanten.LOG_DATEI_DOWNLOAD_ABOS;
        String einstellungen = Daten.getBasisVerzeichnis(false) + Konstanten.XML_DATEI;
        String filme = Daten.getBasisVerzeichnis(false) + Konstanten.XML_DATEI_FILME;
        //
        jTextFieldAbos.setMargin(new Insets(1, 5, 1, 5));
        jTextFieldEinstellungen.setMargin(new Insets(1, 5, 1, 5));
        jTextFieldFilme.setMargin(new Insets(1, 5, 1, 5));
        jTextFieldVersion.setMargin(new Insets(1, 5, 1, 5));
        //
        jTextFieldAbos.setText(abos);
        jTextFieldEinstellungen.setText(einstellungen);
        jTextFieldFilme.setText(filme);
        jTextFieldVersion.setText(Konstanten.VERSION);
    }
    // <editor-fold defaultstate="collapsed" desc="Generated Code">//GEN-BEGIN:initComponents
    private void initComponents() {

        jLabel1 = new javax.swing.JLabel();
        jLabel2 = new javax.swing.JLabel();
        jLabel3 = new javax.swing.JLabel();
        jTextFieldAbos = new javax.swing.JTextField();
        jTextFieldEinstellungen = new javax.swing.JTextField();
        jTextFieldVersion = new javax.swing.JTextField();
        jButtonAnleitung = new javax.swing.JButton();
        jLabel5 = new javax.swing.JLabel();
        jTextFieldFilme = new javax.swing.JTextField();

        jLabel1.setText("Version:");

        jLabel2.setText("Pfad Einstellungen:");

        jLabel3.setText("Pfad Protokoll Abos:");

        jTextFieldAbos.setEditable(false);

        jTextFieldEinstellungen.setEditable(false);

        jTextFieldVersion.setEditable(false);

        jButtonAnleitung.setText("Homepage / Anleitung");
        jButtonAnleitung.setToolTipText("http://zdfmediathk.sourceforge.net/");

        jLabel5.setText("Pfad Filme:");

        jTextFieldFilme.setEditable(false);

        javax.swing.GroupLayout layout = new javax.swing.GroupLayout(this);
        this.setLayout(layout);
        layout.setHorizontalGroup(
            layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(layout.createSequentialGroup()
                .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addGroup(layout.createSequentialGroup()
                        .addContainerGap()
                        .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                            .addComponent(jLabel3)
                            .addComponent(jLabel2)
                            .addComponent(jLabel1)
                            .addComponent(jLabel5))
                        .addGap(48, 48, 48)
                        .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                            .addComponent(jTextFieldEinstellungen, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                            .addComponent(jTextFieldVersion, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                            .addComponent(jTextFieldAbos, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                            .addComponent(jTextFieldFilme, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)))
                    .addGroup(layout.createSequentialGroup()
                        .addGap(12, 12, 12)
                        .addComponent(jButtonAnleitung)))
                .addContainerGap(114, Short.MAX_VALUE))
        );

        layout.linkSize(javax.swing.SwingConstants.HORIZONTAL, new java.awt.Component[] {jTextFieldAbos, jTextFieldEinstellungen, jTextFieldFilme, jTextFieldVersion});

        layout.setVerticalGroup(
            layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(layout.createSequentialGroup()
                .addContainerGap()
                .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(jLabel1)
                    .addComponent(jTextFieldVersion, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE))
                .addGap(18, 18, 18)
                .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(jLabel2)
                    .addComponent(jTextFieldEinstellungen, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(jLabel5)
                    .addComponent(jTextFieldFilme, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(jLabel3)
                    .addComponent(jTextFieldAbos, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED, 71, Short.MAX_VALUE)
                .addComponent(jButtonAnleitung)
                .addContainerGap())
        );

        layout.linkSize(javax.swing.SwingConstants.VERTICAL, new java.awt.Component[] {jTextFieldEinstellungen, jTextFieldFilme});

    }// </editor-fold>//GEN-END:initComponents
    // Variables declaration - do not modify//GEN-BEGIN:variables
    private javax.swing.JButton jButtonAnleitung;
    private javax.swing.JLabel jLabel1;
    private javax.swing.JLabel jLabel2;
    private javax.swing.JLabel jLabel3;
    private javax.swing.JLabel jLabel5;
    private javax.swing.JTextField jTextFieldAbos;
    private javax.swing.JTextField jTextFieldEinstellungen;
    private javax.swing.JTextField jTextFieldFilme;
    private javax.swing.JTextField jTextFieldVersion;
    // End of variables declaration//GEN-END:variables
}
