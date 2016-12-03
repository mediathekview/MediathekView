/*    
 *    MediathekView
 *    Copyright (C) 2012   W. Xaver
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

import mediathek.config.Daten;
import mediathek.config.MVConfig;
import mediathek.gui.PanelVorlage;

import javax.swing.*;
import java.awt.*;

@SuppressWarnings("serial")
public class PanelPset extends PanelVorlage {
    public PanelPset(Daten d, JFrame parentComponent) {
        super(d, parentComponent);
        initComponents();
        jCheckBoxAlleEinstellungen.addActionListener(e -> {
            MVConfig.add(MVConfig.Configs.SYSTEM_ANSICHT_SET_LANG, Boolean.toString(jCheckBoxAlleEinstellungen.isSelected()));
            pset();
        });
        jCheckBoxAlleEinstellungen.setSelected(Boolean.parseBoolean(MVConfig.get(MVConfig.Configs.SYSTEM_ANSICHT_SET_LANG)));
        pset();
    }

    private void pset() {
        // Einstellungen zum Ansehen und Speichern der Filme anpassen
        jPanelPset.removeAll();
        jPanelPset.setLayout(new BorderLayout());
        if (jCheckBoxAlleEinstellungen.isSelected()) {
            jPanelPset.add(new PanelPsetLang(daten, parentComponent, Daten.listePset), BorderLayout.CENTER);
        } else {
            jPanelPset.add(new PanelPsetKurz(daten, parentComponent, Daten.listePset), BorderLayout.CENTER);
        }
        jPanelPset.updateUI();
    }

    // <editor-fold defaultstate="collapsed" desc="Generated Code">//GEN-BEGIN:initComponents
    private void initComponents() {

        jPanelPset = new javax.swing.JPanel();
        jCheckBoxAlleEinstellungen = new javax.swing.JCheckBox();

        jPanelPset.setBorder(javax.swing.BorderFactory.createEtchedBorder());

        javax.swing.GroupLayout jPanelPsetLayout = new javax.swing.GroupLayout(jPanelPset);
        jPanelPset.setLayout(jPanelPsetLayout);
        jPanelPsetLayout.setHorizontalGroup(
                jPanelPsetLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                        .addGap(0, 466, Short.MAX_VALUE)
        );
        jPanelPsetLayout.setVerticalGroup(
                jPanelPsetLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                        .addGap(0, 308, Short.MAX_VALUE)
        );

        jCheckBoxAlleEinstellungen.setText("alle Einstellungen anzeigen");

        javax.swing.GroupLayout layout = new javax.swing.GroupLayout(this);
        this.setLayout(layout);
        layout.setHorizontalGroup(
                layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                        .addGroup(layout.createSequentialGroup()
                                .addContainerGap()
                                .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                                        .addComponent(jPanelPset, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                                        .addGroup(layout.createSequentialGroup()
                                                .addComponent(jCheckBoxAlleEinstellungen)
                                                .addGap(0, 248, Short.MAX_VALUE)))
                                .addContainerGap())
        );
        layout.setVerticalGroup(
                layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                        .addGroup(layout.createSequentialGroup()
                                .addContainerGap()
                                .addComponent(jCheckBoxAlleEinstellungen)
                                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                                .addComponent(jPanelPset, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                                .addContainerGap())
        );
    }// </editor-fold>//GEN-END:initComponents

    // Variables declaration - do not modify//GEN-BEGIN:variables
    private javax.swing.JCheckBox jCheckBoxAlleEinstellungen;
    private javax.swing.JPanel jPanelPset;
    // End of variables declaration//GEN-END:variables
}
