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
package mediathek.gui;

import mSearch.tool.Listener;
import mediathek.config.Daten;
import mediathek.config.Icons;
import mediathek.daten.ListePsetVorlagen;
import mediathek.file.GetFile;
import mediathek.gui.dialog.DialogHilfe;
import mediathek.tool.GuiFunktionenProgramme;

import javax.swing.*;

@SuppressWarnings("serial")
public class ResetSettingsPanel extends JPanel {
    private Daten daten;
    private JFrame parent;

    public ResetSettingsPanel(JFrame pparent, Daten ddaten) {
        initComponents();
        parent = pparent;
        daten = ddaten;

        jButtonHilfeReset.setIcon(Icons.ICON_BUTTON_HELP);
        jButtonHilfeReset.addActionListener(e -> new DialogHilfe(Daten.mediathekGui, true, new GetFile().getHilfeSuchen(GetFile.PFAD_HILFETEXT_RESET)).setVisible(true));
        jButtonResetSets.addActionListener(e -> {
            Daten.listePset.clear();
            //GuiFunktionenProgramme.addVorlagen(ddaten, GuiFunktionenProgramme.getStandardprogramme(ddaten), false /* auto */);
            GuiFunktionenProgramme.addSetVorlagen(parent, daten, ListePsetVorlagen.getStandarset(parent, daten, true /*replaceMuster*/), false /*auto*/, true /*setVersion*/);
            Listener.notify(Listener.EREIGNIS_LISTE_PSET, ResetSettingsPanel.class.getSimpleName());
        });
        jButtonResetAll.addActionListener(e -> {
            int ret = JOptionPane.showConfirmDialog(parent, "Alle Einstellungen zurücksetzen?", "Alle Einstellungen zurücksetzen!", JOptionPane.YES_NO_OPTION);
            if (ret == JOptionPane.OK_OPTION) {
                // damit wird vor dem Beenden das Konfig-Verzeichnis umbenannt und so startet das
                // Programm wie beim ersten Start
                Daten.RESET = true;
                Daten.mediathekGui.beenden(false, false);
            }
        });

    }

    // <editor-fold defaultstate="collapsed" desc="Generated Code">//GEN-BEGIN:initComponents
    private void initComponents() {

        javax.swing.JLabel jLabel1 = new javax.swing.JLabel();
        javax.swing.JPanel jPanel2 = new javax.swing.JPanel();
        jButtonResetSets = new javax.swing.JButton();
        jButtonHilfeReset = new javax.swing.JButton();
        javax.swing.JLabel jLabel7 = new javax.swing.JLabel();
        javax.swing.JSeparator jSeparator1 = new javax.swing.JSeparator();
        jButtonResetAll = new javax.swing.JButton();
        javax.swing.JLabel jLabel10 = new javax.swing.JLabel();

        jLabel1.setText("<html>Bei Problemen sollten die Anleitung oder die FAQ die erste Anlaufstelle sein. Führt das zu keiner Lösung, kann auch eine Suche im Forum weiterhelfen.<br><br>\nWenn auch das nicht weiterhilft, sollte man eine Anfrage im Forum stellen. Damit die Frage auch beantwortet \nwerden kann, sind ein paar Infos wichtig:<br>\n * Möglichst genaue Beschreibung des Problems (was geht nicht, welcher Film, ..)<br>\n * Infos über das Betriebssystem und die Programmversion<br>\n<b>ODER BESSER:</b><br>\nDas generierte Logfile an den Post im Forum anhängen</html>");

        jPanel2.setBorder(javax.swing.BorderFactory.createLineBorder(new java.awt.Color(102, 102, 102)));

        jButtonResetSets.setText("Einstellungen zum Abspielen/Aufzeichnen zurücksetzen");

        jButtonHilfeReset.setIcon(new javax.swing.ImageIcon(getClass().getResource("/mediathek/res/muster/button-help.png"))); // NOI18N
        jButtonHilfeReset.setToolTipText("Hilfe anzeigen");

        jLabel7.setText("<html>Es werden alle Programmsets (auch eigene) zum Abspielen und Aufzeichnen gelöscht und die Standardsets wieder angelegt.<br>Abos und Blacklist bleiben erhalten.</html>");

        jButtonResetAll.setText("Alle Einstellungen zurücksetzen!");

        jLabel10.setText("<html>Alle Einstellungen gehen verloren.<br><b>ACHTUNG</b>, es werden auch eigene Buttons, Abos und die Blacklist gelöscht.</html>");

        javax.swing.GroupLayout jPanel2Layout = new javax.swing.GroupLayout(jPanel2);
        jPanel2.setLayout(jPanel2Layout);
        jPanel2Layout.setHorizontalGroup(
            jPanel2Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel2Layout.createSequentialGroup()
                .addContainerGap()
                .addGroup(jPanel2Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                        .addComponent(jSeparator1, javax.swing.GroupLayout.Alignment.TRAILING)
                    .addGroup(jPanel2Layout.createSequentialGroup()
                            .addGroup(jPanel2Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                                    .addGroup(jPanel2Layout.createSequentialGroup()
                                            .addComponent(jButtonResetSets)
                                            .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                                            .addComponent(jButtonHilfeReset))
                                    .addComponent(jButtonResetAll))
                            .addGap(0, 0, Short.MAX_VALUE))
                    .addGroup(jPanel2Layout.createSequentialGroup()
                            .addGap(6, 6, 6)
                        .addGroup(jPanel2Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                                .addComponent(jLabel10, javax.swing.GroupLayout.DEFAULT_SIZE, 598, Short.MAX_VALUE)
                                .addComponent(jLabel7, javax.swing.GroupLayout.PREFERRED_SIZE, 0, Short.MAX_VALUE))))
                .addContainerGap())
        );
        jPanel2Layout.setVerticalGroup(
            jPanel2Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel2Layout.createSequentialGroup()
                .addContainerGap()
                    .addGroup(jPanel2Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addComponent(jButtonResetSets)
                    .addComponent(jButtonHilfeReset))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                    .addComponent(jLabel7, javax.swing.GroupLayout.PREFERRED_SIZE, 54, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                    .addComponent(jSeparator1, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(jButtonResetAll)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                    .addComponent(jLabel10, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addContainerGap(javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
        );

        javax.swing.GroupLayout layout = new javax.swing.GroupLayout(this);
        this.setLayout(layout);
        layout.setHorizontalGroup(
            layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(layout.createSequentialGroup()
                .addContainerGap()
                    .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                            .addComponent(jLabel1, javax.swing.GroupLayout.PREFERRED_SIZE, 0, Short.MAX_VALUE)
                            .addComponent(jPanel2, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
                .addContainerGap())
        );
        layout.setVerticalGroup(
            layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addGroup(layout.createSequentialGroup()
                .addContainerGap()
                            .addComponent(jLabel1, javax.swing.GroupLayout.PREFERRED_SIZE, 167, javax.swing.GroupLayout.PREFERRED_SIZE)
                            .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                            .addComponent(jPanel2, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                            .addContainerGap(javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
        );
    }// </editor-fold>//GEN-END:initComponents

    // Variables declaration - do not modify//GEN-BEGIN:variables
    private javax.swing.JButton jButtonHilfeReset;
    private javax.swing.JButton jButtonResetAll;
    private javax.swing.JButton jButtonResetSets;
    // End of variables declaration//GEN-END:variables
}
