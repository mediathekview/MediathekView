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
package mediathek.gui.dialog.reset;

import mSearch.tool.Listener;
import mediathek.MediathekGui;
import mediathek.config.Daten;
import mediathek.config.Icons;
import mediathek.daten.ListePsetVorlagen;
import mediathek.file.GetFile;
import mediathek.gui.dialog.DialogHilfe;
import mediathek.tool.GuiFunktionenProgramme;

import javax.swing.*;
import javax.swing.border.LineBorder;
import java.awt.*;

@SuppressWarnings("serial")
public class ResetSettingsPanel extends JPanel {
    private static final String RESET_MESSAGE = "<html>Es werden <b>ALLE</b> von Ihnen erzeugten Änderungen gelöscht.<br>" +
            "Möchten Sie wirklich alle Einstellungen zurücksetzen?<br></html>";

    public ResetSettingsPanel(JFrame parent, Daten daten) {
        initComponents();

        jButtonHilfeReset.setIcon(Icons.ICON_BUTTON_HELP);
        jButtonHilfeReset.addActionListener(e -> new DialogHilfe(parent, true, new GetFile().getHilfeSuchen(GetFile.PFAD_HILFETEXT_RESET)).setVisible(true));
        jButtonResetSets.addActionListener(e -> {
            Daten.listePset.clear();
            GuiFunktionenProgramme.addSetVorlagen(parent, daten, ListePsetVorlagen.getStandarset(parent, daten, true /*replaceMuster*/), false /*auto*/, true /*setVersion*/);
            Listener.notify(Listener.EREIGNIS_LISTE_PSET, ResetSettingsPanel.class.getSimpleName());
        });
        jButtonResetAll.addActionListener(e -> {
            int ret = JOptionPane.showConfirmDialog(parent, RESET_MESSAGE, "Einstellungen zurücksetzen", JOptionPane.YES_NO_OPTION);
            if (ret == JOptionPane.OK_OPTION) {
                // damit wird vor dem Beenden das Konfig-Verzeichnis umbenannt und so startet das
                // Programm wie beim ersten Start
                Daten.setReset(true);
                MediathekGui.ui().beenden(false, false);
            }
        });

    }

    // <editor-fold defaultstate="collapsed" desc="Generated Code">//GEN-BEGIN:initComponents
    // Generated using JFormDesigner non-commercial license
    private void initComponents() {
        var jLabel1 = new JLabel();
        var jPanel2 = new JPanel();
        jButtonResetSets = new JButton();
        jButtonHilfeReset = new JButton();
        var jLabel7 = new JLabel();
        var jSeparator1 = new JSeparator();
        jButtonResetAll = new JButton();
        var jLabel10 = new JLabel();

        //======== this ========

        //---- jLabel1 ----
        jLabel1.setText("<html>Bei Problemen sollten die Anleitung oder die FAQ die erste Anlaufstelle sein. F\u00fchrt das zu keiner L\u00f6sung, kann auch eine Suche im Forum weiterhelfen.<br><br>\nWenn auch das nicht weiterhilft, sollte man eine Anfrage im Forum stellen. Damit die Frage auch beantwortet \nwerden kann, sind ein paar Infos wichtig:<br>\n * M\u00f6glichst genaue Beschreibung des Problems (was geht nicht, welcher Film, ..)<br>\n * Infos \u00fcber das Betriebssystem und die Programmversion<br>\n<b>ODER BESSER:</b><br>\nDas generierte Logfile an den Post im Forum anh\u00e4ngen</html>");

        //======== jPanel2 ========
        {
            jPanel2.setBorder(new LineBorder(new Color(102, 102, 102)));

            //---- jButtonResetSets ----
            jButtonResetSets.setText("Einstellungen zum Abspielen/Aufzeichnen zur\u00fccksetzen");

            //---- jButtonHilfeReset ----
            jButtonHilfeReset.setIcon(new ImageIcon(getClass().getResource("/mediathek/res/muster/button-help.png")));
            jButtonHilfeReset.setToolTipText("Hilfe anzeigen");

            //---- jLabel7 ----
            jLabel7.setText("<html>Es werden alle Programmsets (auch eigene) zum Abspielen und Aufzeichnen gel\u00f6scht und die Standardsets wieder angelegt.<br>Abos und Blacklist bleiben erhalten.</html>");

            //---- jButtonResetAll ----
            jButtonResetAll.setText("Alle Einstellungen zur\u00fccksetzen!");

            //---- jLabel10 ----
            jLabel10.setText("<html>Alle Einstellungen gehen verloren.<br><b>ACHTUNG</b>, es werden auch eigene Buttons, Abos und die Blacklist gel\u00f6scht.</html>");

            GroupLayout jPanel2Layout = new GroupLayout(jPanel2);
            jPanel2.setLayout(jPanel2Layout);
            jPanel2Layout.setHorizontalGroup(
                jPanel2Layout.createParallelGroup()
                    .addGroup(jPanel2Layout.createSequentialGroup()
                        .addContainerGap()
                        .addGroup(jPanel2Layout.createParallelGroup()
                            .addComponent(jSeparator1, GroupLayout.Alignment.TRAILING)
                            .addGroup(jPanel2Layout.createSequentialGroup()
                                .addGroup(jPanel2Layout.createParallelGroup()
                                    .addGroup(jPanel2Layout.createSequentialGroup()
                                        .addComponent(jButtonResetSets)
                                        .addPreferredGap(LayoutStyle.ComponentPlacement.RELATED)
                                        .addComponent(jButtonHilfeReset))
                                    .addComponent(jButtonResetAll))
                                .addGap(0, 0, Short.MAX_VALUE))
                            .addGroup(jPanel2Layout.createSequentialGroup()
                                .addGap(6, 6, 6)
                                .addGroup(jPanel2Layout.createParallelGroup()
                                    .addComponent(jLabel10, GroupLayout.DEFAULT_SIZE, 578, Short.MAX_VALUE)
                                    .addComponent(jLabel7, GroupLayout.DEFAULT_SIZE, 0, Short.MAX_VALUE))))
                        .addContainerGap())
            );
            jPanel2Layout.setVerticalGroup(
                jPanel2Layout.createParallelGroup()
                    .addGroup(jPanel2Layout.createSequentialGroup()
                        .addContainerGap()
                        .addGroup(jPanel2Layout.createParallelGroup()
                            .addComponent(jButtonResetSets)
                            .addComponent(jButtonHilfeReset))
                        .addPreferredGap(LayoutStyle.ComponentPlacement.RELATED)
                        .addComponent(jLabel7, GroupLayout.PREFERRED_SIZE, 54, GroupLayout.PREFERRED_SIZE)
                        .addPreferredGap(LayoutStyle.ComponentPlacement.RELATED)
                        .addComponent(jSeparator1, GroupLayout.PREFERRED_SIZE, GroupLayout.DEFAULT_SIZE, GroupLayout.PREFERRED_SIZE)
                        .addPreferredGap(LayoutStyle.ComponentPlacement.RELATED)
                        .addComponent(jButtonResetAll)
                        .addPreferredGap(LayoutStyle.ComponentPlacement.RELATED)
                        .addComponent(jLabel10, GroupLayout.PREFERRED_SIZE, GroupLayout.DEFAULT_SIZE, GroupLayout.PREFERRED_SIZE)
                        .addContainerGap(GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
            );
        }

        GroupLayout layout = new GroupLayout(this);
        setLayout(layout);
        layout.setHorizontalGroup(
            layout.createParallelGroup()
                .addGroup(layout.createSequentialGroup()
                    .addContainerGap()
                    .addGroup(layout.createParallelGroup()
                        .addComponent(jLabel1, GroupLayout.DEFAULT_SIZE, 0, Short.MAX_VALUE)
                        .addComponent(jPanel2, GroupLayout.DEFAULT_SIZE, GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
                    .addContainerGap())
        );
        layout.setVerticalGroup(
            layout.createParallelGroup()
                .addGroup(layout.createSequentialGroup()
                    .addContainerGap()
                    .addComponent(jLabel1, GroupLayout.PREFERRED_SIZE, 167, GroupLayout.PREFERRED_SIZE)
                    .addPreferredGap(LayoutStyle.ComponentPlacement.RELATED)
                    .addComponent(jPanel2, GroupLayout.PREFERRED_SIZE, GroupLayout.DEFAULT_SIZE, GroupLayout.PREFERRED_SIZE)
                    .addContainerGap(GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
        );
    }// </editor-fold>//GEN-END:initComponents

    // Variables declaration - do not modify//GEN-BEGIN:variables
    // Generated using JFormDesigner non-commercial license
    private JButton jButtonResetSets;
    private JButton jButtonHilfeReset;
    private JButton jButtonResetAll;
    // End of variables declaration//GEN-END:variables
}
