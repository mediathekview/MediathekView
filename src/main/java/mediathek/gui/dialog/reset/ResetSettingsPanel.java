package mediathek.gui.dialog.reset;

import mediathek.config.Daten;
import mediathek.config.Icons;
import mediathek.daten.ListePsetVorlagen;
import mediathek.file.GetFile;
import mediathek.gui.dialog.DialogHilfe;
import mediathek.gui.messages.ProgramSetChangedEvent;
import mediathek.mainwindow.MediathekGui;
import mediathek.tool.GuiFunktionenProgramme;
import mediathek.tool.swing.MultilineLabel;
import org.jdesktop.swingx.VerticalLayout;

import javax.swing.*;
import javax.swing.border.EmptyBorder;
import javax.swing.border.LineBorder;
import java.awt.*;

@SuppressWarnings("serial")
public class ResetSettingsPanel extends JPanel {
    private static final String RESET_MESSAGE = "<html>Es werden <b>ALLE</b> von Ihnen erzeugten Änderungen gelöscht.<br>" +
            "Möchten Sie wirklich alle Einstellungen zurücksetzen?<br></html>";

    public ResetSettingsPanel(JFrame parent) {
        initComponents();

        jButtonHilfeReset.setIcon(Icons.ICON_BUTTON_HELP);
        jButtonHilfeReset.addActionListener(e -> new DialogHilfe(parent, true, new GetFile().getHilfeSuchen(GetFile.PFAD_HILFETEXT_RESET)).setVisible(true));
        jButtonResetSets.addActionListener(e -> {
            Daten.listePset.clear();
            GuiFunktionenProgramme.addSetVorlagen(parent, Daten.getInstance(), ListePsetVorlagen.getStandarset(parent, true), true);
            Daten.getInstance().getMessageBus().publishAsync(new ProgramSetChangedEvent());
        });
        jButtonResetAll.addActionListener(e -> {
            int ret = JOptionPane.showConfirmDialog(parent, RESET_MESSAGE, "Einstellungen zurücksetzen", JOptionPane.YES_NO_OPTION);
            if (ret == JOptionPane.OK_OPTION) {
                // damit wird vor dem Beenden das Konfig-Verzeichnis umbenannt und so startet das
                // Programm wie beim ersten Start
                Daten.setResetConfigurationData(true);
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
        var jLabel7 = new MultilineLabel();
        var jSeparator1 = new JSeparator();
        jButtonResetAll = new JButton();
        var jLabel10 = new JLabel();

        //======== this ========
        setBorder(new EmptyBorder(5, 5, 5, 5));
        setLayout(new VerticalLayout(5));

        //---- jLabel1 ----
        jLabel1.setText("<html>Bei Problemen sollten die Anleitung oder die FAQ die erste Anlaufstelle sein.<br>F\u00fchrt das zu keiner L\u00f6sung, kann auch eine Suche im Forum weiterhelfen.<br><br> Wenn auch das nicht weiterhilft, sollte man eine Anfrage im Forum stellen. Damit diese auch beantwortet  werden kann,<br>sind ein paar Infos wichtig:<br>  * M\u00f6glichst <b>genaue Beschreibung</b> des Problems (was geht nicht, welcher Film, ..)<br>  * Infos \u00fcber das Betriebssystem und die Programmversion</html>"); //NON-NLS
        add(jLabel1);

        //======== jPanel2 ========
        {
            jPanel2.setBorder(new LineBorder(new Color(102, 102, 102)));

            //---- jButtonResetSets ----
            jButtonResetSets.setText("Einstellungen zum Abspielen/Aufzeichnen zur\u00fccksetzen"); //NON-NLS

            //---- jButtonHilfeReset ----
            jButtonHilfeReset.setIcon(new ImageIcon(getClass().getResource("/mediathek/res/muster/button-help.png"))); //NON-NLS
            jButtonHilfeReset.setToolTipText("Hilfe anzeigen"); //NON-NLS

            //---- jLabel7 ----
            jLabel7.setText("Es werden alle Programmsets (auch eigene) zum Abspielen und Aufzeichnen gel\u00f6scht und die Standardsets wieder angelegt.\nAbos und Blacklist bleiben erhalten."); //NON-NLS
            jLabel7.setRows(3);

            //---- jButtonResetAll ----
            jButtonResetAll.setText("Alle Einstellungen zur\u00fccksetzen!"); //NON-NLS

            //---- jLabel10 ----
            jLabel10.setText("<html>Alle Einstellungen gehen verloren.<br><b>ACHTUNG</b>, es werden auch eigene Buttons, Abos und die Blacklist gel\u00f6scht.</html>"); //NON-NLS

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
                                    .addComponent(jLabel10)
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
        add(jPanel2);
    }// </editor-fold>//GEN-END:initComponents

    // Variables declaration - do not modify//GEN-BEGIN:variables
    // Generated using JFormDesigner non-commercial license
    private JButton jButtonResetSets;
    private JButton jButtonHilfeReset;
    private JButton jButtonResetAll;
    // End of variables declaration//GEN-END:variables
}
