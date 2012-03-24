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
package mediathek;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.File;
import javax.swing.JCheckBox;
import javax.swing.JFileChooser;
import mediathek.daten.DDaten;
import mediathek.daten.ListePset;
import mediathek.gui.beobachter.EscBeenden;
import mediathek.gui.dialogEinstellungen.PanelProgrammPfade;
import mediathek.gui.dialogEinstellungen.PanelPsetKurz;
import mediathek.gui.dialogEinstellungen.PanelPsetLang;
import mediathek.importOld.IoXmlLesen__old;
import mediathek.tool.GuiFunktionenProgramme;

public class DialogStarteinstellungen extends javax.swing.JDialog {

    DDaten ddaten;
    private final int STAT_START = 1;
    private final int STAT_PFAD = 2;
    private final int STAT_PSET = 3;
    private final int STAT_FERTIG = 4;
    private int status = STAT_START;

    public DialogStarteinstellungen(java.awt.Frame parent, boolean modal, DDaten dd) {
        super(parent, modal);
        initComponents();
        ddaten = dd;
        this.setTitle("Erster Start");
        jButtonStandard.addActionListener(new ActionListener() {

            @Override
            public void actionPerformed(ActionEvent e) {
                jCheckBoxAnpassen.setVisible(false);
                weiter();
            }
        });
        jButtonAlt.addActionListener(new ActionListener() {

            @Override
            public void actionPerformed(ActionEvent e) {
                alt();
            }
        });
        jButtonZiel.addActionListener(new ZielBeobachter());
        new EscBeenden(this) {

            @Override
            public void beenden_() {
                weiter();
            }
        };
        jCheckBoxAlleEinstellungen.setVisible(false);
        jCheckBoxAlleEinstellungen.addActionListener(new ActionListener() {

            @Override
            public void actionPerformed(ActionEvent e) {
                status = STAT_PSET;
                weiter();
            }
        });
        jCheckBoxSuchen.setSelected(true);
        Daten.system[Konstanten.SYSTEM_UPDATE_SUCHEN_NR] = Boolean.TRUE.toString();
        jCheckBoxSuchen.addActionListener(new BeobCheckBoxSuchen());
        String dateiAlt = IoXmlLesen__old.altExistiert();
        jTextFieldPfad.setText(dateiAlt);
        Daten.system[Konstanten.SYSTEM_PFAD_MPLAYER_NR] = GuiFunktionenProgramme.getMusterPfadMplayer();
        Daten.system[Konstanten.SYSTEM_PFAD_VLC_NR] = GuiFunktionenProgramme.getMusterPfadVlc();
        Daten.system[Konstanten.SYSTEM_PFAD_FLVSTREAMER_NR] = GuiFunktionenProgramme.getMusterPfadFlv();
    }

    private void weiter() {
        switch (status) {
            case STAT_START:
                statusStart();
                break;
            case STAT_PFAD:
                statusPfade();
                break;
            case STAT_PSET:
                statusPset();
                break;
            default:
                beenden();
                break;
        }
    }

    private void alt() {
        if (!jTextFieldPfad.getText().equals("")) {
            new IoXmlLesen__old().importOld(ddaten, jTextFieldPfad.getText());
            status = STAT_PSET;
            weiter();
        }
    }

    private void statusStart() {
        jButtonStandard.setText("Weiter");
        if (Daten.system[Konstanten.SYSTEM_PFAD_VLC_NR].equals("") || Daten.system[Konstanten.SYSTEM_PFAD_FLVSTREAMER_NR].equals("")) {
            status = STAT_PFAD;
        } else if (jCheckBoxAnpassen.isSelected()) {
            status = STAT_PFAD;
        } else {
            // nur dann automatisch Standardprogramme einrichten, sonst fragen
            ListePset pSet = GuiFunktionenProgramme.getStandardprogramme(ddaten);
            if (pSet != null) {
                ddaten.listePset.addPset(pSet);
                status = STAT_FERTIG;
            } else {
                status = STAT_PSET;
            }
        }
        weiter();
    }

    private void statusPfade() {
        // erst Programmpfad prüfen
        jCheckBoxAnpassen.setVisible(false);
        jCheckBoxAlleEinstellungen.setVisible(false);
        jScrollPane1.setViewportView(new PanelProgrammPfade(true /* vlc */, true /* flvstreamer */, false /* mplayer */));
        status = STAT_PSET;
        jButtonStandard.setText("Weiter");
    }

    private void statusPset() {
        jCheckBoxAnpassen.setVisible(false);
        jCheckBoxAlleEinstellungen.setVisible(true);
        if (ddaten.listePset.size() == 0) {
            // Standardset hinzufügen
            ddaten.listePset.addPset(GuiFunktionenProgramme.getStandardprogramme(ddaten));
        }
        if (jCheckBoxAlleEinstellungen.isSelected()) {
            jScrollPane1.setViewportView(new PanelPsetLang(ddaten, ddaten.listePset));
        } else {
            jScrollPane1.setViewportView(new PanelPsetKurz(ddaten, ddaten.listePset));
        }
        status = STAT_FERTIG;
        jButtonStandard.setText("Weiter");
    }

    private void beenden() {
        this.dispose();
    }

    /** This method is called from within the constructor to
     * initialize the form.
     * WARNING: Do NOT modify this code. The content of this method is
     * always regenerated by the Form Editor.
     */
    // <editor-fold defaultstate="collapsed" desc="Generated Code">//GEN-BEGIN:initComponents
    private void initComponents() {

        jPanel2 = new javax.swing.JPanel();
        jButtonStandard = new javax.swing.JButton();
        jCheckBoxAlleEinstellungen = new javax.swing.JCheckBox();
        jCheckBoxAnpassen = new javax.swing.JCheckBox();
        jScrollPane1 = new javax.swing.JScrollPane();
        jPanelExtra = new javax.swing.JPanel();
        jPanel7 = new javax.swing.JPanel();
        jCheckBoxSuchen = new javax.swing.JCheckBox();
        jTextField2 = new javax.swing.JTextField();
        jPanel1 = new javax.swing.JPanel();
        jButtonZiel = new javax.swing.JButton();
        jTextFieldPfad = new javax.swing.JTextField();
        jLabel1 = new javax.swing.JLabel();
        jButtonAlt = new javax.swing.JButton();

        setDefaultCloseOperation(javax.swing.WindowConstants.DISPOSE_ON_CLOSE);

        jPanel2.setBorder(javax.swing.BorderFactory.createLineBorder(new java.awt.Color(153, 153, 255), 3));

        jButtonStandard.setText("Mit Standardeinstellungen starten");

        jCheckBoxAlleEinstellungen.setText("alle Einstellungen anzeigen");

        jCheckBoxAnpassen.setText("vorher anpassen");

        javax.swing.GroupLayout jPanel2Layout = new javax.swing.GroupLayout(jPanel2);
        jPanel2.setLayout(jPanel2Layout);
        jPanel2Layout.setHorizontalGroup(
            jPanel2Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(javax.swing.GroupLayout.Alignment.TRAILING, jPanel2Layout.createSequentialGroup()
                .addContainerGap()
                .addComponent(jCheckBoxAlleEinstellungen)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                .addComponent(jCheckBoxAnpassen)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                .addComponent(jButtonStandard)
                .addContainerGap())
        );
        jPanel2Layout.setVerticalGroup(
            jPanel2Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel2Layout.createSequentialGroup()
                .addContainerGap()
                .addGroup(jPanel2Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(jButtonStandard)
                    .addComponent(jCheckBoxAlleEinstellungen)
                    .addComponent(jCheckBoxAnpassen))
                .addContainerGap(javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
        );

        jPanel7.setBorder(javax.swing.BorderFactory.createTitledBorder("Programmupdate"));

        jCheckBoxSuchen.setSelected(true);
        jCheckBoxSuchen.setText("Einmal am Tag nach einer neuen Programmversion suchen");

        javax.swing.GroupLayout jPanel7Layout = new javax.swing.GroupLayout(jPanel7);
        jPanel7.setLayout(jPanel7Layout);
        jPanel7Layout.setHorizontalGroup(
            jPanel7Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel7Layout.createSequentialGroup()
                .addContainerGap()
                .addComponent(jCheckBoxSuchen)
                .addContainerGap(243, Short.MAX_VALUE))
        );
        jPanel7Layout.setVerticalGroup(
            jPanel7Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel7Layout.createSequentialGroup()
                .addComponent(jCheckBoxSuchen)
                .addContainerGap(javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
        );

        jTextField2.setBackground(new java.awt.Color(204, 204, 204));
        jTextField2.setFont(new java.awt.Font("Dialog", 1, 14)); // NOI18N
        jTextField2.setHorizontalAlignment(javax.swing.JTextField.CENTER);
        jTextField2.setText("Alte Einstellungen");

        jPanel1.setBorder(javax.swing.BorderFactory.createLineBorder(new java.awt.Color(204, 204, 204)));

        jButtonZiel.setIcon(new javax.swing.ImageIcon(getClass().getResource("/mediathek/res/fileopen_16.png"))); // NOI18N
        jButtonZiel.setContentAreaFilled(false);

        jTextFieldPfad.setEditable(false);

        jLabel1.setText("Pfad:");

        javax.swing.GroupLayout jPanel1Layout = new javax.swing.GroupLayout(jPanel1);
        jPanel1.setLayout(jPanel1Layout);
        jPanel1Layout.setHorizontalGroup(
            jPanel1Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel1Layout.createSequentialGroup()
                .addContainerGap()
                .addComponent(jLabel1)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(jTextFieldPfad)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(jButtonZiel)
                .addContainerGap())
        );
        jPanel1Layout.setVerticalGroup(
            jPanel1Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel1Layout.createSequentialGroup()
                .addContainerGap()
                .addGroup(jPanel1Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.CENTER)
                    .addComponent(jLabel1)
                    .addComponent(jTextFieldPfad, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                    .addComponent(jButtonZiel))
                .addContainerGap(javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
        );

        jPanel1Layout.linkSize(javax.swing.SwingConstants.VERTICAL, new java.awt.Component[] {jButtonZiel, jTextFieldPfad});

        jButtonAlt.setText("Mit alten Einstellungen starten");
        jButtonAlt.setContentAreaFilled(false);

        javax.swing.GroupLayout jPanelExtraLayout = new javax.swing.GroupLayout(jPanelExtra);
        jPanelExtra.setLayout(jPanelExtraLayout);
        jPanelExtraLayout.setHorizontalGroup(
            jPanelExtraLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanelExtraLayout.createSequentialGroup()
                .addContainerGap()
                .addGroup(jPanelExtraLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addComponent(jPanel7, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                    .addComponent(jTextField2, javax.swing.GroupLayout.Alignment.TRAILING)
                    .addComponent(jPanel1, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                    .addGroup(javax.swing.GroupLayout.Alignment.TRAILING, jPanelExtraLayout.createSequentialGroup()
                        .addGap(0, 0, Short.MAX_VALUE)
                        .addComponent(jButtonAlt)))
                .addContainerGap())
        );
        jPanelExtraLayout.setVerticalGroup(
            jPanelExtraLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(javax.swing.GroupLayout.Alignment.TRAILING, jPanelExtraLayout.createSequentialGroup()
                .addContainerGap()
                .addComponent(jTextField2, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                .addComponent(jPanel1, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addGap(18, 18, 18)
                .addComponent(jButtonAlt)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED, 217, Short.MAX_VALUE)
                .addComponent(jPanel7, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addContainerGap())
        );

        jScrollPane1.setViewportView(jPanelExtra);

        javax.swing.GroupLayout layout = new javax.swing.GroupLayout(getContentPane());
        getContentPane().setLayout(layout);
        layout.setHorizontalGroup(
            layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(layout.createSequentialGroup()
                .addContainerGap()
                .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addComponent(jScrollPane1)
                    .addComponent(jPanel2, javax.swing.GroupLayout.Alignment.TRAILING, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
                .addContainerGap())
        );
        layout.setVerticalGroup(
            layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(layout.createSequentialGroup()
                .addContainerGap()
                .addComponent(jScrollPane1)
                .addGap(18, 18, 18)
                .addComponent(jPanel2, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addContainerGap())
        );

        pack();
    }// </editor-fold>//GEN-END:initComponents
    /**
     * @param args the command line arguments
     */
    // Variables declaration - do not modify//GEN-BEGIN:variables
    private javax.swing.JButton jButtonAlt;
    private javax.swing.JButton jButtonStandard;
    private javax.swing.JButton jButtonZiel;
    private javax.swing.JCheckBox jCheckBoxAlleEinstellungen;
    private javax.swing.JCheckBox jCheckBoxAnpassen;
    private javax.swing.JCheckBox jCheckBoxSuchen;
    private javax.swing.JLabel jLabel1;
    private javax.swing.JPanel jPanel1;
    private javax.swing.JPanel jPanel2;
    private javax.swing.JPanel jPanel7;
    private javax.swing.JPanel jPanelExtra;
    private javax.swing.JScrollPane jScrollPane1;
    private javax.swing.JTextField jTextField2;
    private javax.swing.JTextField jTextFieldPfad;
    // End of variables declaration//GEN-END:variables

    private class ZielBeobachter implements ActionListener {

        @Override
        public void actionPerformed(ActionEvent e) {
            int returnVal;
            JFileChooser chooser = new JFileChooser();
            chooser.setFileHidingEnabled(false);
            if (!jTextFieldPfad.getText().equals("")) {
                chooser.setCurrentDirectory(new File(jTextFieldPfad.getText()));
            }
            chooser.setFileSelectionMode(JFileChooser.FILES_ONLY);
            returnVal = chooser.showOpenDialog(null);
            if (returnVal == JFileChooser.APPROVE_OPTION) {
                try {
                    jTextFieldPfad.setText(chooser.getSelectedFile().getAbsolutePath());
                } catch (Exception ex) {
                    Log.fehlerMeldung("DialogImportOld.ZielBeobachter", ex);
                }
            }
        }
    }

    private class BeobCheckBoxSuchen implements ActionListener {

        @Override
        public void actionPerformed(ActionEvent e) {
            Daten.system[Konstanten.SYSTEM_UPDATE_SUCHEN_NR] = Boolean.toString(jCheckBoxSuchen.isSelected());
        }
    }
}
