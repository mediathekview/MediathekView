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
import javax.swing.JFileChooser;
import mediathek.daten.DDaten;
import mediathek.gui.beobachter.EscBeenden;
import mediathek.gui.dialog.DialogOk;
import mediathek.gui.dialogEinstellungen.PanelProgrammPfade;
import mediathek.importOld.IoXmlLesen__old;
import mediathek.tool.GuiFunktionenProgramme;

public class DialogStarteinstellungen extends javax.swing.JDialog {

    DDaten ddaten;

    public DialogStarteinstellungen(java.awt.Frame parent, boolean modal, DDaten dd) {
        super(parent, modal);
        initComponents();
        ddaten = dd;
        this.setTitle("Erster Start");
        jButtonStandard.addActionListener(new ActionListener() {

            @Override
            public void actionPerformed(ActionEvent e) {
                standard();
            }
        });
        jButtonAnpassen.addActionListener(new ActionListener() {

            @Override
            public void actionPerformed(ActionEvent e) {
                beenden();
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
                beenden();
            }
        };
        jCheckBoxSuchen.setSelected(true);
        Daten.system[Konstanten.SYSTEM_UPDATE_SUCHEN_NR] = Boolean.TRUE.toString();
        jCheckBoxSuchen.addActionListener(new BeobCheckBoxSuchen());
        String dateiAlt = IoXmlLesen__old.altExistiert();
        jTextFieldPfad.setText(dateiAlt);
    }

    private void standard() {
//////        Daten.system[Konstanten.SYSTEM_PFAD_VLC_NR] = getPfadVlc();
//////        Daten.system[Konstanten.SYSTEM_PFAD_FLVSTREAMER_NR] = getPfadFlv();
//////        if (!Daten.system[Konstanten.SYSTEM_PFAD_VLC_NR].equals("") && !Daten.system[Konstanten.SYSTEM_PFAD_VLC_NR].equals("")) {
//////            // nur dann automatisch Standardprogramme einrichten, sonst fragen
//////            // dann mit Standardwerten füllen
//////            GuiFunktionenProgramme.addStandardprogramme(ddaten, true /* auto */);
//////        } else {
        new DialogOk(null, true, new PanelProgrammPfade(), "Pfade Standardprogramme").setVisible(true);
//////////        }
        beenden();
    }

    private void alt() {
        if (!jTextFieldPfad.getText().equals("")) {
            new IoXmlLesen__old().importOld(ddaten, jTextFieldPfad.getText());
            beenden();
        }
    }

    private void beenden() {
        if (ddaten.listePset.size() == 0) {
            // dann mit Standardwerten füllen
            GuiFunktionenProgramme.addStandardprogramme(ddaten, false /* auto */);
        }
        this.dispose();
    }

    private String getPfadVlc() {
        final String PFAD_LINUX_VLC = "/usr/bin/vlc";
        final String PFAD_MAC_VLC = "/Applications/VLC.app/Contents/MacOS/VLC";
        String pfad = "";
        if (System.getProperty("os.name").toLowerCase().contains("windows")) {
            pfad = getWindowsVlcPath();
        } else if (System.getProperty("os.name").toLowerCase().contains("linux")) {
            pfad = PFAD_LINUX_VLC;
        } else if (System.getProperty("os.name").toLowerCase().contains("mac")) {
            pfad = PFAD_MAC_VLC;
        }
        if (new File(pfad).exists()) {
            return pfad;
        } else {
            return "";
        }
    }

    private static String getWindowsVlcPath() {
        //Für Windows den Pfad des VLC ermitteln
        //sonst den deutschen Defaultpfad für Programme verwenden verwenden
        final String PFAD_WIN_VLC_DEFAULT = "C:\\Programme\\VideoLAN\\VLC\\vlc.exe";
        final String PFAD_WIN_VLC = "\\VideoLAN\\VLC\\vlc.exe";
        String vlcPfad = "";
        try {
            if (System.getProperty("os.name").toLowerCase().contains("windows")) {
                if (System.getenv("ProgramFiles") != null) {
                    vlcPfad = System.getenv("ProgramFiles") + PFAD_WIN_VLC;
                    if (new File(vlcPfad).exists()) {
                        return vlcPfad;
                    }
                }
            }
            if (System.getenv("ProgramFiles(x86)") != null) {
                vlcPfad = System.getenv("ProgramFiles(x86)") + PFAD_WIN_VLC;
                if (new File(vlcPfad).exists()) {
                    return vlcPfad;
                }
            }
        } catch (Exception ex) {
        }
        return PFAD_WIN_VLC_DEFAULT;
    }

    private String getPfadFlv() {
        final String PFAD_LINUX_FLV = "/usr/bin/flvstreamer";
        final String PFAD_WINDOWS_FLV = "bin\\flvstreamer_win32_latest.exe";
        String pfad = "";
        if (System.getProperty("os.name").toLowerCase().contains("windows")) {
            pfad = GuiFunktionenProgramme.getPathJar() + PFAD_WINDOWS_FLV;
        } else if (System.getProperty("os.name").toLowerCase().contains("linux")) {
            pfad = PFAD_LINUX_FLV;
        }
        if (new File(pfad).exists()) {
            return pfad;
        } else {
            return "";
        }
    }

    /** This method is called from within the constructor to
     * initialize the form.
     * WARNING: Do NOT modify this code. The content of this method is
     * always regenerated by the Form Editor.
     */
    // <editor-fold defaultstate="collapsed" desc="Generated Code">//GEN-BEGIN:initComponents
    private void initComponents() {

        jPanel7 = new javax.swing.JPanel();
        jCheckBoxSuchen = new javax.swing.JCheckBox();
        jButtonStandard = new javax.swing.JButton();
        jButtonAnpassen = new javax.swing.JButton();
        jButtonAlt = new javax.swing.JButton();
        jTextField1 = new javax.swing.JTextField();
        jTextField2 = new javax.swing.JTextField();
        jPanel1 = new javax.swing.JPanel();
        jButtonZiel = new javax.swing.JButton();
        jTextFieldPfad = new javax.swing.JTextField();
        jLabel1 = new javax.swing.JLabel();
        filler1 = new javax.swing.Box.Filler(new java.awt.Dimension(0, 15), new java.awt.Dimension(0, 15), new java.awt.Dimension(32767, 15));
        filler2 = new javax.swing.Box.Filler(new java.awt.Dimension(0, 15), new java.awt.Dimension(0, 15), new java.awt.Dimension(32767, 15));

        setDefaultCloseOperation(javax.swing.WindowConstants.DISPOSE_ON_CLOSE);

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
                .addContainerGap(318, Short.MAX_VALUE))
        );
        jPanel7Layout.setVerticalGroup(
            jPanel7Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel7Layout.createSequentialGroup()
                .addComponent(jCheckBoxSuchen)
                .addContainerGap(javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
        );

        jButtonStandard.setText("Mit Standardeinstellungen starten");

        jButtonAnpassen.setText("Einstellungen vorher anpassen");
        jButtonAnpassen.setContentAreaFilled(false);

        jButtonAlt.setText("Mit alten Einstellungen starten");
        jButtonAlt.setContentAreaFilled(false);

        jTextField1.setBackground(new java.awt.Color(204, 204, 255));
        jTextField1.setFont(new java.awt.Font("Dialog", 1, 14)); // NOI18N
        jTextField1.setHorizontalAlignment(javax.swing.JTextField.CENTER);
        jTextField1.setText("Standardeinstellungen");

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

        javax.swing.GroupLayout layout = new javax.swing.GroupLayout(getContentPane());
        getContentPane().setLayout(layout);
        layout.setHorizontalGroup(
            layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(layout.createSequentialGroup()
                .addContainerGap()
                .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addComponent(filler1, javax.swing.GroupLayout.Alignment.TRAILING, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                    .addComponent(jTextField1)
                    .addComponent(jPanel7, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                    .addComponent(jPanel1, javax.swing.GroupLayout.Alignment.TRAILING, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                    .addGroup(javax.swing.GroupLayout.Alignment.TRAILING, layout.createSequentialGroup()
                        .addGap(0, 0, Short.MAX_VALUE)
                        .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                            .addComponent(jButtonStandard, javax.swing.GroupLayout.Alignment.TRAILING)
                            .addComponent(jButtonAnpassen, javax.swing.GroupLayout.Alignment.TRAILING)
                            .addComponent(jButtonAlt, javax.swing.GroupLayout.Alignment.TRAILING)))
                    .addComponent(jTextField2)
                    .addComponent(filler2, javax.swing.GroupLayout.Alignment.TRAILING, javax.swing.GroupLayout.DEFAULT_SIZE, 773, Short.MAX_VALUE))
                .addContainerGap())
        );

        layout.linkSize(javax.swing.SwingConstants.HORIZONTAL, new java.awt.Component[] {jButtonAlt, jButtonAnpassen, jButtonStandard});

        layout.setVerticalGroup(
            layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(javax.swing.GroupLayout.Alignment.TRAILING, layout.createSequentialGroup()
                .addContainerGap()
                .addComponent(jTextField1, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addGap(18, 18, 18)
                .addComponent(jButtonStandard)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                .addComponent(jButtonAnpassen)
                .addGap(18, 18, 18)
                .addComponent(filler1, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addGap(18, 18, 18)
                .addComponent(jTextField2, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addGap(18, 18, 18)
                .addComponent(jPanel1, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addGap(18, 18, 18)
                .addComponent(jButtonAlt)
                .addGap(18, 18, 18)
                .addComponent(filler2, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED, 18, Short.MAX_VALUE)
                .addComponent(jPanel7, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addContainerGap())
        );

        pack();
    }// </editor-fold>//GEN-END:initComponents
    /**
     * @param args the command line arguments
     */
    // Variables declaration - do not modify//GEN-BEGIN:variables
    private javax.swing.Box.Filler filler1;
    private javax.swing.Box.Filler filler2;
    private javax.swing.JButton jButtonAlt;
    private javax.swing.JButton jButtonAnpassen;
    private javax.swing.JButton jButtonStandard;
    private javax.swing.JButton jButtonZiel;
    private javax.swing.JCheckBox jCheckBoxSuchen;
    private javax.swing.JLabel jLabel1;
    private javax.swing.JPanel jPanel1;
    private javax.swing.JPanel jPanel7;
    private javax.swing.JTextField jTextField1;
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
