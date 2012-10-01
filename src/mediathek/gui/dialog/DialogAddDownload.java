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
package mediathek.gui.dialog;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.File;
import javax.swing.JFileChooser;
import javax.swing.JOptionPane;
import mediathek.controller.io.starter.Start;
import mediathek.daten.DDaten;
import mediathek.daten.DatenDownload;
import mediathek.daten.DatenFilm;
import mediathek.daten.DatenPset;
import mediathek.tool.EscBeenden;
import mediathek.tool.GuiFunktionenProgramme;
import mediathek.tool.Log;

public class DialogAddDownload extends javax.swing.JDialog {

    private DatenPset pSet = null;
    private boolean ok = false;
    private DatenDownload datenDownload = null;
    private DDaten ddaten;
    private DatenFilm datenFilm;

    public DialogAddDownload(java.awt.Frame parent, DDaten dd, DatenFilm film) {
        super(parent, true);
        initComponents();
        ddaten = dd;
        datenFilm = film;
        this.setTitle("Film Speichern");
        // Felder init
        init();
    }

    private void init() {
        if (ddaten.listePset.getListeSpeichern().size() == 0) {
            JOptionPane.showMessageDialog(null, "Im Menü unter \"Datei->Optionen->Videoplayer\" ein Programm zum Aufzeichnen festlegen.",
                    "kein Videoplayer!", JOptionPane.INFORMATION_MESSAGE);
            // Satz mit x, war wohl nix
            ok = false;
            beenden();
        }
        jButtonZiel.addActionListener(new ZielBeobachter());
        jButtonBeenden.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                if (check()) {
                    beenden();
                }
            }
        });
        getRootPane().setDefaultButton(jButtonBeenden); //TH
        new EscBeenden(this) {
            @Override
            public void beenden_() {
                ok = false;
                beenden();
            }
        };
        jButtonAbbrechen.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                ok = false;
                beenden();
            }
        });
        jComboBoxPgr.setModel(new javax.swing.DefaultComboBoxModel(ddaten.listePset.getListeSpeichern().getObjectDataCombo()));
        setGruppe();
        if (ddaten.listePset.getListeSpeichern().size() == 1) {
            // macht dann keinen Sinn
            jComboBoxPgr.setEnabled(false);
        } else {
            jComboBoxPgr.addActionListener(new BeobComboProgramm());
        }
        jTextFieldSender.setText(datenFilm.arr[DatenFilm.FILM_SENDER_NR]);
        jTextFieldTitel.setText(datenFilm.arr[DatenFilm.FILM_TITEL_NR]);
    }

    private void setGruppe() {
        pSet = ddaten.listePset.getListeSpeichern().get(jComboBoxPgr.getSelectedIndex());
        // beim ersten mal werden die Standardpfade gesucht
        datenDownload = new DatenDownload(pSet, datenFilm, Start.QUELLE_DOWNLOAD, null, "", "");
        if (datenDownload.arr[DatenDownload.DOWNLOAD_ZIEL_DATEINAME_NR].equals("")) {
            jTextFieldName.setEnabled(false);
            jTextFieldPfad.setEnabled(false);
            jButtonZiel.setEnabled(false);
            jTextFieldName.setText("");
            jTextFieldPfad.setText("");
        } else {
            jTextFieldName.setEnabled(true);
            jTextFieldPfad.setEnabled(true);
            jButtonZiel.setEnabled(true);
            jTextFieldName.setText(datenDownload.arr[DatenDownload.DOWNLOAD_ZIEL_DATEINAME_NR]);
            jTextFieldPfad.setText(datenDownload.arr[DatenDownload.DOWNLOAD_ZIEL_PFAD_NR]);
        }
    }

    private boolean check() {
        ok = false;
        String pfad = jTextFieldPfad.getText();
        String name = jTextFieldName.getText();
        if (datenDownload != null) {
            if (pfad.equals("") || name.equals("")) {
                JOptionPane.showMessageDialog(null, "Pfad oder Name ist leer", "Fehlerhafter Pfad/Name!", JOptionPane.ERROR_MESSAGE);
            } else {
                if (!pfad.substring(pfad.length() - 1).equals(File.separator)) {
                    pfad += File.separator;
                }
                if (GuiFunktionenProgramme.checkPfadBeschreibbar(pfad)) {
                    ok = true;
                } else {
                    JOptionPane.showMessageDialog(null, "Pfad ist nicht beschreibbar", "Fehlerhafter Pfad!", JOptionPane.ERROR_MESSAGE);
                }
            }
        }
        return ok;
    }

    private void beenden() {
        if (ok) {
            // jetzt wird mit den angegebenen Pfaden gearbeitet
            datenDownload = new DatenDownload(pSet, datenFilm, Start.QUELLE_DOWNLOAD, null, jTextFieldName.getText(), jTextFieldPfad.getText());
            ddaten.listeDownloads.add(datenDownload);
            if (jCheckBoxStarten.isSelected()) {
                // und evtl. auch gleich starten
                datenDownload.starten(ddaten);
            }
        }
        this.dispose();
    }

    /** This method is called from within the constructor to
     * initialize the form.
     * WARNING: Do NOT modify this code. The content of this method is
     * always regenerated by the Form Editor.
     */
    // <editor-fold defaultstate="collapsed" desc="Generated Code">//GEN-BEGIN:initComponents
    private void initComponents() {

        jButtonBeenden = new javax.swing.JButton();
        jButtonAbbrechen = new javax.swing.JButton();
        jCheckBoxStarten = new javax.swing.JCheckBox();
        jPanel3 = new javax.swing.JPanel();
        jLabel2 = new javax.swing.JLabel();
        jLabel3 = new javax.swing.JLabel();
        jTextFieldTitel = new javax.swing.JTextField();
        jTextFieldSender = new javax.swing.JTextField();
        jPanel5 = new javax.swing.JPanel();
        jComboBoxPgr = new javax.swing.JComboBox();
        jPanel1 = new javax.swing.JPanel();
        jTextFieldName = new javax.swing.JTextField();
        jButtonZiel = new javax.swing.JButton();
        jTextFieldPfad = new javax.swing.JTextField();
        jLabel1 = new javax.swing.JLabel();
        jLabel4 = new javax.swing.JLabel();

        setDefaultCloseOperation(javax.swing.WindowConstants.DISPOSE_ON_CLOSE);

        jButtonBeenden.setText("Ok");

        jButtonAbbrechen.setText("Abbrechen");

        jCheckBoxStarten.setSelected(true);
        jCheckBoxStarten.setText("Download sofort starten");

        jPanel3.setBorder(javax.swing.BorderFactory.createTitledBorder("Film"));

        jLabel2.setText("Sender:");

        jLabel3.setText("Titel:");

        jTextFieldTitel.setEditable(false);
        jTextFieldTitel.setText("jTextField2");

        jTextFieldSender.setEditable(false);
        jTextFieldSender.setText("jTextField1");

        javax.swing.GroupLayout jPanel3Layout = new javax.swing.GroupLayout(jPanel3);
        jPanel3.setLayout(jPanel3Layout);
        jPanel3Layout.setHorizontalGroup(
            jPanel3Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel3Layout.createSequentialGroup()
                .addContainerGap()
                .addGroup(jPanel3Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addComponent(jLabel2)
                    .addComponent(jLabel3))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addGroup(jPanel3Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addComponent(jTextFieldSender)
                    .addComponent(jTextFieldTitel))
                .addContainerGap())
        );
        jPanel3Layout.setVerticalGroup(
            jPanel3Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel3Layout.createSequentialGroup()
                .addContainerGap()
                .addGroup(jPanel3Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.CENTER)
                    .addComponent(jLabel2)
                    .addComponent(jTextFieldSender, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addGroup(jPanel3Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.CENTER)
                    .addComponent(jLabel3)
                    .addComponent(jTextFieldTitel, javax.swing.GroupLayout.PREFERRED_SIZE, 25, javax.swing.GroupLayout.PREFERRED_SIZE))
                .addContainerGap(javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
        );

        jPanel3Layout.linkSize(javax.swing.SwingConstants.VERTICAL, new java.awt.Component[] {jTextFieldSender, jTextFieldTitel});

        jPanel5.setBorder(javax.swing.BorderFactory.createTitledBorder("Programmset zum Aufzeichnen"));

        jComboBoxPgr.setModel(new javax.swing.DefaultComboBoxModel(new String[] { "Item 1", "Item 2", "Item 3", "Item 4" }));

        javax.swing.GroupLayout jPanel5Layout = new javax.swing.GroupLayout(jPanel5);
        jPanel5.setLayout(jPanel5Layout);
        jPanel5Layout.setHorizontalGroup(
            jPanel5Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel5Layout.createSequentialGroup()
                .addContainerGap()
                .addComponent(jComboBoxPgr, 0, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                .addContainerGap())
        );
        jPanel5Layout.setVerticalGroup(
            jPanel5Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel5Layout.createSequentialGroup()
                .addContainerGap()
                .addComponent(jComboBoxPgr, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addContainerGap(javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
        );

        jPanel1.setBorder(javax.swing.BorderFactory.createTitledBorder("Speicherort"));

        jButtonZiel.setIcon(new javax.swing.ImageIcon(getClass().getResource("/mediathek/res/fileopen_16.png"))); // NOI18N

        jLabel1.setText("Zielpfad:");

        jLabel4.setText("Dateiname:");

        javax.swing.GroupLayout jPanel1Layout = new javax.swing.GroupLayout(jPanel1);
        jPanel1.setLayout(jPanel1Layout);
        jPanel1Layout.setHorizontalGroup(
            jPanel1Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel1Layout.createSequentialGroup()
                .addContainerGap()
                .addGroup(jPanel1Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addComponent(jLabel4)
                    .addComponent(jLabel1))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addGroup(jPanel1Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addComponent(jTextFieldPfad)
                    .addComponent(jTextFieldName))
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
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addGroup(jPanel1Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.CENTER)
                    .addComponent(jLabel4)
                    .addComponent(jTextFieldName, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE))
                .addContainerGap(javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
        );

        jPanel1Layout.linkSize(javax.swing.SwingConstants.VERTICAL, new java.awt.Component[] {jButtonZiel, jTextFieldName, jTextFieldPfad});

        javax.swing.GroupLayout layout = new javax.swing.GroupLayout(getContentPane());
        getContentPane().setLayout(layout);
        layout.setHorizontalGroup(
            layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(layout.createSequentialGroup()
                .addContainerGap()
                .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addComponent(jPanel3, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                    .addComponent(jPanel5, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                    .addComponent(jPanel1, javax.swing.GroupLayout.Alignment.TRAILING, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                    .addGroup(layout.createSequentialGroup()
                        .addComponent(jCheckBoxStarten)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED, 74, Short.MAX_VALUE)
                        .addComponent(jButtonBeenden, javax.swing.GroupLayout.PREFERRED_SIZE, 93, javax.swing.GroupLayout.PREFERRED_SIZE)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                        .addComponent(jButtonAbbrechen)))
                .addContainerGap())
        );

        layout.linkSize(javax.swing.SwingConstants.HORIZONTAL, new java.awt.Component[] {jButtonAbbrechen, jButtonBeenden});

        layout.setVerticalGroup(
            layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(javax.swing.GroupLayout.Alignment.TRAILING, layout.createSequentialGroup()
                .addContainerGap()
                .addComponent(jPanel3, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                .addComponent(jPanel5, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                .addComponent(jPanel1, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addGap(18, 18, 18)
                .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.CENTER)
                    .addComponent(jButtonBeenden)
                    .addComponent(jButtonAbbrechen)
                    .addComponent(jCheckBoxStarten))
                .addContainerGap(javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
        );

        pack();
    }// </editor-fold>//GEN-END:initComponents
    // Variables declaration - do not modify//GEN-BEGIN:variables
    private javax.swing.JButton jButtonAbbrechen;
    private javax.swing.JButton jButtonBeenden;
    private javax.swing.JButton jButtonZiel;
    private javax.swing.JCheckBox jCheckBoxStarten;
    private javax.swing.JComboBox jComboBoxPgr;
    private javax.swing.JLabel jLabel1;
    private javax.swing.JLabel jLabel2;
    private javax.swing.JLabel jLabel3;
    private javax.swing.JLabel jLabel4;
    private javax.swing.JPanel jPanel1;
    private javax.swing.JPanel jPanel3;
    private javax.swing.JPanel jPanel5;
    private javax.swing.JTextField jTextFieldName;
    private javax.swing.JTextField jTextFieldPfad;
    private javax.swing.JTextField jTextFieldSender;
    private javax.swing.JTextField jTextFieldTitel;
    // End of variables declaration//GEN-END:variables

    private class BeobComboProgramm implements ActionListener {

        @Override
        public void actionPerformed(ActionEvent e) {
            setGruppe();
        }
    }

    private class ZielBeobachter implements ActionListener {

        @Override
        public void actionPerformed(ActionEvent e) {
            int returnVal;
            JFileChooser chooser = new JFileChooser();
            if (!jTextFieldPfad.getText().equals("")) {
                chooser.setCurrentDirectory(new File(jTextFieldPfad.getText()));
            }
            chooser.setFileSelectionMode(JFileChooser.DIRECTORIES_ONLY);
            returnVal = chooser.showOpenDialog(null);
            if (returnVal == JFileChooser.APPROVE_OPTION) {
                try {
                    jTextFieldPfad.setText(chooser.getSelectedFile().getAbsolutePath());
                } catch (Exception ex) {
                    Log.fehlerMeldung(356871087, "DialogAddDownload.ZielBeobachter", ex);
                }
            }
        }
    }
}
