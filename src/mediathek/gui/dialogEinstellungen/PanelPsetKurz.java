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

import java.awt.Color;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.File;
import javax.swing.*;
import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;
import mediathek.Log;
import mediathek.daten.DDaten;
import mediathek.daten.DatenProg;
import mediathek.daten.DatenPset;
import mediathek.daten.ListePset;
import mediathek.gui.PanelVorlage;
import mediathek.tool.GuiFunktionen;

public class PanelPsetKurz extends PanelVorlage {

    public boolean ok = false;
    public String zielPfad = "";
    private DatenPset pSet;
    private static final String MUSTER_PFAD = "PFAD";
    private ListePset listePset;

    public PanelPsetKurz(DDaten d, ListePset llistePset) {
        super(d);
        initComponents();
        listePset = llistePset;
        jComboBoxPset.setModel(new DefaultComboBoxModel(listePset.getObjectDataCombo()));
        if (listePset.size() > 0) {
            jComboBoxPset.addActionListener(new ActionListener() {

                @Override
                public void actionPerformed(ActionEvent e) {
                    pSet = listePset.get(jComboBoxPset.getSelectedIndex());
                }
            });
            pSet = listePset.getFirst();
            jTextFieldName.setText(pSet.arr[DatenPset.PROGRAMMSET_NAME_NR]);
            jTextFieldName.getDocument().addDocumentListener(new BeobDoc(jTextFieldName, pSet.arr, DatenPset.PROGRAMMSET_NAME_NR));
            jTextArea1.setText(pSet.arr[DatenPset.PROGRAMMSET_BESCHREIBUNG_NR]);
            if (!pSet.istAbspielen() && !pSet.istSpeichern() && pSet.arr[DatenPset.PROGRAMMSET_ZIEL_PFAD_NR].equals("")) {
                // dann Zielpfad ausblenden
                jLabel2.setVisible(false);
                jTextFieldZiel.setVisible(false);
                jButtonZiel.setVisible(false);
            } else {
                // Zielpfad muss gesetzt werden
                if (pSet.arr[DatenPset.PROGRAMMSET_ZIEL_PFAD_NR].contains(MUSTER_PFAD)) {
                    pSet.arr[DatenPset.PROGRAMMSET_ZIEL_PFAD_NR] = pSet.arr[DatenPset.PROGRAMMSET_ZIEL_PFAD_NR].replace(MUSTER_PFAD, "");
                } else if (pSet.arr[DatenPset.PROGRAMMSET_ZIEL_PFAD_NR].equals("")) {
                    pSet.arr[DatenPset.PROGRAMMSET_ZIEL_PFAD_NR] = GuiFunktionen.getHomePath();
                }
                jTextFieldZiel.setText(pSet.arr[DatenPset.PROGRAMMSET_ZIEL_PFAD_NR]);
                jTextFieldZiel.getDocument().addDocumentListener(new BeobDoc(jTextFieldZiel, pSet.arr, DatenPset.PROGRAMMSET_ZIEL_PFAD_NR));
                jButtonZiel.addActionListener(new ZielBeobachter(false, jTextFieldZiel, pSet.arr, DatenPset.PROGRAMMSET_ZIEL_PFAD_NR));
            }
        }
        extra();
    }

    private void extra() {
        GridBagLayout gridbag = new GridBagLayout();
        GridBagConstraints c = new GridBagConstraints();
        c.fill = GridBagConstraints.HORIZONTAL;
        c.insets = new Insets(4, 10, 4, 10);
        c.weightx = 1;
        c.weighty = 0;
        c.gridx = 0;
        c.gridy = 0;
        String name;
        jPanelExtra.setLayout(gridbag);
        for (int i = 0; i < pSet.getListeProg().size(); ++i) {
            DatenProg prog = pSet.getProg(i);
            if (prog.arr[DatenProg.PROGRAMM_PROGRAMMPFAD_NR].contains(MUSTER_PFAD)) {
                name = "Programmpfad";
                JPanel panel = new JPanel();
                panel.setBorder(javax.swing.BorderFactory.createTitledBorder(javax.swing.BorderFactory.createLineBorder(new Color(80, 80, 80), 1), prog.arr[DatenProg.PROGRAMM_NAME_NR]));
                setFeld(panel, name, prog.arr, DatenProg.PROGRAMM_PROGRAMMPFAD_NR);
                gridbag.setConstraints(panel, c);
                jPanelExtra.add(panel);
                ++c.gridy;
            }
//////            if (prog.arr[DatenProg.PROGRAMM_SCHALTER_NR].contains(MUSTER_PFAD)) {
//////                name = "Programmschalter";
//////                setFeld(gridbag, c, ++zeile, name, prog.arr, DatenProg.PROGRAMM_SCHALTER_NR);
//////            }
        }// for
        c.weighty = 10;
        JLabel label = new JLabel();
        gridbag.setConstraints(label, c);
        jPanelExtra.add(label);
    }

    private void setFeld(JPanel panel, String name, String[] arr, int idx) {
        GridBagLayout gridbag = new GridBagLayout();
        GridBagConstraints c = new GridBagConstraints();
        panel.setLayout(gridbag);
        c.fill = GridBagConstraints.HORIZONTAL;
        c.insets = new Insets(4, 10, 4, 10);
        c.weightx = 1;
        c.weighty = 0;
        c.gridx = 0;
        c.gridy = 0;
        arr[idx] = arr[idx].replace(MUSTER_PFAD, "");
        String vorgabe;
        if (arr[idx].contains("[") && arr[idx].contains("]")) {
            vorgabe = arr[idx].substring(arr[idx].indexOf("["), arr[idx].indexOf("]"));
            arr[idx] = vorgabe;
        }
        // Label
        c.gridx = 0;
        c.weightx = 0;
        c.gridwidth = 1;
        JLabel label = new JLabel(name + ": ");
        gridbag.setConstraints(label, c);
        panel.add(label);
        // Textfeld
        c.gridx = 1;
        c.weightx = 10;
        JTextField textField = new JTextField(arr[idx]);
        textField.getDocument().addDocumentListener(new BeobDoc(textField, arr, idx));
        gridbag.setConstraints(textField, c);
        panel.add(textField);
        // Button
        c.gridx = 2;
        c.weightx = 0;
        JButton button = new JButton();
        button.setIcon(new javax.swing.ImageIcon(getClass().getResource("/mediathek/res/fileopen_16.png")));
        button.addActionListener(new ZielBeobachter(false, textField, arr, idx));
        gridbag.setConstraints(button, c);
        panel.add(button);
    }

    private boolean check() {
        ok = false;
        if (jTextFieldName.getText().equals("")) {
            JOptionPane.showMessageDialog(null, "Name ist leer", "Kein Name für das Programmset!", JOptionPane.ERROR_MESSAGE);
            return false;
        }
//        if (zielPfad.equals("")) {
//            JOptionPane.showMessageDialog(null, "Pfad ist leer", "Fehlerhafter Pfad!", JOptionPane.ERROR_MESSAGE);
//        } else {
//            if (!zielIstDatei) {
//                if (GuiFunktionenProgramme.checkPfadBeschreibbar(zielPfad)) {
//                    ok = true;
//                } else {
//                    JOptionPane.showMessageDialog(null, "Pfad ist nicht beschreibbar", "Fehlerhafter Pfad!", JOptionPane.ERROR_MESSAGE);
//                }
//            }
//        }
        return true;
    }

    /** This method is called from within the constructor to
     * initialize the form.
     * WARNING: Do NOT modify this code. The content of this method is
     * always regenerated by the Form Editor.
     */
    // <editor-fold defaultstate="collapsed" desc="Generated Code">//GEN-BEGIN:initComponents
    private void initComponents() {

        jPanel2 = new javax.swing.JPanel();
        jTextField1 = new javax.swing.JTextField();
        jPanelExtra = new javax.swing.JPanel();
        jPanel1 = new javax.swing.JPanel();
        jTextField2 = new javax.swing.JTextField();
        jComboBoxPset = new javax.swing.JComboBox();
        jLabel1 = new javax.swing.JLabel();
        jLabel2 = new javax.swing.JLabel();
        jTextFieldZiel = new javax.swing.JTextField();
        jTextFieldName = new javax.swing.JTextField();
        jButtonZiel = new javax.swing.JButton();
        jScrollPane2 = new javax.swing.JScrollPane();
        jTextArea1 = new javax.swing.JTextArea();

        jPanel2.setBorder(javax.swing.BorderFactory.createLineBorder(new java.awt.Color(153, 153, 255)));

        jTextField1.setBackground(new java.awt.Color(204, 204, 255));
        jTextField1.setEditable(false);
        jTextField1.setHorizontalAlignment(javax.swing.JTextField.CENTER);
        jTextField1.setText("Programme");

        jPanelExtra.setBorder(javax.swing.BorderFactory.createLineBorder(new java.awt.Color(0, 0, 0)));

        javax.swing.GroupLayout jPanelExtraLayout = new javax.swing.GroupLayout(jPanelExtra);
        jPanelExtra.setLayout(jPanelExtraLayout);
        jPanelExtraLayout.setHorizontalGroup(
            jPanelExtraLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGap(0, 0, Short.MAX_VALUE)
        );
        jPanelExtraLayout.setVerticalGroup(
            jPanelExtraLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGap(0, 81, Short.MAX_VALUE)
        );

        javax.swing.GroupLayout jPanel2Layout = new javax.swing.GroupLayout(jPanel2);
        jPanel2.setLayout(jPanel2Layout);
        jPanel2Layout.setHorizontalGroup(
            jPanel2Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel2Layout.createSequentialGroup()
                .addContainerGap()
                .addGroup(jPanel2Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addComponent(jTextField1)
                    .addComponent(jPanelExtra, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
                .addContainerGap())
        );
        jPanel2Layout.setVerticalGroup(
            jPanel2Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel2Layout.createSequentialGroup()
                .addContainerGap()
                .addComponent(jTextField1, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                .addComponent(jPanelExtra, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                .addContainerGap())
        );

        jPanel1.setBorder(javax.swing.BorderFactory.createLineBorder(new java.awt.Color(153, 153, 255)));

        jTextField2.setBackground(new java.awt.Color(204, 204, 255));
        jTextField2.setEditable(false);
        jTextField2.setFont(new java.awt.Font("Dialog", 1, 14)); // NOI18N
        jTextField2.setHorizontalAlignment(javax.swing.JTextField.CENTER);
        jTextField2.setText("Programmset");

        jComboBoxPset.setModel(new javax.swing.DefaultComboBoxModel(new String[] { "Item 1", "Item 2", "Item 3", "Item 4" }));

        jLabel1.setText("Setname:");

        jLabel2.setText("Zielpfad:");

        jButtonZiel.setIcon(new javax.swing.ImageIcon(getClass().getResource("/mediathek/res/fileopen_16.png"))); // NOI18N

        jTextArea1.setColumns(20);
        jTextArea1.setEditable(false);
        jTextArea1.setRows(8);
        jScrollPane2.setViewportView(jTextArea1);

        javax.swing.GroupLayout jPanel1Layout = new javax.swing.GroupLayout(jPanel1);
        jPanel1.setLayout(jPanel1Layout);
        jPanel1Layout.setHorizontalGroup(
            jPanel1Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel1Layout.createSequentialGroup()
                .addContainerGap()
                .addGroup(jPanel1Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addComponent(jTextField2)
                    .addComponent(jComboBoxPset, 0, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                    .addGroup(jPanel1Layout.createSequentialGroup()
                        .addGroup(jPanel1Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                            .addComponent(jLabel1)
                            .addComponent(jLabel2))
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                        .addGroup(jPanel1Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                            .addGroup(jPanel1Layout.createSequentialGroup()
                                .addComponent(jTextFieldZiel)
                                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                                .addComponent(jButtonZiel))
                            .addComponent(jTextFieldName)))
                    .addComponent(jScrollPane2, javax.swing.GroupLayout.DEFAULT_SIZE, 452, Short.MAX_VALUE))
                .addContainerGap())
        );
        jPanel1Layout.setVerticalGroup(
            jPanel1Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel1Layout.createSequentialGroup()
                .addContainerGap()
                .addComponent(jTextField2, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addGap(12, 12, 12)
                .addComponent(jComboBoxPset, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addGroup(jPanel1Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(jLabel1)
                    .addComponent(jTextFieldName, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addGroup(jPanel1Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(jLabel2)
                    .addComponent(jTextFieldZiel, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                    .addComponent(jButtonZiel))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(jScrollPane2, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addContainerGap(javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
        );

        jPanel1Layout.linkSize(javax.swing.SwingConstants.VERTICAL, new java.awt.Component[] {jButtonZiel, jTextFieldName, jTextFieldZiel});

        javax.swing.GroupLayout layout = new javax.swing.GroupLayout(this);
        this.setLayout(layout);
        layout.setHorizontalGroup(
            layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(layout.createSequentialGroup()
                .addContainerGap()
                .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addComponent(jPanel1, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                    .addComponent(jPanel2, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
                .addContainerGap())
        );
        layout.setVerticalGroup(
            layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(layout.createSequentialGroup()
                .addContainerGap()
                .addComponent(jPanel1, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                .addComponent(jPanel2, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                .addContainerGap())
        );
    }// </editor-fold>//GEN-END:initComponents
    // Variables declaration - do not modify//GEN-BEGIN:variables
    private javax.swing.JButton jButtonZiel;
    private javax.swing.JComboBox jComboBoxPset;
    private javax.swing.JLabel jLabel1;
    private javax.swing.JLabel jLabel2;
    private javax.swing.JPanel jPanel1;
    private javax.swing.JPanel jPanel2;
    private javax.swing.JPanel jPanelExtra;
    private javax.swing.JScrollPane jScrollPane2;
    private javax.swing.JTextArea jTextArea1;
    private javax.swing.JTextField jTextField1;
    private javax.swing.JTextField jTextField2;
    private javax.swing.JTextField jTextFieldName;
    private javax.swing.JTextField jTextFieldZiel;
    // End of variables declaration//GEN-END:variables

    private class ZielBeobachter implements ActionListener {

        JTextField textField;
        boolean file;
        String[] arr;
        int idx;

        public ZielBeobachter(boolean ffile, JTextField tt, String[] aarr, int iidx) {
            file = ffile;
            textField = tt;
            arr = aarr;
            idx = iidx;
        }

        @Override
        public void actionPerformed(ActionEvent e) {
            int returnVal;
            JFileChooser chooser = new JFileChooser();
            chooser.setCurrentDirectory(new File(textField.getText()));
            if (file) {
                chooser.setFileSelectionMode(JFileChooser.FILES_ONLY);
            } else {
                chooser.setFileSelectionMode(JFileChooser.DIRECTORIES_ONLY);
            }
            returnVal = chooser.showOpenDialog(null);
            if (returnVal == JFileChooser.APPROVE_OPTION) {
                try {
                    textField.setText(chooser.getSelectedFile().getAbsolutePath());
                    arr[idx] = textField.getText();
                } catch (Exception ex) {
                    Log.fehlerMeldung("DialogZielPset.ZielBeobachter", ex);
                }
            }
        }
    }

    private class BeobDoc implements DocumentListener {

        JTextField textField;
        String[] arr;
        int idx;

        public BeobDoc(JTextField tt, String[] aarr, int iidx) {
            textField = tt;
            arr = aarr;
            idx = iidx;
        }

        @Override
        public void insertUpdate(DocumentEvent e) {
            set();
        }

        @Override
        public void removeUpdate(DocumentEvent e) {
            set();
        }

        @Override
        public void changedUpdate(DocumentEvent e) {
            set();
        }

        private void set() {
            arr[idx] = textField.getText();
        }
    }
}
