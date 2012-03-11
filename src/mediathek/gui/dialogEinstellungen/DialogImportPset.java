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
import mediathek.daten.DatenProg;
import mediathek.daten.DatenPset;
import mediathek.gui.beobachter.EscBeenden;
import mediathek.tool.GuiFunktionen;

public class DialogImportPset extends javax.swing.JDialog {

    public boolean ok = false;
    public String zielPfad = "";
    private DatenPset pSet;
    private static final String MUSTER_PFAD = "PFAD";

    public DialogImportPset(java.awt.Frame parent, boolean modal, DatenPset ps) {
        super(parent, modal);
        initComponents();
        pSet = ps;
        jButtonOk.addActionListener(new OkBeobachter());
        new EscBeenden(this) {

            @Override
            public void beenden_() {
                ok = false;
                beenden();
            }
        };
        jTextFieldName.setText(ps.arr[DatenPset.PROGRAMMSET_NAME_NR]);
        jTextFieldName.getDocument().addDocumentListener(new BeobDoc(jTextFieldName, pSet.arr, DatenPset.PROGRAMMSET_NAME_NR));
        extra();
    }

    private void extra() {
        GridBagLayout gridbag = new GridBagLayout();
        GridBagConstraints c = new GridBagConstraints();
        c.weightx = 0;
        c.fill = GridBagConstraints.HORIZONTAL;
        c.insets = new Insets(4, 10, 4, 10);
        c.gridy = 0;
        int zeile = 0;
        String name;
        jPanelExtra.setLayout(gridbag);
        if (pSet.arr[DatenPset.PROGRAMMSET_ZIEL_PFAD_NR].contains(MUSTER_PFAD)) {
            zeile = setFeld(gridbag, c, zeile, "Zielpfad", pSet.arr, DatenPset.PROGRAMMSET_ZIEL_PFAD_NR);
        }
        for (int i = 0; i < pSet.getListeProg().size(); ++i) {
            DatenProg prog = pSet.getProg(i);
            if (prog.arr[DatenProg.PROGRAMM_PROGRAMMPFAD_NR].contains(MUSTER_PFAD)) {
                name = prog.arr[DatenProg.PROGRAMM_NAME_NR];
                zeile = setFeld(gridbag, c, zeile, name, prog.arr, DatenProg.PROGRAMM_PROGRAMMPFAD_NR);
            }
//////            if (prog.arr[DatenProg.PROGRAMM_SCHALTER_NR].contains(MUSTER_PFAD)) {
//////                name = "Programmschalter";
//////                setFeld(gridbag, c, ++zeile, name, prog.arr, DatenProg.PROGRAMM_SCHALTER_NR);
//////            }
        }// for
        c.weighty = 10;
        c.gridx = 1;
        c.gridy = zeile;
        JLabel label = new JLabel();
        gridbag.setConstraints(label, c);
        jPanelExtra.add(label);
    }

    private int setFeld(GridBagLayout gridbag, GridBagConstraints c, int zeile, String name, String[] arr, int idx) {
        arr[idx] = arr[idx].replace(MUSTER_PFAD, "");
        String vorgabe = "";
        String text = "";
        if (arr[idx].contains("]")) {
            vorgabe = arr[idx].substring(1, arr[idx].indexOf("]"));
            arr[idx] = arr[idx].substring(arr[idx].indexOf("]"));
            arr[idx] = arr[idx].replaceFirst("]", "");
        }
        if (arr[idx].contains("]")) {
            text = arr[idx].substring(1, arr[idx].indexOf("]"));
        }
        if (!vorgabe.equals("")) {
            arr[idx] = vorgabe;
        }
        if (!text.equals("")) {
            c.gridy = zeile;
            zeile += 4;
            c.gridx = 0;
            c.weightx = 1;
            c.gridwidth = 3;
            c.gridheight = 4;
            JTextArea area = new JTextArea(text);
            area.setRows(4);
            gridbag.setConstraints(area, c);
            jPanelExtra.add(area);
        }
        c.gridwidth = 1;
        c.gridheight = 1;
        c.gridy = zeile;
        JLabel label = new JLabel(name + ": ");
        gridbag.setConstraints(label, c);
        jPanelExtra.add(label);
        c.gridx = 1;
        c.weightx = 10;
        JTextField textField = new JTextField(arr[idx]);
        textField.getDocument().addDocumentListener(new BeobDoc(textField, arr, idx));
        gridbag.setConstraints(textField, c);
        jPanelExtra.add(textField);
        c.gridx = 2;
        c.weightx = 1;
        JButton button = new JButton();
        button.setIcon(new javax.swing.ImageIcon(getClass().getResource("/mediathek/res/fileopen_16.png")));
        button.addActionListener(new ZielBeobachter(false, textField, arr, idx));
        gridbag.setConstraints(button, c);
        jPanelExtra.add(button);
        return ++zeile;
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

    private String setPfad(String pfad) {
        String ret = "";
        if (pfad.equals("")) {
            ret = GuiFunktionen.getHomePath();
        } else {
            ret = pfad;
        }
        return ret;
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

        jButtonOk = new javax.swing.JButton();
        jTextFieldName = new javax.swing.JTextField();
        jLabel1 = new javax.swing.JLabel();
        jScrollPane1 = new javax.swing.JScrollPane();
        jPanelExtra = new javax.swing.JPanel();

        setDefaultCloseOperation(javax.swing.WindowConstants.DISPOSE_ON_CLOSE);

        jButtonOk.setText("Ok");

        jLabel1.setText("Setname:");

        jPanelExtra.setBorder(new javax.swing.border.SoftBevelBorder(javax.swing.border.BevelBorder.RAISED));

        javax.swing.GroupLayout jPanelExtraLayout = new javax.swing.GroupLayout(jPanelExtra);
        jPanelExtra.setLayout(jPanelExtraLayout);
        jPanelExtraLayout.setHorizontalGroup(
            jPanelExtraLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGap(0, 412, Short.MAX_VALUE)
        );
        jPanelExtraLayout.setVerticalGroup(
            jPanelExtraLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGap(0, 395, Short.MAX_VALUE)
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
                    .addGroup(layout.createSequentialGroup()
                        .addGap(0, 0, Short.MAX_VALUE)
                        .addComponent(jButtonOk, javax.swing.GroupLayout.PREFERRED_SIZE, 118, javax.swing.GroupLayout.PREFERRED_SIZE))
                    .addGroup(layout.createSequentialGroup()
                        .addComponent(jLabel1)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                        .addComponent(jTextFieldName)))
                .addContainerGap())
        );
        layout.setVerticalGroup(
            layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(layout.createSequentialGroup()
                .addContainerGap()
                .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(jTextFieldName, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                    .addComponent(jLabel1))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                .addComponent(jScrollPane1)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                .addComponent(jButtonOk)
                .addContainerGap())
        );

        layout.linkSize(javax.swing.SwingConstants.VERTICAL, new java.awt.Component[] {jButtonOk, jTextFieldName});

        pack();
    }// </editor-fold>//GEN-END:initComponents
    /**
     * @param args the command line arguments
     */
    // Variables declaration - do not modify//GEN-BEGIN:variables
    private javax.swing.JButton jButtonOk;
    private javax.swing.JLabel jLabel1;
    private javax.swing.JPanel jPanelExtra;
    private javax.swing.JScrollPane jScrollPane1;
    private javax.swing.JTextField jTextFieldName;
    // End of variables declaration//GEN-END:variables

    private class OkBeobachter implements ActionListener {

        @Override
        public void actionPerformed(ActionEvent e) {
            if (check()) {
                beenden();
            }
        }
    }

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
