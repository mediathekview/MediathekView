/*
 * MediathekView
 * Copyright (C) 2014 W. Xaver
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
package mediathek.gui.dialog;

import com.jidesoft.utils.SystemInfo;
import java.awt.Color;
import java.awt.FileDialog;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.File;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import javax.swing.DefaultComboBoxModel;
import javax.swing.JFileChooser;
import javax.swing.JFrame;
import javax.swing.JOptionPane;
import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;
import javax.swing.text.JTextComponent;
import mediathek.controller.Log;
import mediathek.daten.Daten;
import mediathek.daten.DatenDownload;
import mediathek.res.GetIcon;
import mediathek.tool.FilenameUtils;
import mediathek.tool.GuiFunktionen;
import mediathek.tool.MVColor;
import mediathek.tool.MVConfig;
import mediathek.tool.MVMessageDialog;

public class MVPanelDownloadZiel extends javax.swing.JPanel {

    public boolean nameGeaendert = false;
    private DatenDownload datenDownload;
    private JFrame parent = null;
    boolean letztenPfadAnzeigen = false;

    public MVPanelDownloadZiel(JFrame p, DatenDownload download, boolean letzterPfad) {
        initComponents();
        parent = p;
        datenDownload = download;
        letztenPfadAnzeigen = letzterPfad;
        jButtonPath.setIcon(GetIcon.getProgramIcon("fileopen_16.png"));
        jButtonDelPath.setIcon(GetIcon.getProgramIcon("del_16.png"));
        jLabelExists.setText("");
//        jCheckBoxPath.setSelected(Boolean.parseBoolean(Daten.mVConfig.get(MVConfig.SYSTEM_DIALOG_DOWNLOAD__LETZTEN_PFAD_ANZEIGEN)));
//        jCheckBoxPath.addActionListener(new ActionListener() {
//            @Override
//            public void actionPerformed(ActionEvent e) {
//                Daten.mVConfig.add(MVConfig.SYSTEM_DIALOG_DOWNLOAD__LETZTEN_PFAD_ANZEIGEN, Boolean.toString(jCheckBoxPath.isSelected()));
//            }
//        });
        jButtonPath.addActionListener(new ZielBeobachter());
        jButtonDelPath.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                Daten.mVConfig.add(MVConfig.SYSTEM_DIALOG_DOWNLOAD__PFADE_ZUM_SPEICHERN, "");
                jComboBoxPath.setModel(new DefaultComboBoxModel<>(new String[]{datenDownload.arr[DatenDownload.DOWNLOAD_ZIEL_PFAD_NR]}));
            }
        });
        jTextFieldName.setText(datenDownload.arr[DatenDownload.DOWNLOAD_ZIEL_DATEINAME_NR]);
        jTextFieldName.getDocument().addDocumentListener(new DocumentListener() {

            @Override
            public void insertUpdate(DocumentEvent e) {
                tus();
            }

            @Override
            public void removeUpdate(DocumentEvent e) {
                tus();
            }

            @Override
            public void changedUpdate(DocumentEvent e) {
                tus();
            }

            private void tus() {
                nameGeaendert = true;
                checkPfadName();
                final String n1 = jTextFieldName.getText();
                final String n2 = FilenameUtils.checkDateiname(n1, false /*pfad*/);
                if (!n1.equals(n2)) {
                    jTextFieldName.setBackground(MVColor.DOWNLOAD_FEHLER.color);
                } else {
                    jTextFieldName.setBackground(javax.swing.UIManager.getDefaults().getColor("TextField.background"));
                }
            }
        });
        setModelPfad(datenDownload.arr[DatenDownload.DOWNLOAD_ZIEL_PFAD_NR]);
        ((JTextComponent) jComboBoxPath.getEditor().getEditorComponent()).setOpaque(true);
        ((JTextComponent) jComboBoxPath.getEditor().getEditorComponent()).getDocument().addDocumentListener(new DocumentListener() {

            @Override
            public void insertUpdate(DocumentEvent e) {
                tus();
            }

            @Override
            public void removeUpdate(DocumentEvent e) {
                tus();
            }

            @Override
            public void changedUpdate(DocumentEvent e) {
                tus();
            }

            private void tus() {
                nameGeaendert = true;
                checkPfadName();
                String s = ((JTextComponent) jComboBoxPath.getEditor().getEditorComponent()).getText();
                if (!s.equals(FilenameUtils.checkDateiname(s, true /*pfad*/))) {
                    jComboBoxPath.getEditor().getEditorComponent().setBackground(MVColor.DOWNLOAD_FEHLER.color);
                } else {
                    jComboBoxPath.getEditor().getEditorComponent().setBackground(Color.WHITE);
                }
            }
        });
        checkPfadName();
    }

    private void setModelPfad(String pfad) {
        ArrayList<String> pfade = new ArrayList<>();
        // wenn gewünscht, den letzten verwendeten Pfad an den Anfang setzen
        if (!letztenPfadAnzeigen && !pfad.isEmpty()) {
            // dann kommt der Pfad des Sets an den Anfang
            pfade.add(pfad);
        }
        if (!Daten.mVConfig.get(MVConfig.SYSTEM_DIALOG_DOWNLOAD__PFADE_ZUM_SPEICHERN).isEmpty()) {
            String[] p = Daten.mVConfig.get(MVConfig.SYSTEM_DIALOG_DOWNLOAD__PFADE_ZUM_SPEICHERN).split("<>");
            for (String s : p) {
                if (!pfade.contains(s)) {
                    pfade.add(s);
                }
            }
        }

        if (letztenPfadAnzeigen && !pfad.isEmpty()) {
            // dann kommt der Pfad des Sets an den Schluss
            if (!pfade.contains(pfad)) {
                pfade.add(pfad);
            }
        }
        jComboBoxPath.setModel(new DefaultComboBoxModel<>(pfade.toArray(new String[pfade.size()])));
    }

// nur die Pfade beim Start manueller Downloads speichern    
//    public void saveComboPfad() {
//        ArrayList<String> pfade = new ArrayList<>();
//        String s = jComboBoxPath.getSelectedItem().toString();
//        if (!s.equals(orgPfad) || jCheckBoxPath.isSelected()) {
//            pfade.add(s);
//        }
//        for (int i = 0; i < jComboBoxPath.getItemCount(); ++i) {
//            s = jComboBoxPath.getItemAt(i);
//            if (!s.equals(orgPfad) && !pfade.contains(s)) {
//                pfade.add(s);
//            }
//        }
//        if (!pfade.isEmpty()) {
//            s = pfade.get(0);
//            for (int i = 1; i < Konstanten.MAX_PFADE_DIALOG_DOWNLOAD && i < pfade.size(); ++i) {
//                if (!pfade.get(i).isEmpty()) {
//                    s += "<>" + pfade.get(i);
//                }
//            }
//        }
//        Daten.mVConfig.add(MVConfig.SYSTEM_DIALOG_DOWNLOAD__PFADE_ZUM_SPEICHERN, s);
//    }
    private void checkPfadName() {
        String pfad = ((JTextComponent) jComboBoxPath.getEditor().getEditorComponent()).getText();
        String name = jTextFieldName.getText();
        String p;
        if (pfad.endsWith(File.separator)) {
            p = pfad.substring(0, pfad.length() - 1);
        } else {
            p = pfad;
        }
        String pfadName = GuiFunktionen.concatPaths(p, name);
        try {
            File file = new File(pfadName);
            if (file.exists()) {
                jLabelExists.setForeground(MVColor.DOWNLOAD_DATEINAME_EXISTIERT.color);
                jLabelExists.setText("Datei existiert schon!");
            } else if (!jTextFieldName.getText().equals(datenDownload.arr[DatenDownload.DOWNLOAD_ZIEL_DATEINAME_NR])
                    || !(((JTextComponent) jComboBoxPath.getEditor().getEditorComponent()).getText()).equals(datenDownload.arr[DatenDownload.DOWNLOAD_ZIEL_PFAD_NR])) {
                jLabelExists.setForeground(MVColor.DOWNLOAD_DATEINAME_NEU.color);
                jLabelExists.setText("Neuer Name");
            } else {
                jLabelExists.setForeground(MVColor.DOWNLOAD_DATEINAME_ALT.color);
//                jLabelExists.setText("Alter Name");
                jLabelExists.setText("");
            }
        } catch (Exception ignored) {
        }
    }

    public boolean setPfadName_geaendert() {
        // setzt den neuen Namen und liefert, ob er sich geändert hat
        String pfad = jComboBoxPath.getSelectedItem().toString();
        String name = jTextFieldName.getText();
        if (pfad.endsWith(File.separator)) {
            pfad = pfad.substring(0, pfad.length() - 1);
        }
        //##############################################
        // zur Sicherheit bei Unsinn im Set
        if (pfad.equals("")) {
            pfad = GuiFunktionen.getStandardDownloadPath();
        }
        if (name.equals("")) {
            name = new SimpleDateFormat("yyyyMMdd").format(new Date()) + "_" + datenDownload.arr[DatenDownload.DOWNLOAD_THEMA_NR] + "-" + datenDownload.arr[DatenDownload.DOWNLOAD_TITEL_NR] + ".mp4";
        }
        String[] pathName = {pfad, name};
        GuiFunktionen.checkLengthPath(pathName);
        if (!pathName[0].equals(pfad) || !pathName[1].equals(name)) {
            MVMessageDialog.showMessageDialog(parent, "Dateiname war zu lang und wurde gekürzt!",
                    "Pfad zu lang!", JOptionPane.ERROR_MESSAGE);
            pfad = pathName[0];
            name = pathName[1];
        }
        String orgPfad = datenDownload.arr[DatenDownload.DOWNLOAD_ZIEL_PFAD_DATEINAME_NR];
        //##############################################
        datenDownload.arr[DatenDownload.DOWNLOAD_ZIEL_DATEINAME_NR] = name;
        datenDownload.arr[DatenDownload.DOWNLOAD_ZIEL_PFAD_NR] = pfad;
        datenDownload.arr[DatenDownload.DOWNLOAD_ZIEL_PFAD_DATEINAME_NR] = GuiFunktionen.addsPfad(pfad, name);
        return !orgPfad.equals(datenDownload.arr[DatenDownload.DOWNLOAD_ZIEL_PFAD_DATEINAME_NR]);
    }

    // <editor-fold defaultstate="collapsed" desc="Generated Code">//GEN-BEGIN:initComponents
    private void initComponents() {

        javax.swing.JLabel jLabel1 = new javax.swing.JLabel();
        jComboBoxPath = new javax.swing.JComboBox<String>();
        jButtonPath = new javax.swing.JButton();
        jButtonDelPath = new javax.swing.JButton();
        javax.swing.JLabel jLabel2 = new javax.swing.JLabel();
        jTextFieldName = new javax.swing.JTextField();
        jLabelExists = new javax.swing.JLabel();

        jLabel1.setText("Zielpfad:");

        jComboBoxPath.setEditable(true);

        jButtonPath.setIcon(new javax.swing.ImageIcon(getClass().getResource("/mediathek/res/programm/fileopen_16.png"))); // NOI18N

        jButtonDelPath.setIcon(new javax.swing.ImageIcon(getClass().getResource("/mediathek/res/programm/del_16.png"))); // NOI18N

        jLabel2.setText("Dateiname:");

        jLabelExists.setText("Datei existiert schon!");

        javax.swing.GroupLayout layout = new javax.swing.GroupLayout(this);
        this.setLayout(layout);
        layout.setHorizontalGroup(
            layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(javax.swing.GroupLayout.Alignment.TRAILING, layout.createSequentialGroup()
                .addContainerGap()
                .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.TRAILING)
                    .addGroup(layout.createSequentialGroup()
                        .addComponent(jLabelExists)
                        .addGap(0, 0, Short.MAX_VALUE))
                    .addGroup(javax.swing.GroupLayout.Alignment.LEADING, layout.createSequentialGroup()
                        .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                            .addComponent(jLabel2)
                            .addComponent(jLabel1))
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                        .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                            .addGroup(layout.createSequentialGroup()
                                .addComponent(jComboBoxPath, 0, 429, Short.MAX_VALUE)
                                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                                .addComponent(jButtonPath)
                                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                                .addComponent(jButtonDelPath))
                            .addComponent(jTextFieldName))))
                .addContainerGap())
        );
        layout.setVerticalGroup(
            layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(layout.createSequentialGroup()
                .addContainerGap()
                .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(jLabel1)
                    .addComponent(jComboBoxPath, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                    .addComponent(jButtonPath)
                    .addComponent(jButtonDelPath))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(jLabel2)
                    .addComponent(jTextFieldName, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE))
                .addGap(10, 10, 10)
                .addComponent(jLabelExists)
                .addContainerGap(javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
        );

        layout.linkSize(javax.swing.SwingConstants.VERTICAL, new java.awt.Component[] {jButtonDelPath, jButtonPath, jComboBoxPath, jTextFieldName});

    }// </editor-fold>//GEN-END:initComponents


    // Variables declaration - do not modify//GEN-BEGIN:variables
    private javax.swing.JButton jButtonDelPath;
    private javax.swing.JButton jButtonPath;
    private javax.swing.JComboBox<String> jComboBoxPath;
    private javax.swing.JLabel jLabelExists;
    private javax.swing.JTextField jTextFieldName;
    // End of variables declaration//GEN-END:variables

    private class ZielBeobachter implements ActionListener {

        @Override
        public void actionPerformed(ActionEvent e) {
            //we can use native directory chooser on Mac...
            if (SystemInfo.isMacOSX()) {
                //we want to select a directory only, so temporarily change properties
                System.setProperty("apple.awt.fileDialogForDirectories", "true");
                FileDialog chooser = new FileDialog(parent, "Film speichern");
                chooser.setVisible(true);
                if (chooser.getFile() != null) {
                    //A directory was selected, that means Cancel was not pressed
                    try {
                        jComboBoxPath.addItem(chooser.getDirectory() + chooser.getFile());
                        jComboBoxPath.setSelectedItem(chooser.getDirectory() + chooser.getFile());
                    } catch (Exception ex) {
                        Log.fehlerMeldung(356871087, ex);
                    }
                }
                System.setProperty("apple.awt.fileDialogForDirectories", "false");
            } else {
                //use the cross-platform swing chooser
                int returnVal;
                JFileChooser chooser = new JFileChooser();
                if (!jComboBoxPath.getSelectedItem().toString().equals("")) {
                    chooser.setCurrentDirectory(new File(jComboBoxPath.getSelectedItem().toString()));
                }
                chooser.setFileSelectionMode(JFileChooser.DIRECTORIES_ONLY);
                returnVal = chooser.showOpenDialog(null);
                if (returnVal == JFileChooser.APPROVE_OPTION) {
                    try {
                        jComboBoxPath.addItem(chooser.getSelectedFile().getAbsolutePath());
                        jComboBoxPath.setSelectedItem(chooser.getSelectedFile().getAbsolutePath());
                    } catch (Exception ex) {
                        Log.fehlerMeldung(356871087, ex);
                    }
                }
            }
        }
    }

}
