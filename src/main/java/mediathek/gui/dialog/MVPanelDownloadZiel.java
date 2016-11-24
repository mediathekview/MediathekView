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
import mSearch.tool.FilenameUtils;
import mSearch.tool.Log;
import mediathek.config.Icons;
import mediathek.config.MVColor;
import mediathek.config.MVConfig;
import mediathek.daten.DatenDownload;
import mediathek.tool.GuiFunktionen;
import mediathek.tool.MVMessageDialog;

import javax.swing.*;
import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;
import javax.swing.text.JTextComponent;
import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.File;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;

@SuppressWarnings("serial")
public class MVPanelDownloadZiel extends JPanel {
    public boolean nameGeaendert = false;
    private final DatenDownload datenDownload;
    private JFrame parent = null;
    boolean letztenPfadAnzeigen = false;

    public MVPanelDownloadZiel(JFrame p, DatenDownload download, boolean letzterPfad) {
        initComponents();
        parent = p;
        datenDownload = download;
        letztenPfadAnzeigen = letzterPfad;
        jButtonPath.setIcon(Icons.ICON_BUTTON_FILE_OPEN);
        jButtonDelPath.setIcon(Icons.ICON_BUTTON_DEL);
         jLabelExists.setText("");
        jButtonPath.addActionListener(new ZielBeobachter());
        jButtonDelPath.addActionListener(e -> {
            MVConfig.add(MVConfig.Configs.SYSTEM_DIALOG_DOWNLOAD__PFADE_ZUM_SPEICHERN, "");
            jComboBoxPath.setModel(new DefaultComboBoxModel<>(new String[]{datenDownload.arr[DatenDownload.DOWNLOAD_ZIEL_PFAD]}));
        });
        jTextFieldName.setText(datenDownload.arr[DatenDownload.DOWNLOAD_ZIEL_DATEINAME]);
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
        setModelPfad(datenDownload.arr[DatenDownload.DOWNLOAD_ZIEL_PFAD]);
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
        if (!MVConfig.get(MVConfig.Configs.SYSTEM_DIALOG_DOWNLOAD__PFADE_ZUM_SPEICHERN).isEmpty()) {
            String[] p = MVConfig.get(MVConfig.Configs.SYSTEM_DIALOG_DOWNLOAD__PFADE_ZUM_SPEICHERN).split("<>");
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
            } else if (!jTextFieldName.getText().equals(datenDownload.arr[DatenDownload.DOWNLOAD_ZIEL_DATEINAME])
                    || !(((JTextComponent) jComboBoxPath.getEditor().getEditorComponent()).getText()).equals(datenDownload.arr[DatenDownload.DOWNLOAD_ZIEL_PFAD])) {
                jLabelExists.setForeground(MVColor.DOWNLOAD_DATEINAME_NEU.color);
                jLabelExists.setText("Neuer Name");
            } else {
                jLabelExists.setForeground(MVColor.DOWNLOAD_DATEINAME_ALT.color);
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
            name = new SimpleDateFormat("yyyyMMdd").format(new Date()) + "_" + datenDownload.arr[DatenDownload.DOWNLOAD_THEMA] + "-" + datenDownload.arr[DatenDownload.DOWNLOAD_TITEL] + ".mp4";
        }
        String[] pathName = {pfad, name};
        GuiFunktionen.checkLengthPath(pathName);
        if (!pathName[0].equals(pfad) || !pathName[1].equals(name)) {
            MVMessageDialog.showMessageDialog(parent, "Dateiname war zu lang und wurde gekürzt!",
                    "Pfad zu lang!", JOptionPane.ERROR_MESSAGE);
            pfad = pathName[0];
            name = pathName[1];
        }
        String orgPfad = datenDownload.arr[DatenDownload.DOWNLOAD_ZIEL_PFAD_DATEINAME];
        //##############################################
        datenDownload.arr[DatenDownload.DOWNLOAD_ZIEL_DATEINAME] = name;
        datenDownload.arr[DatenDownload.DOWNLOAD_ZIEL_PFAD] = pfad;
        datenDownload.arr[DatenDownload.DOWNLOAD_ZIEL_PFAD_DATEINAME] = GuiFunktionen.addsPfad(pfad, name);
        return !orgPfad.equals(datenDownload.arr[DatenDownload.DOWNLOAD_ZIEL_PFAD_DATEINAME]);
    }

    // <editor-fold defaultstate="collapsed" desc="Generated Code">//GEN-BEGIN:initComponents
    private void initComponents() {

        javax.swing.JLabel jLabel1 = new javax.swing.JLabel();
        jComboBoxPath = new javax.swing.JComboBox<>();
        jButtonPath = new javax.swing.JButton();
        jButtonDelPath = new javax.swing.JButton();
        javax.swing.JLabel jLabel2 = new javax.swing.JLabel();
        jTextFieldName = new javax.swing.JTextField();
        jLabelExists = new javax.swing.JLabel();

        jLabel1.setText("Zielpfad:");

        jComboBoxPath.setEditable(true);

        jButtonPath.setIcon(new javax.swing.ImageIcon(getClass().getResource("/mediathek/res/muster/button-file-open.png"))); // NOI18N
        jButtonPath.setToolTipText("Zielpfad auswählen");

        jButtonDelPath.setIcon(new javax.swing.ImageIcon(getClass().getResource("/mediathek/res/muster/button-del.png"))); // NOI18N
        jButtonDelPath.setToolTipText("gespeicherte Pfade löschen");

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
                                .addComponent(jComboBoxPath, 0, 445, Short.MAX_VALUE)
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
                        Log.errorLog(356871087, ex);
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
                        Log.errorLog(356871087, ex);
                    }
                }
            }
        }
    }

}
