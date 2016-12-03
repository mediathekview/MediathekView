/*
 * MediathekView
 * Copyright (C) 2008 W. Xaver
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
package mediathek.gui.dialogEinstellungen;

import com.jidesoft.utils.SystemInfo;
import mSearch.tool.Listener;
import mSearch.tool.Log;
import mediathek.config.*;
import mediathek.gui.PanelVorlage;
import mediathek.tool.GuiFunktionen;
import mediathek.tool.TextCopyPaste;

import javax.swing.*;
import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;
import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.File;

@SuppressWarnings("serial")
public class PanelFilmlisteLaden extends PanelVorlage {
    public PanelFilmlisteLaden(Daten d, JFrame parentComponent) {
        super(d, parentComponent);
        initComponents();
        init();
    }

    private void init() {
        jButtonDateiAuswaehlen.setIcon(Icons.ICON_BUTTON_FILE_OPEN);
        jButtonUrl.setIcon(Icons.ICON_BUTTON_AKTUALISIEREN);
        initRadio();
        jButtonUrl.addActionListener(e -> jTextFieldUrl.setText(daten.getFilmeLaden().getDownloadUrl_akt()));
        jButtonLoad.addActionListener(ae -> daten.getFilmeLaden().loadFilmlist(""));
        jButtonDateiAuswaehlen.addActionListener(new BeobPfad());
        jButtonFilmeLaden.addActionListener(e -> {
            if (jCheckBoxUpdate.isSelected()) {
                daten.getFilmeLaden().updateFilmlist(jTextFieldUrl.getText());
            } else {
                daten.getFilmeLaden().loadFilmlist(jTextFieldUrl.getText());
            }
        });
        jRadioButtonManuell.addActionListener(new BeobOption());
        jRadioButtonAuto.addActionListener(new BeobOption());
        jTextFieldUrl.getDocument().addDocumentListener(new BeobDateiUrl());
        jTextFieldUrl.addMouseListener(new TextCopyPaste());
        Listener.addListener(new Listener(Listener.EREIGNIS_ART_IMPORT_FILMLISTE, PanelFilmlisteLaden.class.getSimpleName()) {
            @Override
            public void ping() {
                initRadio();
            }
        });
    }

    private void initRadio() {
        if (GuiFunktionen.getImportArtFilme() == Konstanten.UPDATE_FILME_AUS) {
            jRadioButtonManuell.setSelected(true);
        } else {
            jRadioButtonAuto.setSelected(true);
        }
        jTextFieldUrl.setText(MVConfig.get(MVConfig.Configs.SYSTEM_IMPORT_URL_MANUELL));
        setPanelTabelle(jRadioButtonManuell.isSelected());
    }

    private void setPanelTabelle(boolean manuell) {
        if (manuell) {
            jTextAreaManuell.setBackground(MVColor.FILMLISTE_LADEN_AKTIV.color);
            jTextAreaAuto.setBackground(null);
        } else {
            jTextAreaManuell.setBackground(null);
            jTextAreaAuto.setBackground(MVColor.FILMLISTE_LADEN_AKTIV.color);
        }
    }

    // <editor-fold defaultstate="collapsed" desc="Generated Code">//GEN-BEGIN:initComponents
    private void initComponents() {

        javax.swing.ButtonGroup buttonGroup1 = new javax.swing.ButtonGroup();
        javax.swing.JPanel jPanelAuto = new javax.swing.JPanel();
        javax.swing.JScrollPane jScrollPane2 = new javax.swing.JScrollPane();
        jTextAreaAuto = new javax.swing.JTextArea();
        jButtonLoad = new javax.swing.JButton();
        javax.swing.JPanel jPanelManuel = new javax.swing.JPanel();
        javax.swing.JLabel jLabel1 = new javax.swing.JLabel();
        jTextFieldUrl = new javax.swing.JTextField();
        jButtonDateiAuswaehlen = new javax.swing.JButton();
        jButtonFilmeLaden = new javax.swing.JButton();
        javax.swing.JScrollPane jScrollPane3 = new javax.swing.JScrollPane();
        jTextAreaManuell = new javax.swing.JTextArea();
        jCheckBoxUpdate = new javax.swing.JCheckBox();
        jButtonUrl = new javax.swing.JButton();
        jRadioButtonAuto = new javax.swing.JRadioButton();
        jRadioButtonManuell = new javax.swing.JRadioButton();

        jPanelAuto.setBorder(javax.swing.BorderFactory.createTitledBorder(javax.swing.BorderFactory.createLineBorder(new java.awt.Color(0, 0, 255)), "Die Filmliste automatisch laden"));

        jTextAreaAuto.setEditable(false);
        jTextAreaAuto.setColumns(20);
        jTextAreaAuto.setRows(4);
        jTextAreaAuto.setText("Die Filmliste wird beim Programmstart automatisch geladen (wenn sie\nälter als 3h ist). Zusätzlich kann sie über den Button \"Neue Filmliste laden\"\naktualisiert werden. Zum Update werden dann nur noch die Differenzlisten geladen (enthalten\nnur die neuen Filme).");
        jTextAreaAuto.setMargin(new java.awt.Insets(4, 4, 4, 4));
        jScrollPane2.setViewportView(jTextAreaAuto);

        jButtonLoad.setText("Filme jetzt laden");

        javax.swing.GroupLayout jPanelAutoLayout = new javax.swing.GroupLayout(jPanelAuto);
        jPanelAuto.setLayout(jPanelAutoLayout);
        jPanelAutoLayout.setHorizontalGroup(
                jPanelAutoLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                        .addGroup(jPanelAutoLayout.createSequentialGroup()
                                .addContainerGap()
                                .addGroup(jPanelAutoLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                                        .addComponent(jScrollPane2)
                                        .addGroup(javax.swing.GroupLayout.Alignment.TRAILING, jPanelAutoLayout.createSequentialGroup()
                                                .addGap(0, 0, Short.MAX_VALUE)
                                                .addComponent(jButtonLoad)))
                                .addContainerGap())
        );
        jPanelAutoLayout.setVerticalGroup(
                jPanelAutoLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                        .addGroup(jPanelAutoLayout.createSequentialGroup()
                                .addComponent(jScrollPane2, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                                .addComponent(jButtonLoad)
                                .addContainerGap(javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
        );

        jPanelManuel.setBorder(javax.swing.BorderFactory.createTitledBorder(javax.swing.BorderFactory.createLineBorder(new java.awt.Color(0, 0, 255)), "Filmliste nur manuell laden"));

        jLabel1.setText("URL/Datei:");

        jButtonDateiAuswaehlen.setIcon(new javax.swing.ImageIcon(getClass().getResource("/mediathek/res/muster/button-file-open.png"))); // NOI18N
        jButtonDateiAuswaehlen.setToolTipText("URL oder lokale Filmliste auswählen");

        jButtonFilmeLaden.setText("Filme jetzt laden");

        jTextAreaManuell.setEditable(false);
        jTextAreaManuell.setColumns(20);
        jTextAreaManuell.setRows(4);
        jTextAreaManuell.setText("Die Filmliste wird nur manuell über den Button \"Neue Filmliste laden\"\ngeladen. Es wird dann dieser Dialog angezeigt und es kann eine URL/Datei zum\nLaden angegeben werden.");
        jTextAreaManuell.setMargin(new java.awt.Insets(4, 4, 4, 4));
        jScrollPane3.setViewportView(jTextAreaManuell);

        jCheckBoxUpdate.setText("alte Filmliste nicht löschen, nur erweitern");

        jButtonUrl.setIcon(new javax.swing.ImageIcon(getClass().getResource("/mediathek/res/muster/button-aktualisieren.png"))); // NOI18N
        jButtonUrl.setToolTipText("Neue URL suchen");

        javax.swing.GroupLayout jPanelManuelLayout = new javax.swing.GroupLayout(jPanelManuel);
        jPanelManuel.setLayout(jPanelManuelLayout);
        jPanelManuelLayout.setHorizontalGroup(
                jPanelManuelLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                        .addGroup(jPanelManuelLayout.createSequentialGroup()
                                .addContainerGap()
                                .addGroup(jPanelManuelLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                                        .addGroup(javax.swing.GroupLayout.Alignment.TRAILING, jPanelManuelLayout.createSequentialGroup()
                                                .addComponent(jLabel1)
                                                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                                                .addComponent(jTextFieldUrl)
                                                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                                                .addComponent(jButtonUrl)
                                                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                                                .addComponent(jButtonDateiAuswaehlen))
                                        .addComponent(jScrollPane3)
                                        .addGroup(javax.swing.GroupLayout.Alignment.TRAILING, jPanelManuelLayout.createSequentialGroup()
                                                .addComponent(jCheckBoxUpdate)
                                                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED, 263, Short.MAX_VALUE)
                                                .addComponent(jButtonFilmeLaden)))
                                .addContainerGap())
        );
        jPanelManuelLayout.setVerticalGroup(
                jPanelManuelLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                        .addGroup(jPanelManuelLayout.createSequentialGroup()
                                .addComponent(jScrollPane3, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                                .addGroup(jPanelManuelLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                                        .addComponent(jTextFieldUrl, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                                        .addComponent(jLabel1)
                                        .addComponent(jButtonDateiAuswaehlen)
                                        .addComponent(jButtonUrl))
                                .addGap(18, 18, 18)
                                .addGroup(jPanelManuelLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                                        .addComponent(jButtonFilmeLaden)
                                        .addComponent(jCheckBoxUpdate))
                                .addContainerGap())
        );

        jPanelManuelLayout.linkSize(javax.swing.SwingConstants.VERTICAL, new java.awt.Component[] {jButtonDateiAuswaehlen, jLabel1, jTextFieldUrl});

        buttonGroup1.add(jRadioButtonAuto);

        buttonGroup1.add(jRadioButtonManuell);

        javax.swing.GroupLayout layout = new javax.swing.GroupLayout(this);
        this.setLayout(layout);
        layout.setHorizontalGroup(
                layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                        .addGroup(layout.createSequentialGroup()
                                .addContainerGap()
                                .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                                        .addComponent(jRadioButtonAuto)
                                        .addComponent(jRadioButtonManuell))
                                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                                .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                                        .addComponent(jPanelAuto, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                                        .addComponent(jPanelManuel, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
                                .addContainerGap())
        );
        layout.setVerticalGroup(
                layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                        .addGroup(layout.createSequentialGroup()
                                .addContainerGap()
                                .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                                        .addComponent(jRadioButtonAuto)
                                        .addComponent(jPanelAuto, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE))
                                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                                .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                                        .addComponent(jRadioButtonManuell)
                                        .addComponent(jPanelManuel, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE))
                                .addContainerGap(javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
        );
    }// </editor-fold>//GEN-END:initComponents

    // Variables declaration - do not modify//GEN-BEGIN:variables
    private javax.swing.JButton jButtonDateiAuswaehlen;
    private javax.swing.JButton jButtonFilmeLaden;
    private javax.swing.JButton jButtonLoad;
    private javax.swing.JButton jButtonUrl;
    private javax.swing.JCheckBox jCheckBoxUpdate;
    private javax.swing.JRadioButton jRadioButtonAuto;
    private javax.swing.JRadioButton jRadioButtonManuell;
    private javax.swing.JTextArea jTextAreaAuto;
    private javax.swing.JTextArea jTextAreaManuell;
    private javax.swing.JTextField jTextFieldUrl;
    // End of variables declaration//GEN-END:variables

    private class BeobOption implements ActionListener {

        @Override
        public void actionPerformed(ActionEvent e) {
            if (!stopBeob) {
                if (jRadioButtonManuell.isSelected()) {
                    MVConfig.add(MVConfig.Configs.SYSTEM_IMPORT_ART_FILME, String.valueOf(Konstanten.UPDATE_FILME_AUS));
                } else {
                    MVConfig.add(MVConfig.Configs.SYSTEM_IMPORT_ART_FILME, String.valueOf(Konstanten.UPDATE_FILME_AUTO));
                }
                // den Dialog gibts 2x
                Listener.notify(Listener.EREIGNIS_ART_IMPORT_FILMLISTE, this.getClass().getSimpleName());
            }
        }
    }

    private class BeobPfad implements ActionListener {

        @Override
        public void actionPerformed(ActionEvent e) {
            //we can use native chooser on Mac...
            if (SystemInfo.isMacOSX()) {
                FileDialog chooser = new FileDialog(daten.getMediathekGui(), "Filmliste laden");
                chooser.setMode(FileDialog.LOAD);
                chooser.setVisible(true);
                if (chooser.getFile() != null) {
                    try {
                        File destination = new File(chooser.getDirectory() + chooser.getFile());
                        jTextFieldUrl.setText(destination.getAbsolutePath());
                    } catch (Exception ex) {
                        Log.errorLog(102036579, ex);
                    }
                }
            } else {
                int returnVal;
                JFileChooser chooser = new JFileChooser();
                if (!jTextFieldUrl.getText().equals("")) {
                    chooser.setCurrentDirectory(new File(jTextFieldUrl.getText()));
                }
                chooser.setFileSelectionMode(JFileChooser.FILES_ONLY);
                chooser.setFileHidingEnabled(false);
                returnVal = chooser.showOpenDialog(null);
                if (returnVal == JFileChooser.APPROVE_OPTION) {
                    try {
                        jTextFieldUrl.setText(chooser.getSelectedFile().getAbsolutePath());
                    } catch (Exception ex) {
                        Log.errorLog(733025319, ex);
                    }
                }
            }
        }
    }

    private class BeobDateiUrl implements DocumentListener {

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
            MVConfig.add(MVConfig.Configs.SYSTEM_IMPORT_URL_MANUELL, jTextFieldUrl.getText());
        }
    }
}
