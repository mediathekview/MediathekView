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
import mSearch.filmlisten.DatenFilmlisteUrl;
import mSearch.tool.Listener;
import mSearch.tool.Log;
import mediathek.config.Daten;
import mediathek.config.Icons;
import mediathek.config.MVConfig;
import mediathek.gui.PanelVorlage;
import mediathek.tool.TModel;

import javax.swing.*;
import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;
import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.io.File;

@SuppressWarnings("serial")
public class PanelFilmlisten extends PanelVorlage {
    public PanelFilmlisten(Daten d, JFrame parentComponent) {
        super(d, parentComponent);
        initComponents();
        init();
    }

    private void init() {
        if (!Daten.getInstance().isDebug()) {
            jScrollPane1.setVisible(false);
            jTable1.setVisible(false);
            jLabelAktListe.setVisible(false);
            jRadioButtonAkt.setVisible(false);
            jRadioButtonDiff.setVisible(false);
            jButtonAkualisieren.setVisible(false);
        }
        jButtonAkualisieren.setIcon(Icons.ICON_BUTTON_AKTUALISIEREN);
        jButtonDateiAuswaehlen.setIcon(Icons.ICON_BUTTON_FILE_OPEN);
        jButtonUrl.setIcon(Icons.ICON_BUTTON_AKTUALISIEREN);
        tabelleLaden();
        jButtonUrl.addActionListener(new ActionListener() {

            @Override
            public void actionPerformed(ActionEvent e) {
                jTextFieldUrl.setText(daten.getFilmeLaden().getDownloadUrl_akt());
            }
        });
        jButtonAkualisieren.addActionListener(new ActionListener() {

            @Override
            public void actionPerformed(ActionEvent e) {
                listeFilmlistenSuchen();
            }
        });
        jButtonDateiAuswaehlen.addActionListener(new BeobPfad());
        jButtonFilmeLaden.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                if (jCheckBoxUpdate.isSelected()) {
                    daten.getFilmeLaden().updateFilmlist(jTextFieldUrl.getText());
                } else {
                    daten.getFilmeLaden().loadFilmlist(jTextFieldUrl.getText());
                }
            }
        });
        jRadioButtonAkt.addActionListener(new ActionListener() {

            @Override
            public void actionPerformed(ActionEvent e) {
                jCheckBoxUpdate.setSelected(jRadioButtonDiff.isSelected());
                tabelleLaden();
            }
        });
        jRadioButtonDiff.addActionListener(new ActionListener() {

            @Override
            public void actionPerformed(ActionEvent e) {
                jCheckBoxUpdate.setSelected(jRadioButtonDiff.isSelected());
                tabelleLaden();
            }
        });
        jTable1.addMouseListener(new BeobachterTableSelect());
        jTextFieldUrl.getDocument().addDocumentListener(new BeobDateiUrl());
        Listener.addListener(new Listener(Listener.EREIGNIS_LISTE_URL_FILMLISTEN, PanelFilmlisten.class.getSimpleName()) {
            @Override
            public void ping() {
                tabelleLaden();
            }
        });
    }

    private void listeFilmlistenSuchen() {
        this.setCursor(Cursor.getPredefinedCursor(Cursor.WAIT_CURSOR));
        if (jRadioButtonAkt.isSelected()) {
            daten.getFilmeLaden().updateDownloadUrlsFilmlisten(true);
        } else /*diff*/ {
            daten.getFilmeLaden().updateDownloadUrlsFilmlisten(false);
        }
        stopBeob = true;
        tabelleLaden();
        stopBeob = false;
        this.setCursor(Cursor.getDefaultCursor());
    }

    private void tabelleLaden() {
        if (jRadioButtonAkt.isSelected()) {
            jTable1.setModel(new TModel(daten.getFilmeLaden().getDownloadUrlsFilmlisten_akt().getTableObjectData(), DatenFilmlisteUrl.FILM_UPDATE_SERVER_COLUMN_NAMES_ANZEIGE));
        } else /*diff*/ {
            jTable1.setModel(new TModel(daten.getFilmeLaden().getDownloadUrlsFilmlisten_diff().getTableObjectData(), DatenFilmlisteUrl.FILM_UPDATE_SERVER_COLUMN_NAMES_ANZEIGE));
        }
        for (int i = 0; i < jTable1.getColumnCount(); ++i) {
            if (i == DatenFilmlisteUrl.FILM_UPDATE_SERVER_URL_NR) {
                jTable1.getColumnModel().getColumn(i).setMinWidth(10);
                jTable1.getColumnModel().getColumn(i).setMaxWidth(3000);
                jTable1.getColumnModel().getColumn(i).setPreferredWidth(400);
            } else if (i == DatenFilmlisteUrl.FILM_UPDATE_SERVER_ART_NR) {
                jTable1.getColumnModel().getColumn(i).setMinWidth(0);
                jTable1.getColumnModel().getColumn(i).setMaxWidth(0);
                jTable1.getColumnModel().getColumn(i).setPreferredWidth(0);
            } else {
                jTable1.getColumnModel().getColumn(i).setMinWidth(10);
                jTable1.getColumnModel().getColumn(i).setMaxWidth(3000);
                jTable1.getColumnModel().getColumn(i).setPreferredWidth(100);
            }
        }
        if (jRadioButtonDiff.isSelected() || jRadioButtonAkt.isSelected()) {
            jTable1.getColumnModel().getColumn(jTable1.convertColumnIndexToView(DatenFilmlisteUrl.FILM_UPDATE_SERVER_DATUM_NR)).setMinWidth(0);
            jTable1.getColumnModel().getColumn(jTable1.convertColumnIndexToView(DatenFilmlisteUrl.FILM_UPDATE_SERVER_DATUM_NR)).setPreferredWidth(0);
            jTable1.getColumnModel().getColumn(jTable1.convertColumnIndexToView(DatenFilmlisteUrl.FILM_UPDATE_SERVER_DATUM_NR)).setMaxWidth(0);
            jTable1.getColumnModel().getColumn(jTable1.convertColumnIndexToView(DatenFilmlisteUrl.FILM_UPDATE_SERVER_ZEIT_NR)).setMinWidth(0);
            jTable1.getColumnModel().getColumn(jTable1.convertColumnIndexToView(DatenFilmlisteUrl.FILM_UPDATE_SERVER_ZEIT_NR)).setPreferredWidth(0);
            jTable1.getColumnModel().getColumn(jTable1.convertColumnIndexToView(DatenFilmlisteUrl.FILM_UPDATE_SERVER_ZEIT_NR)).setMaxWidth(0);
//            jTable1.getColumnModel().getColumn(jTable1.convertColumnIndexToView(DatenFilmlisteUrl.FILM_UPDATE_SERVER_PRIO_NR)).setMinWidth(0);
//            jTable1.getColumnModel().getColumn(jTable1.convertColumnIndexToView(DatenFilmlisteUrl.FILM_UPDATE_SERVER_PRIO_NR)).setPreferredWidth(0);
//            jTable1.getColumnModel().getColumn(jTable1.convertColumnIndexToView(DatenFilmlisteUrl.FILM_UPDATE_SERVER_PRIO_NR)).setMaxWidth(0);
        }
    }

    private void table1Select(boolean doppel) {
        stopBeob = true;
        DatenFilmlisteUrl datenUrlFilmliste = null;
        int selectedTableRow = jTable1.getSelectedRow();
        if (selectedTableRow >= 0) {
            String url = jTable1.getModel().getValueAt(jTable1.convertRowIndexToModel(selectedTableRow), DatenFilmlisteUrl.FILM_UPDATE_SERVER_URL_NR).toString();
            if (jRadioButtonAkt.isSelected()) {
                datenUrlFilmliste = daten.getFilmeLaden().getDownloadUrlsFilmlisten_akt().getDatenUrlFilmliste(url);
            } else /*diff*/ {
                datenUrlFilmliste = daten.getFilmeLaden().getDownloadUrlsFilmlisten_diff().getDatenUrlFilmliste(url);
            }
        }
        if (datenUrlFilmliste != null) {
            jTextFieldUrl.setText(datenUrlFilmliste.arr[DatenFilmlisteUrl.FILM_UPDATE_SERVER_URL_NR]);
            if (doppel) {
                // dann wars ein Doppelklick, gleich laden
                if (jCheckBoxUpdate.isSelected()) {
                    daten.getFilmeLaden().updateFilmlist(jTextFieldUrl.getText());
                } else {
                    daten.getFilmeLaden().loadFilmlist(jTextFieldUrl.getText());
                }
            }
        }
        stopBeob = false;
    }

    // <editor-fold defaultstate="collapsed" desc="Generated Code">//GEN-BEGIN:initComponents
    private void initComponents() {

        javax.swing.ButtonGroup buttonGroup1 = new javax.swing.ButtonGroup();
        javax.swing.ButtonGroup buttonGroup2 = new javax.swing.ButtonGroup();
        jPanelManuel = new javax.swing.JPanel();
        jScrollPane1 = new javax.swing.JScrollPane();
        jTable1 = new javax.swing.JTable();
        jLabelAktListe = new javax.swing.JLabel();
        jButtonAkualisieren = new javax.swing.JButton();
        javax.swing.JLabel jLabel1 = new javax.swing.JLabel();
        jTextFieldUrl = new javax.swing.JTextField();
        jButtonDateiAuswaehlen = new javax.swing.JButton();
        jButtonFilmeLaden = new javax.swing.JButton();
        jCheckBoxUpdate = new javax.swing.JCheckBox();
        jRadioButtonAkt = new javax.swing.JRadioButton();
        jRadioButtonDiff = new javax.swing.JRadioButton();
        jButtonUrl = new javax.swing.JButton();

        jPanelManuel.setBorder(javax.swing.BorderFactory.createTitledBorder(javax.swing.BorderFactory.createLineBorder(new java.awt.Color(0, 0, 255)), "Filmliste manuell laden"));

        jTable1.setAutoCreateRowSorter(true);
        jTable1.setModel(new TModel());
        jTable1.setAutoResizeMode(javax.swing.JTable.AUTO_RESIZE_OFF);
        jScrollPane1.setViewportView(jTable1);

        jLabelAktListe.setText("Liste aktualisieren:");

        jButtonAkualisieren.setIcon(new javax.swing.ImageIcon(getClass().getResource("/mediathek/res/muster/button-aktualisieren.png"))); // NOI18N

        jLabel1.setText("URL/Datei:");

        jButtonDateiAuswaehlen.setIcon(new javax.swing.ImageIcon(getClass().getResource("/mediathek/res/muster/button-file-open.png"))); // NOI18N
        jButtonDateiAuswaehlen.setToolTipText("URL oder lokale Filmliste auswählen");

        jButtonFilmeLaden.setText("Filme jetzt laden");

        jCheckBoxUpdate.setText("alte Filmliste nicht löschen, nur erweitern");

        buttonGroup2.add(jRadioButtonAkt);
        jRadioButtonAkt.setSelected(true);
        jRadioButtonAkt.setText("aktuelle komplette Listen");

        buttonGroup2.add(jRadioButtonDiff);
        jRadioButtonDiff.setText("Differenzlisten");

        jButtonUrl.setIcon(new javax.swing.ImageIcon(getClass().getResource("/mediathek/res/muster/button-aktualisieren.png"))); // NOI18N
        jButtonUrl.setToolTipText("Neue URL suchen");

        javax.swing.GroupLayout jPanelManuelLayout = new javax.swing.GroupLayout(jPanelManuel);
        jPanelManuel.setLayout(jPanelManuelLayout);
        jPanelManuelLayout.setHorizontalGroup(
            jPanelManuelLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanelManuelLayout.createSequentialGroup()
                .addContainerGap()
                .addGroup(jPanelManuelLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addComponent(jScrollPane1)
                    .addGroup(javax.swing.GroupLayout.Alignment.TRAILING, jPanelManuelLayout.createSequentialGroup()
                        .addComponent(jLabel1)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                        .addComponent(jTextFieldUrl)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                        .addComponent(jButtonUrl)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                        .addComponent(jButtonDateiAuswaehlen))
                    .addGroup(javax.swing.GroupLayout.Alignment.TRAILING, jPanelManuelLayout.createSequentialGroup()
                        .addComponent(jCheckBoxUpdate)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                        .addComponent(jButtonFilmeLaden))
                    .addGroup(jPanelManuelLayout.createSequentialGroup()
                        .addComponent(jRadioButtonAkt)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                        .addComponent(jRadioButtonDiff)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED, 348, Short.MAX_VALUE)
                        .addComponent(jLabelAktListe)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                        .addComponent(jButtonAkualisieren)))
                .addContainerGap())
        );
        jPanelManuelLayout.setVerticalGroup(
            jPanelManuelLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanelManuelLayout.createSequentialGroup()
                .addContainerGap()
                .addGroup(jPanelManuelLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.CENTER)
                    .addComponent(jRadioButtonAkt)
                    .addComponent(jRadioButtonDiff)
                    .addComponent(jLabelAktListe)
                    .addComponent(jButtonAkualisieren))
                .addGap(9, 9, 9)
                .addComponent(jScrollPane1, javax.swing.GroupLayout.DEFAULT_SIZE, 444, Short.MAX_VALUE)
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

        javax.swing.GroupLayout layout = new javax.swing.GroupLayout(this);
        this.setLayout(layout);
        layout.setHorizontalGroup(
            layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(layout.createSequentialGroup()
                .addGap(31, 31, 31)
                .addComponent(jPanelManuel, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                .addContainerGap())
        );
        layout.setVerticalGroup(
            layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(javax.swing.GroupLayout.Alignment.TRAILING, layout.createSequentialGroup()
                .addContainerGap()
                .addComponent(jPanelManuel, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                .addContainerGap())
        );
    }// </editor-fold>//GEN-END:initComponents
    // Variables declaration - do not modify//GEN-BEGIN:variables
    private javax.swing.JButton jButtonAkualisieren;
    private javax.swing.JButton jButtonDateiAuswaehlen;
    private javax.swing.JButton jButtonFilmeLaden;
    private javax.swing.JButton jButtonUrl;
    private javax.swing.JCheckBox jCheckBoxUpdate;
    private javax.swing.JLabel jLabelAktListe;
    private javax.swing.JPanel jPanelManuel;
    private javax.swing.JRadioButton jRadioButtonAkt;
    private javax.swing.JRadioButton jRadioButtonDiff;
    private javax.swing.JScrollPane jScrollPane1;
    private javax.swing.JTable jTable1;
    private javax.swing.JTextField jTextFieldUrl;
    // End of variables declaration//GEN-END:variables

    private class BeobachterTableSelect implements MouseListener {

        @Override
        public void mouseClicked(MouseEvent e) {
            if (e.getClickCount() == 2) {
                table1Select(true);
            } else {
                table1Select(false);
            }
        }

        @Override
        public void mousePressed(MouseEvent e) {
        }

        @Override
        public void mouseReleased(MouseEvent e) {
        }

        @Override
        public void mouseEntered(MouseEvent e) {
        }

        @Override
        public void mouseExited(MouseEvent e) {
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
