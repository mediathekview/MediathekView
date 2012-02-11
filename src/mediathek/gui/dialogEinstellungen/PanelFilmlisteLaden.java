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

import java.awt.Cursor;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.File;
import javax.swing.JFileChooser;
import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;
import javax.swing.table.DefaultTableModel;
import mediathek.Daten;
import mediathek.Konstanten;
import mediathek.Log;
import mediathek.controller.filme.filmeImportieren.filmUpdateServer.DatenFilmUpdateServer;
import mediathek.controller.filme.filmeImportieren.filmUpdateServer.FilmUpdateServer;
import mediathek.daten.DDaten;
import mediathek.gui.PanelVorlage;
import mediathek.tool.GuiFunktionen;
import mediathek.tool.GuiKonstanten;

public class PanelFilmlisteLaden extends PanelVorlage {

    public PanelFilmlisteLaden(DDaten d) {
        super(d);
        initComponents();
        init();
        jButtonUpdate.addActionListener(new BeobSuchen());
        jButtonDateiAuswaehlen.addActionListener(new BeobPfad());
        jButtonFilmeLaden.addActionListener(new ActionListener() {

            @Override
            public void actionPerformed(ActionEvent e) {
                Daten.filmeLaden.filmeLaden(ddaten);
            }
        });
        jRadioButtonUpdateAus.addActionListener(new BeobOption());
        jRadioButtonUrl.addActionListener(new BeobOption());
        jRadioButtonAuto.addActionListener(new BeobOption());
        jTable1.getSelectionModel().addListSelectionListener(new BeobachterTableSelect());
        jTextFieldUrl.getDocument().addDocumentListener(new BeobDateiUrl());
    }

    @Override
    public void neuLaden() {
        init();
    }

    private void init() {
        jRadioButtonAuto.setSelected(GuiFunktionen.getImportArtFilme() == GuiKonstanten.UPDATE_FILME_AUTO);
        jRadioButtonUrl.setSelected(GuiFunktionen.getImportArtFilme() == GuiKonstanten.UPDATE_FILME_URL);
        jRadioButtonUpdateAus.setSelected(GuiFunktionen.getImportArtFilme() == GuiKonstanten.UPDATE_FILME_AUS);
        jTextFieldUrl.setText(Daten.system[Konstanten.SYSTEM_IMPORT_URL_MANUELL_NR]);
        tabelleLaden();
        setPanelTabelle(jRadioButtonUpdateAus.isSelected());
    }

    private void updateSuchen() {
        this.setCursor(Cursor.getPredefinedCursor(Cursor.WAIT_CURSOR));
        DDaten.filmeLaden.getListeFilmUpdateServer(true); // Liste neu laden und eine URL auswählen
        stopBeob = true;
        jTextFieldUrl.setText(Daten.system[Konstanten.SYSTEM_IMPORT_URL_MANUELL_NR]);
        tabelleLaden();
        stopBeob = false;
        this.setCursor(Cursor.getDefaultCursor());
    }

    private void tabelleLaden() {
        DefaultTableModel model = new DefaultTableModel(DDaten.filmeLaden.getListeFilmUpdateServer(false).getTableObjectData(), FilmUpdateServer.FILM_UPDATE_SERVER_COLUMN_NAMES_ANZEIGE) {

            @Override
            public boolean isCellEditable(int i, int j) {
                return false;
            }
        };
        jTable1.setModel(model);
        for (int i = 0; i < jTable1.getColumnCount(); ++i) {
            if (i == FilmUpdateServer.FILM_UPDATE_SERVER_URL_NR) {
                jTable1.getColumnModel().getColumn(i).setMinWidth(10);
                jTable1.getColumnModel().getColumn(i).setMaxWidth(3000);
                jTable1.getColumnModel().getColumn(i).setPreferredWidth(350);
            } else if (i == FilmUpdateServer.FILM_UPDATE_SERVER_PRIO_NR || i == FilmUpdateServer.FILM_UPDATE_SERVER_ANZAHL_NR) {
                jTable1.getColumnModel().getColumn(i).setMinWidth(0);
                jTable1.getColumnModel().getColumn(i).setMaxWidth(0);
                jTable1.getColumnModel().getColumn(i).setPreferredWidth(0);
            } else {
                jTable1.getColumnModel().getColumn(i).setMinWidth(10);
                jTable1.getColumnModel().getColumn(i).setMaxWidth(3000);
                jTable1.getColumnModel().getColumn(i).setPreferredWidth(100);
            }
        }
    }

    private void table1Select() {
        stopBeob = true;
        DatenFilmUpdateServer aktUpdate = null;
        int selectedTableRow = jTable1.getSelectedRow();
        if (selectedTableRow >= 0) {
            aktUpdate = Daten.filmeLaden.getListeFilmUpdateServer(false).getNrUpdate(jTable1.getModel().getValueAt(jTable1.convertRowIndexToModel(selectedTableRow),
                    FilmUpdateServer.FILM_UPDATE_SERVER_URL_NR).toString());
        }
        if (aktUpdate != null) {
            //jRadioButtonUpdateAus.setSelected(true);
            //daten.system[Konstanten.SYSTEM_IMPORT_ART_FILME_NR] = String.valueOf(Konstanten.UPDATE_FILME_AUS);
            jTextFieldUrl.setText(aktUpdate.arr[FilmUpdateServer.FILM_UPDATE_SERVER_URL_NR]);
            ddaten.setGeaendertPanelSofort();
            Daten.system[Konstanten.SYSTEM_IMPORT_URL_MANUELL_NR] = aktUpdate.arr[FilmUpdateServer.FILM_UPDATE_SERVER_URL_NR];
        }
        stopBeob = false;
    }

    private void setPanelTabelle(boolean manuell) {
        jButtonUpdate.setEnabled(manuell);
        jTextFieldUrl.setEnabled(manuell);
        jTable1.setEnabled(manuell);
        jButtonDateiAuswaehlen.setEnabled(manuell);
        jButtonFilmeLaden.setEnabled(manuell);
    }

    /** This method is called from within the constructor to
     * initialize the form.
     * WARNING: Do NOT modify this code. The content of this method is
     * always regenerated by the Form Editor.
     */
    // <editor-fold defaultstate="collapsed" desc="Generated Code">//GEN-BEGIN:initComponents
    private void initComponents() {

        buttonGroup1 = new javax.swing.ButtonGroup();
        jPanel3 = new javax.swing.JPanel();
        jRadioButtonAuto = new javax.swing.JRadioButton();
        jRadioButtonUrl = new javax.swing.JRadioButton();
        jPanel2 = new javax.swing.JPanel();
        jRadioButtonUpdateAus = new javax.swing.JRadioButton();
        jScrollPane1 = new javax.swing.JScrollPane();
        jTable1 = new javax.swing.JTable();
        jLabel2 = new javax.swing.JLabel();
        jButtonUpdate = new javax.swing.JButton();
        jLabel1 = new javax.swing.JLabel();
        jTextFieldUrl = new javax.swing.JTextField();
        jButtonDateiAuswaehlen = new javax.swing.JButton();
        jButtonFilmeLaden = new javax.swing.JButton();

        jPanel3.setBorder(javax.swing.BorderFactory.createTitledBorder(javax.swing.BorderFactory.createLineBorder(new java.awt.Color(0, 0, 255)), "Filmliste (URL) automatisch wählen"));

        buttonGroup1.add(jRadioButtonAuto);
        jRadioButtonAuto.setText("zusätzlich beim Programmstart laden, wenn die Filmliste älter als 5h ist");

        buttonGroup1.add(jRadioButtonUrl);
        jRadioButtonUrl.setText("das Laden der Filme von Hand anstoßen, die URL automatisch wählen");

        javax.swing.GroupLayout jPanel3Layout = new javax.swing.GroupLayout(jPanel3);
        jPanel3.setLayout(jPanel3Layout);
        jPanel3Layout.setHorizontalGroup(
            jPanel3Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel3Layout.createSequentialGroup()
                .addContainerGap()
                .addGroup(jPanel3Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addComponent(jRadioButtonUrl)
                    .addComponent(jRadioButtonAuto))
                .addContainerGap(javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
        );
        jPanel3Layout.setVerticalGroup(
            jPanel3Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel3Layout.createSequentialGroup()
                .addContainerGap()
                .addComponent(jRadioButtonUrl)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                .addComponent(jRadioButtonAuto)
                .addContainerGap(javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
        );

        jPanel2.setBorder(javax.swing.BorderFactory.createTitledBorder(javax.swing.BorderFactory.createLineBorder(new java.awt.Color(0, 0, 255)), "Filmliste (URL/Datei) manuell wählen und diesen Dialog anzeigen"));

        buttonGroup1.add(jRadioButtonUpdateAus);
        jRadioButtonUpdateAus.setText("Alles manuell");

        jTable1.setAutoCreateRowSorter(true);
        jTable1.setBorder(javax.swing.BorderFactory.createEtchedBorder());
        jTable1.setModel(new javax.swing.table.DefaultTableModel(
            new Object [][] {
                {null, null, null, null},
                {null, null, null, null},
                {null, null, null, null},
                {null, null, null, null}
            },
            new String [] {
                "Title 1", "Title 2", "Title 3", "Title 4"
            }
        ));
        jTable1.setAutoResizeMode(javax.swing.JTable.AUTO_RESIZE_OFF);
        jScrollPane1.setViewportView(jTable1);

        jLabel2.setText("Liste der Downloadserver (URL's) aktualisieren:");

        jButtonUpdate.setIcon(new javax.swing.ImageIcon(getClass().getResource("/mediathek/res/view-refresh_16.png"))); // NOI18N

        jLabel1.setText("URL/Datei:");

        jButtonDateiAuswaehlen.setIcon(new javax.swing.ImageIcon(getClass().getResource("/mediathek/res/fileopen_16.png"))); // NOI18N

        jButtonFilmeLaden.setText("Filme jetzt laden");

        javax.swing.GroupLayout jPanel2Layout = new javax.swing.GroupLayout(jPanel2);
        jPanel2.setLayout(jPanel2Layout);
        jPanel2Layout.setHorizontalGroup(
            jPanel2Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel2Layout.createSequentialGroup()
                .addContainerGap()
                .addGroup(jPanel2Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addComponent(jScrollPane1)
                    .addGroup(jPanel2Layout.createSequentialGroup()
                        .addComponent(jRadioButtonUpdateAus)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED, 16, Short.MAX_VALUE)
                        .addComponent(jLabel2)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                        .addComponent(jButtonUpdate))
                    .addGroup(javax.swing.GroupLayout.Alignment.TRAILING, jPanel2Layout.createSequentialGroup()
                        .addComponent(jLabel1)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                        .addComponent(jTextFieldUrl)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                        .addComponent(jButtonDateiAuswaehlen))
                    .addGroup(javax.swing.GroupLayout.Alignment.TRAILING, jPanel2Layout.createSequentialGroup()
                        .addGap(0, 0, Short.MAX_VALUE)
                        .addComponent(jButtonFilmeLaden)))
                .addContainerGap())
        );
        jPanel2Layout.setVerticalGroup(
            jPanel2Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel2Layout.createSequentialGroup()
                .addGroup(jPanel2Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.CENTER)
                    .addComponent(jRadioButtonUpdateAus)
                    .addComponent(jLabel2)
                    .addComponent(jButtonUpdate))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                .addComponent(jScrollPane1, javax.swing.GroupLayout.DEFAULT_SIZE, 133, Short.MAX_VALUE)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                .addGroup(jPanel2Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(jTextFieldUrl, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                    .addComponent(jLabel1)
                    .addComponent(jButtonDateiAuswaehlen))
                .addGap(18, 18, 18)
                .addComponent(jButtonFilmeLaden)
                .addContainerGap())
        );

        jPanel2Layout.linkSize(javax.swing.SwingConstants.VERTICAL, new java.awt.Component[] {jButtonDateiAuswaehlen, jLabel1, jTextFieldUrl});

        javax.swing.GroupLayout layout = new javax.swing.GroupLayout(this);
        this.setLayout(layout);
        layout.setHorizontalGroup(
            layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(layout.createSequentialGroup()
                .addContainerGap()
                .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addComponent(jPanel3, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                    .addComponent(jPanel2, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
                .addContainerGap())
        );
        layout.setVerticalGroup(
            layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(layout.createSequentialGroup()
                .addContainerGap()
                .addComponent(jPanel3, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addGap(18, 18, 18)
                .addComponent(jPanel2, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                .addContainerGap())
        );
    }// </editor-fold>//GEN-END:initComponents

    // Variables declaration - do not modify//GEN-BEGIN:variables
    private javax.swing.ButtonGroup buttonGroup1;
    private javax.swing.JButton jButtonDateiAuswaehlen;
    private javax.swing.JButton jButtonFilmeLaden;
    private javax.swing.JButton jButtonUpdate;
    private javax.swing.JLabel jLabel1;
    private javax.swing.JLabel jLabel2;
    private javax.swing.JPanel jPanel2;
    private javax.swing.JPanel jPanel3;
    private javax.swing.JRadioButton jRadioButtonAuto;
    private javax.swing.JRadioButton jRadioButtonUpdateAus;
    private javax.swing.JRadioButton jRadioButtonUrl;
    private javax.swing.JScrollPane jScrollPane1;
    private javax.swing.JTable jTable1;
    private javax.swing.JTextField jTextFieldUrl;
    // End of variables declaration//GEN-END:variables

    private class BeobSuchen implements ActionListener {

        @Override
        public void actionPerformed(ActionEvent e) {
            updateSuchen();
        }
    }

    private class BeobOption implements ActionListener {

        @Override
        public void actionPerformed(ActionEvent e) {
            if (!stopBeob) {
                setPanelTabelle(jRadioButtonUpdateAus.isSelected());
                if (jRadioButtonUpdateAus.isSelected()) {
                    Daten.system[Konstanten.SYSTEM_IMPORT_ART_FILME_NR] = String.valueOf(GuiKonstanten.UPDATE_FILME_AUS);
                } else if (jRadioButtonUrl.isSelected()) {
                    Daten.system[Konstanten.SYSTEM_IMPORT_ART_FILME_NR] = String.valueOf(GuiKonstanten.UPDATE_FILME_URL);
                } else {
                    Daten.system[Konstanten.SYSTEM_IMPORT_ART_FILME_NR] = String.valueOf(GuiKonstanten.UPDATE_FILME_AUTO);
                    Daten.filmeLaden.resetTimer();
                }
                Daten.setGeaendert();
            }
        }
    }

    private class BeobachterTableSelect implements ListSelectionListener {

        public int selectedModelRow = -1;

        @Override
        public void valueChanged(ListSelectionEvent event) {
            if (!stopBeob) {
                if (!event.getValueIsAdjusting()) {
                    table1Select();
                }
            }
        }
    }

    private class BeobPfad implements ActionListener {

        @Override
        public void actionPerformed(ActionEvent e) {
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
                    Log.fehlerMeldung("PanelImportFilme.BeobPfad", ex);
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
            Daten.system[Konstanten.SYSTEM_IMPORT_URL_MANUELL_NR] = jTextFieldUrl.getText();
            Daten.setGeaendert();
        }
    }
}
