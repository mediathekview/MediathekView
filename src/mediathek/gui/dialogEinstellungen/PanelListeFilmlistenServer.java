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

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.io.File;
import javax.swing.JDialog;
import javax.swing.JFileChooser;
import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;
import javax.swing.event.ListSelectionEvent;
import mediathek.controller.filmeLaden.importieren.DatenFilmlistenServer;
import mediathek.daten.DDaten;
import mediathek.daten.Daten;
import mediathek.gui.PanelVorlage;
import mediathek.tool.GuiKonstanten;
import mediathek.tool.Konstanten;
import mediathek.tool.ListenerMediathekView;
import mediathek.tool.Log;
import mediathek.tool.TModel;

public class PanelListeFilmlistenServer extends PanelVorlage {

    private JDialog dialog = null;

    public PanelListeFilmlistenServer(DDaten d, JDialog ddialog) {
        super(d);
        dialog = ddialog;
        initComponents();
        init();
    }

    public PanelListeFilmlistenServer(DDaten d) {
        super(d);
        initComponents();
        init();
    }

    private void init() {
        initRadio();
        tabelleLaden();
        jButtonDateiAuswaehlen.addActionListener(new BeobPfad());
        jRadioButtonUpdateAus.addActionListener(new BeobOption());
        jRadioButtonAuto.addActionListener(new BeobOption());
        //jTable1.getSelectionModel().addListSelectionListener(new BeobachterTableSelect());
        jTable1.addMouseListener(new BeobachterTableSelect());
        jTextFieldUrl.getDocument().addDocumentListener(new BeobDateiUrl());
        ListenerMediathekView.addListener(new ListenerMediathekView(ListenerMediathekView.EREIGNIS_LISTE_FILMLISTEN_SERVER, PanelListeFilmlistenServer.class.getSimpleName()) {
            @Override
            public void ping() {
                tabelleLaden();
            }
        });
//        ListenerMediathekView.addListener(new ListenerMediathekView(ListenerMediathekView.EREIGNIS_ART_IMPORT_FILMLISTE, PanelListeFilmlistenServer.class.getSimpleName()) {
//            @Override
//            public void ping() {
//                initRadio();
//            }
//        });
    }

    private void initRadio() {
//        if (GuiFunktionen.getImportArtFilme() == GuiKonstanten.UPDATE_FILME_AUS) {
//            jRadioButtonUpdateAus.setSelected(true);
//        } else {
//            jRadioButtonAuto.setSelected(true);
//        }
//        jTextFieldUrl.setText(Daten.system[Konstanten.SYSTEM_IMPORT_URL_MANUELL_NR]);
//        setPanelTabelle(jRadioButtonUpdateAus.isSelected());
    }

    private void tabelleLaden() {
        TModel model = new TModel(DDaten.filmeLaden.getListeFilmlistnServer().getTableObjectData(), DatenFilmlistenServer.FILM_LISTEN_SERVER_COLUMN_NAMES_ANZEIGE);
        jTable1.setModel(model);
        for (int i = 0; i < jTable1.getColumnCount(); ++i) {
            jTable1.getColumnModel().getColumn(i).setMinWidth(10);
            jTable1.getColumnModel().getColumn(i).setMaxWidth(3000);
            jTable1.getColumnModel().getColumn(i).setPreferredWidth(100);
        }
    }

    private void table1Select(boolean doppel) {
//        stopBeob = true;
//        DatenUrlFilmliste datenUrlFilmliste = null;
//        int selectedTableRow = jTable1.getSelectedRow();
//        if (selectedTableRow >= 0) {
//            datenUrlFilmliste = Daten.filmeLaden.getListeUrlFilmlisten(false).getNrUpdate(jTable1.getModel().getValueAt(jTable1.convertRowIndexToModel(selectedTableRow),
//                    FilmlistenServer.FILM_UPDATE_SERVER_URL_NR).toString());
//        }
//        if (datenUrlFilmliste != null) {
//            //jRadioButtonUpdateAus.setSelected(true);
//            //daten.system[Konstanten.SYSTEM_IMPORT_ART_FILME_NR] = String.valueOf(Konstanten.UPDATE_FILME_AUS);
//            jTextFieldUrl.setText(datenUrlFilmliste.arr[FilmlistenServer.FILM_UPDATE_SERVER_URL_NR]);
//            Daten.system[Konstanten.SYSTEM_IMPORT_URL_MANUELL_NR] = datenUrlFilmliste.arr[FilmlistenServer.FILM_UPDATE_SERVER_URL_NR];
//            if (doppel) {
//                // dann wars ein Doppelklick, gleich laden
//                Daten.filmeLaden.importFilmliste(Daten.system[Konstanten.SYSTEM_IMPORT_URL_MANUELL_NR]);
//            }
//        }
//        stopBeob = false;
//        if (doppel && dialog != null) {
//            dialog.dispose();
//        }
    }

    private void setPanelTabelle(boolean manuell) {
        jTextFieldUrl.setEnabled(manuell);
        jTable1.setEnabled(manuell);
        jButtonDateiAuswaehlen.setEnabled(manuell);
    }

    /** This method is called from within the constructor to
     * initialize the form.
     * WARNING: Do NOT modify this code. The content of this method is
     * always regenerated by the Form Editor.
     */
    // <editor-fold defaultstate="collapsed" desc="Generated Code">//GEN-BEGIN:initComponents
    private void initComponents() {

        buttonGroup1 = new javax.swing.ButtonGroup();
        jPanel2 = new javax.swing.JPanel();
        jScrollPane1 = new javax.swing.JScrollPane();
        jTable1 = new javax.swing.JTable();
        jLabel1 = new javax.swing.JLabel();
        jTextFieldUrl = new javax.swing.JTextField();
        jButtonDateiAuswaehlen = new javax.swing.JButton();
        jRadioButtonAuto = new javax.swing.JRadioButton();
        jRadioButtonUpdateAus = new javax.swing.JRadioButton();

        jPanel2.setBorder(javax.swing.BorderFactory.createEtchedBorder());

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

        jLabel1.setText("URL/Datei:");

        jButtonDateiAuswaehlen.setIcon(new javax.swing.ImageIcon(getClass().getResource("/mediathek/res/fileopen_16.png"))); // NOI18N

        javax.swing.GroupLayout jPanel2Layout = new javax.swing.GroupLayout(jPanel2);
        jPanel2.setLayout(jPanel2Layout);
        jPanel2Layout.setHorizontalGroup(
            jPanel2Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel2Layout.createSequentialGroup()
                .addContainerGap()
                .addGroup(jPanel2Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addComponent(jScrollPane1)
                    .addGroup(javax.swing.GroupLayout.Alignment.TRAILING, jPanel2Layout.createSequentialGroup()
                        .addComponent(jLabel1)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                        .addComponent(jTextFieldUrl)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                        .addComponent(jButtonDateiAuswaehlen)))
                .addContainerGap())
        );
        jPanel2Layout.setVerticalGroup(
            jPanel2Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel2Layout.createSequentialGroup()
                .addGap(25, 25, 25)
                .addComponent(jScrollPane1, javax.swing.GroupLayout.DEFAULT_SIZE, 169, Short.MAX_VALUE)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                .addGroup(jPanel2Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(jTextFieldUrl, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                    .addComponent(jLabel1)
                    .addComponent(jButtonDateiAuswaehlen))
                .addContainerGap())
        );

        jPanel2Layout.linkSize(javax.swing.SwingConstants.VERTICAL, new java.awt.Component[] {jButtonDateiAuswaehlen, jLabel1, jTextFieldUrl});

        buttonGroup1.add(jRadioButtonAuto);
        jRadioButtonAuto.setText("Das Programm wählt selbst aus, wo die Filmlisten geladen werden");

        buttonGroup1.add(jRadioButtonUpdateAus);
        jRadioButtonUpdateAus.setText("Selbst auswählen: Bei falschen Angaben klappt das das Laden der Filme nicht mehr!");

        javax.swing.GroupLayout layout = new javax.swing.GroupLayout(this);
        this.setLayout(layout);
        layout.setHorizontalGroup(
            layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(layout.createSequentialGroup()
                .addContainerGap()
                .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addGroup(layout.createSequentialGroup()
                        .addComponent(jRadioButtonAuto)
                        .addGap(13, 13, 13))
                    .addGroup(javax.swing.GroupLayout.Alignment.TRAILING, layout.createSequentialGroup()
                        .addComponent(jRadioButtonUpdateAus)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)))
                .addContainerGap(29, Short.MAX_VALUE))
            .addGroup(layout.createSequentialGroup()
                .addGap(29, 29, 29)
                .addComponent(jPanel2, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                .addContainerGap())
        );
        layout.setVerticalGroup(
            layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(layout.createSequentialGroup()
                .addContainerGap()
                .addComponent(jRadioButtonAuto)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                .addComponent(jRadioButtonUpdateAus)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(jPanel2, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                .addContainerGap())
        );
    }// </editor-fold>//GEN-END:initComponents

    // Variables declaration - do not modify//GEN-BEGIN:variables
    private javax.swing.ButtonGroup buttonGroup1;
    private javax.swing.JButton jButtonDateiAuswaehlen;
    private javax.swing.JLabel jLabel1;
    private javax.swing.JPanel jPanel2;
    private javax.swing.JRadioButton jRadioButtonAuto;
    private javax.swing.JRadioButton jRadioButtonUpdateAus;
    private javax.swing.JScrollPane jScrollPane1;
    private javax.swing.JTable jTable1;
    private javax.swing.JTextField jTextFieldUrl;
    // End of variables declaration//GEN-END:variables

    private class BeobOption implements ActionListener {

        @Override
        public void actionPerformed(ActionEvent e) {
            if (!stopBeob) {
                if (jRadioButtonUpdateAus.isSelected()) {
                    Daten.system[Konstanten.SYSTEM_IMPORT_ART_FILME_NR] = String.valueOf(GuiKonstanten.UPDATE_FILME_AUS);
                } else {
                    Daten.system[Konstanten.SYSTEM_IMPORT_ART_FILME_NR] = String.valueOf(GuiKonstanten.UPDATE_FILME_AUTO);
                }                // den Dialog gibts 2x
                ListenerMediathekView.notify(ListenerMediathekView.EREIGNIS_ART_IMPORT_FILMLISTE, this.getClass().getSimpleName());
            }
        }
    }

    private class BeobachterTableSelect implements MouseListener {

        public void valueChanged(ListSelectionEvent event) {
            if (!stopBeob) {
                if (!event.getValueIsAdjusting()) {
                    table1Select(false);
                }
            }
        }

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
                    Log.fehlerMeldung(733025319, "PanelImportFilme.BeobPfad", ex);
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
        }
    }
}
