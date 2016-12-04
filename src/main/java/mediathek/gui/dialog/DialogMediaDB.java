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

import mSearch.tool.FilenameUtils;
import mSearch.tool.Listener;
import mSearch.tool.Log;
import mSearch.tool.SysMsg;
import mediathek.config.Daten;
import mediathek.config.Icons;
import mediathek.config.MVConfig;
import mediathek.daten.DatenMediaDB;
import mediathek.file.GetFile;
import mediathek.tool.*;

import javax.swing.*;
import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;
import java.awt.*;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.io.File;

@SuppressWarnings("serial")
public class DialogMediaDB extends JDialog {
    private final JFrame parent;
    private final Daten daten;


    //    private boolean init = false;
    private MVTable tabelleFilme;
    public DialogMediaDB(JFrame pparent) {
        super(pparent, false);
        daten = Daten.getInstance();
        initComponents();
        this.parent = pparent;
        this.addWindowListener(new WindowAdapter() {
            @Override
            public void windowClosing(WindowEvent e) {
                beenden();
            }
        });
        this.setTitle("Mediensammlung durchsuchen");
        Listener.addListener(new Listener(Listener.EREIGNIS_MEDIA_DB_START, DialogMediaDB.class.getSimpleName()) {
            @Override
            public void ping() {
                // neue DB suchen
                makeIndex(true);
                jLabelSum.setText("0");
            }
        });
        Listener.addListener(new Listener(Listener.EREIGNIS_MEDIA_DB_STOP, DialogMediaDB.class.getSimpleName()) {
            @Override
            public void ping() {
                // neue DB liegt vor
                makeIndex(false);
                jLabelSum.setText(daten.getListeMediaDB().size() + "");
                searchFilmInDb();
            }
        });

        tabelleFilme = new MVTable(MVTable.TableType.MEDIA_DB);
        jScrollPane3.setViewportView(tabelleFilme);

        TModelMediaDB modelFilm = new TModelMediaDB(new Object[][]{}, DatenMediaDB.COLUMN_NAMES);
        final CellRendererMediaDB cellRenderer = new CellRendererMediaDB();
        tabelleFilme.setDefaultRenderer(Object.class, cellRenderer);
        tabelleFilme.setModel(modelFilm);
        tabelleFilme.addMouseListener(new BeobMausTabelle());
        tabelleFilme.getSelectionModel().addListSelectionListener(new BeobTableSelect());
        tabelleFilme.setAutoResizeMode(MVTable.AUTO_RESIZE_SUBSEQUENT_COLUMNS);
        tabelleFilme.initTabelle();

        progress.setVisible(false);
        progress.setIndeterminate(true);
        progress.setMaximum(0);
        progress.setMinimum(0);
        progress.setValue(0);

        jTextFieldSearch.addActionListener(e -> searchFilmInDb());
        jTextFieldSearch.getDocument().addDocumentListener(new BeobDoc());

        jButtonIndex.addActionListener(e -> daten.getListeMediaDB().createMediaDB(""));

        jButtonHelp.setIcon(Icons.ICON_BUTTON_HELP);
        jButtonHelp.addActionListener(e -> new DialogHilfe(parent, true, new GetFile().getHilfeSuchen(GetFile.PFAD_HILFETEXT_DIALOG_MEDIA_DB)).setVisible(true));
        jButtonSearch.addActionListener(e -> searchFilmInDb());
        jButtonBeenden.addActionListener(e -> beenden());
        new EscBeenden(this) {
            @Override
            public void beenden_() {
                beenden();
            }
        };
        GuiFunktionen.setSize(MVConfig.Configs.SYSTEM_MEDIA_DB_DIALOG_GROESSE, this, parent);
    }



    @Override
    public void setVisible(boolean vis) {
        super.setVisible(vis);
        if (vis && daten.getListeMediaPath().isEmpty()) {
            JOptionPane.showMessageDialog(parent, "Erst in den Einstellungen eine Mediensammlung einrichten.", "Mediensammlung leer!", JOptionPane.ERROR_MESSAGE);
        }
//        if (!init) {
//            // beim ersten anzeigen den Index bauen
//            Daten.mVMediaDB.makeIndex();
//            init = true;
//        }
    }

    public final void setVis() {
        this.setVisible(Boolean.parseBoolean(MVConfig.get(MVConfig.Configs.SYSTEM_MEDIA_DB_DIALOG_ANZEIGEN)));
        Listener.notify(Listener.EREIGNIS_DIALOG_MEDIA_DB, DialogMediaDB.class.getName());
    }

    public void tabelleSpeichern() {
        if (tabelleFilme != null) {
            tabelleFilme.tabelleNachDatenSchreiben();
        }
    }

    public void setFilter(String titel) {
        titel = FilenameUtils.replaceLeerDateiname(titel, false /*pfad*/,
                Boolean.parseBoolean(MVConfig.get(MVConfig.Configs.SYSTEM_USE_REPLACETABLE)),
                Boolean.parseBoolean(MVConfig.get(MVConfig.Configs.SYSTEM_ONLY_ASCII))); // mit den eingestellten Ersetzungen bearbeiten
        jTextFieldSearch.setText(titel);
    }

    private synchronized void searchFilmInDb() {
        TModelMediaDB model = new TModelMediaDB(new Object[][]{}, DatenMediaDB.COLUMN_NAMES);
        daten.getListeMediaDB().searchFilmInDB(model, jTextFieldSearch.getText());
        tabelleFilme.getSpalten();
        tabelleFilme.setModel(model);
        tabelleFilme.setSpalten();
        jLabelSizeFound.setText(model.getRowCount() + "");
    }

    private void makeIndex(boolean makeIndex) {
        progress.setVisible(makeIndex);
        jTextFieldSearch.setEnabled(!makeIndex);
        jButtonSearch.setEnabled(!makeIndex);
        jButtonIndex.setEnabled(!makeIndex);
    }

    private void zielordnerOeffnen() {
        int row = tabelleFilme.getSelectedRow();
        if (row >= 0) {
            String s = (String) tabelleFilme.getModel().getValueAt(tabelleFilme.convertRowIndexToModel(row), DatenMediaDB.MEDIA_DB_PATH);
            DirOpenAction.zielordnerOeffnen(parent, s);
        } else {
            new HinweisKeineAuswahl().zeigen(parent);
        }
    }

    private void filmAbspielen_() {
        int row = tabelleFilme.getSelectedRow();
        if (row >= 0) {
            String file = (String) tabelleFilme.getModel().getValueAt(tabelleFilme.convertRowIndexToModel(row), DatenMediaDB.MEDIA_DB_NAME);
            String path = (String) tabelleFilme.getModel().getValueAt(tabelleFilme.convertRowIndexToModel(row), DatenMediaDB.MEDIA_DB_PATH);
            OpenPlayerAction.filmAbspielen(parent, path + File.separator + file);
        } else {
            new HinweisKeineAuswahl().zeigen(parent);
        }
    }

    private void aktFilmSetzen() {
        int row = tabelleFilme.getSelectedRow();
        if (row >= 0) {
            String file = (String) tabelleFilme.getModel().getValueAt(tabelleFilme.convertRowIndexToModel(row), DatenMediaDB.MEDIA_DB_NAME);
            String path = (String) tabelleFilme.getModel().getValueAt(tabelleFilme.convertRowIndexToModel(row), DatenMediaDB.MEDIA_DB_PATH);
            jTextFieldFilmTitle.setText(file);
            jTextFieldFilmPfad.setText(path);
        } else {
            jTextFieldFilmTitle.setText("");
            jTextFieldFilmPfad.setText("");
        }
    }

    private void filmLoeschen_() {
        String del = "";
        int row = tabelleFilme.getSelectedRow();
        if (row < 0) {
            new HinweisKeineAuswahl().zeigen(parent);
            return;
        }
        try {
            String file = (String) tabelleFilme.getModel().getValueAt(tabelleFilme.convertRowIndexToModel(row), DatenMediaDB.MEDIA_DB_NAME);
            String path = (String) tabelleFilme.getModel().getValueAt(tabelleFilme.convertRowIndexToModel(row), DatenMediaDB.MEDIA_DB_PATH);
            del = path + File.separator + file;
            File delFile = new File(del);
            if (!delFile.exists()) {
                MVMessageDialog.showMessageDialog(parent, "Die Datei existiert nicht!", "Film löschen", JOptionPane.ERROR_MESSAGE);
                return;
            }
            int ret = JOptionPane.showConfirmDialog(parent, delFile.getAbsolutePath(), "Film Löschen?", JOptionPane.YES_NO_OPTION);
            if (ret != JOptionPane.OK_OPTION) {
                return;
            }

            // und jetzt die Datei löschen
            SysMsg.sysMsg(new String[]{"Datei löschen: ", delFile.getAbsolutePath()});
            if (!delFile.delete()) {
                throw new Exception();
            }
        } catch (Exception ex) {
            MVMessageDialog.showMessageDialog(parent, "Konnte die Datei nicht löschen!", "Film löschen", JOptionPane.ERROR_MESSAGE);
            Log.errorLog(984512036, "Fehler beim löschen: " + del);
        }
    }

    private void beenden() {
        MVConfig.add(MVConfig.Configs.SYSTEM_MEDIA_DB_DIALOG_ANZEIGEN, Boolean.FALSE.toString());
        setVis();
    }

    // <editor-fold defaultstate="collapsed" desc="Generated Code">//GEN-BEGIN:initComponents
    private void initComponents() {

        jButtonBeenden = new javax.swing.JButton();
        jButtonHelp = new javax.swing.JButton();
        jTextFieldSearch = new javax.swing.JTextField();
        jButtonSearch = new javax.swing.JButton();
        jScrollPane3 = new javax.swing.JScrollPane();
        javax.swing.JTable jTableFilm = new javax.swing.JTable();
        javax.swing.JLabel jLabel1 = new javax.swing.JLabel();
        jLabelSizeFound = new javax.swing.JLabel();
        javax.swing.JLabel jLabel4 = new javax.swing.JLabel();
        jButtonIndex = new javax.swing.JButton();
        jLabelSum = new javax.swing.JLabel();
        progress = new javax.swing.JProgressBar();
        javax.swing.JLabel jLabel3 = new javax.swing.JLabel();
        jTextFieldFilmTitle = new javax.swing.JTextField();
        javax.swing.JLabel jLabel5 = new javax.swing.JLabel();
        jTextFieldFilmPfad = new javax.swing.JTextField();

        setDefaultCloseOperation(javax.swing.WindowConstants.DISPOSE_ON_CLOSE);

        jButtonBeenden.setText("Ok");

        jButtonHelp.setIcon(new javax.swing.ImageIcon(getClass().getResource("/mediathek/res/muster/button-help.png"))); // NOI18N
        jButtonHelp.setToolTipText("Hilfe anzeigen");

        jButtonSearch.setText("Suchen");

        jTableFilm.setAutoCreateRowSorter(true);
        jTableFilm.setModel(new javax.swing.table.DefaultTableModel(
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
        jScrollPane3.setViewportView(jTableFilm);

        jLabel1.setText("Treffer:");

        jLabelSizeFound.setText("0");

        jLabel4.setText("Anzahl Medien gesamt:");

        jButtonIndex.setText("Index neu aufbauen");

        jLabelSum.setText("0");

        jLabel3.setText("Titel:");

        jTextFieldFilmTitle.setEditable(false);

        jLabel5.setText("Pfad:");

        jTextFieldFilmPfad.setEditable(false);

        javax.swing.GroupLayout layout = new javax.swing.GroupLayout(getContentPane());
        getContentPane().setLayout(layout);
        layout.setHorizontalGroup(
            layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(layout.createSequentialGroup()
                .addContainerGap()
                .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addComponent(jScrollPane3, javax.swing.GroupLayout.DEFAULT_SIZE, 536, Short.MAX_VALUE)
                    .addGroup(layout.createSequentialGroup()
                        .addComponent(jTextFieldSearch)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                        .addComponent(jButtonSearch)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                        .addComponent(jButtonHelp))
                    .addGroup(javax.swing.GroupLayout.Alignment.TRAILING, layout.createSequentialGroup()
                        .addComponent(progress, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                        .addComponent(jButtonIndex)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                        .addComponent(jButtonBeenden, javax.swing.GroupLayout.PREFERRED_SIZE, 93, javax.swing.GroupLayout.PREFERRED_SIZE))
                    .addGroup(layout.createSequentialGroup()
                        .addComponent(jLabel4)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                        .addComponent(jLabelSum)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                        .addComponent(jLabel1)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                        .addComponent(jLabelSizeFound))
                    .addGroup(layout.createSequentialGroup()
                        .addComponent(jLabel3)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                        .addComponent(jTextFieldFilmTitle))
                    .addGroup(layout.createSequentialGroup()
                        .addComponent(jLabel5)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                        .addComponent(jTextFieldFilmPfad)))
                .addContainerGap())
        );
        layout.setVerticalGroup(
            layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(javax.swing.GroupLayout.Alignment.TRAILING, layout.createSequentialGroup()
                .addContainerGap()
                .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                        .addComponent(jTextFieldSearch, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                        .addComponent(jButtonSearch))
                    .addComponent(jButtonHelp))
                .addGap(18, 18, 18)
                .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                        .addComponent(jLabel4)
                        .addComponent(jLabelSum))
                    .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                        .addComponent(jLabel1)
                        .addComponent(jLabelSizeFound)))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(jScrollPane3, javax.swing.GroupLayout.DEFAULT_SIZE, 490, Short.MAX_VALUE)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(jLabel3)
                    .addComponent(jTextFieldFilmTitle, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(jLabel5)
                    .addComponent(jTextFieldFilmPfad, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.CENTER)
                    .addComponent(progress, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                    .addComponent(jButtonIndex)
                    .addComponent(jButtonBeenden))
                .addContainerGap())
        );

        layout.linkSize(javax.swing.SwingConstants.VERTICAL, new java.awt.Component[] {jButtonHelp, jButtonSearch, jTextFieldSearch});

        pack();
    }// </editor-fold>//GEN-END:initComponents
    // Variables declaration - do not modify//GEN-BEGIN:variables
    private javax.swing.JButton jButtonBeenden;
    private javax.swing.JButton jButtonHelp;
    private javax.swing.JButton jButtonIndex;
    private javax.swing.JButton jButtonSearch;
    private javax.swing.JLabel jLabelSizeFound;
    private javax.swing.JLabel jLabelSum;
    private javax.swing.JScrollPane jScrollPane3;
    private javax.swing.JTextField jTextFieldFilmPfad;
    private javax.swing.JTextField jTextFieldFilmTitle;
    private javax.swing.JTextField jTextFieldSearch;
    private javax.swing.JProgressBar progress;
    // End of variables declaration//GEN-END:variables

    private class BeobTableSelect implements ListSelectionListener {

        @Override
        public void valueChanged(ListSelectionEvent event) {
            if (!event.getValueIsAdjusting()) {
                aktFilmSetzen();
            }
        }
    }

    private class BeobDoc implements DocumentListener {

        @Override
        public void insertUpdate(DocumentEvent e
        ) {
            tus();
        }

        @Override
        public void removeUpdate(DocumentEvent e
        ) {
            tus();
        }

        @Override
        public void changedUpdate(DocumentEvent e
        ) {
            tus();
        }

        private void tus() {
            Filter.checkPattern1(jTextFieldSearch);
            if (Boolean.parseBoolean(MVConfig.get(MVConfig.Configs.SYSTEM_MEDIA_DB_ECHTZEITSUCHE))) {
                searchFilmInDb();
            }
        }
    }

    public class BeobMausTabelle extends MouseAdapter {

        private Point p;

        @Override
        public void mousePressed(MouseEvent arg0) {
            if (arg0.isPopupTrigger()) {
                showMenu(arg0);
            }
        }

        @Override
        public void mouseReleased(MouseEvent arg0) {
            if (arg0.isPopupTrigger()) {
                showMenu(arg0);
            }
        }

        private void showMenu(MouseEvent evt) {
            p = evt.getPoint();
            int nr = tabelleFilme.rowAtPoint(p);
            if (nr >= 0) {
                tabelleFilme.setRowSelectionInterval(nr, nr);
            }
            JPopupMenu jPopupMenu = new JPopupMenu();

            // Film abspielen
            JMenuItem itemPlayerDownload = new JMenuItem("gespeicherten Film (Datei) abspielen");
            itemPlayerDownload.setIcon(Icons.ICON_MENUE_FILM_START);
            itemPlayerDownload.addActionListener(e -> filmAbspielen_());
            jPopupMenu.add(itemPlayerDownload);

            // Film löschen
            JMenuItem itemDeleteDownload = new JMenuItem("gespeicherten Film (Datei) löschen");
            itemDeleteDownload.setIcon(Icons.ICON_BUTTON_DEL);
            itemDeleteDownload.addActionListener(e -> filmLoeschen_());
            jPopupMenu.add(itemDeleteDownload);

            // Zielordner öffnen
            JMenuItem itemOeffnen = new JMenuItem("Zielordner öffnen");
            itemOeffnen.setIcon(Icons.ICON_MENUE_FILE_OPEN);
            jPopupMenu.add(itemOeffnen);
            itemOeffnen.addActionListener(arg0 -> zielordnerOeffnen());

            jPopupMenu.addSeparator();
            // Reset Tabelle
            JMenuItem itemResetTab = new JMenuItem("Tabelle zurücksetzen");
            jPopupMenu.add(itemResetTab);
            itemResetTab.addActionListener(arg0 -> tabelleFilme.resetTabelle());

            // ######################
            // Menü anzeigen
            jPopupMenu.show(evt.getComponent(), evt.getX(), evt.getY());
        }

    }

}
