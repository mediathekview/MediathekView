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

import com.jidesoft.utils.SystemInfo;
import java.awt.FileDialog;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.File;
import javax.swing.JFileChooser;
import javax.swing.JFrame;
import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;
import mediathek.controller.Log;
import mediathek.daten.Daten;
import mediathek.file.GetFile;
import mediathek.gui.PanelVorlage;
import mediathek.gui.dialog.DialogHilfe;
import mediathek.res.GetIcon;
import mediathek.tool.HinweisKeineAuswahl;
import mediathek.tool.ListenerMediathekView;
import mediathek.tool.MVConfig;
import mediathek.tool.TModel;

public class PanelMediaDB extends PanelVorlage {

    private final TModel modelPath = new TModel(new Object[][]{}, new String[]{"Pfad"});

    public PanelMediaDB(Daten d, JFrame parent) {
        super(d, parent);
        initComponents();
        daten = d;

        ListenerMediathekView.addListener(new ListenerMediathekView(ListenerMediathekView.EREIGNIS_MEDIA_DB_START, PanelMediaDB.class.getSimpleName()) {
            @Override
            public void ping() {
                // neue DB suchen
                setIndex(false);
            }
        });
        ListenerMediathekView.addListener(new ListenerMediathekView(ListenerMediathekView.EREIGNIS_MEDIA_DB_STOP, PanelMediaDB.class.getSimpleName()) {
            @Override
            public void ping() {
                // neue DB liegt vor
                jLabelSizeIndex.setText(Daten.mVMediaDB.getSizeFileArray() + "");
                setIndex(true);
            }
        });

        progress.setVisible(false);
        progress.setIndeterminate(true);
        progress.setMaximum(0);
        progress.setMinimum(0);
        progress.setValue(0);
        jTablePath.setModel(modelPath);
        jTextFieldSuffix.setText(Daten.mVConfig.get(MVConfig.SYSTEM_MEDIA_DB_SUFFIX));
        jTextFieldSuffix.getDocument().addDocumentListener(new DocumentListener() {

            @Override
            public void insertUpdate(DocumentEvent e) {
                Daten.mVConfig.add(MVConfig.SYSTEM_MEDIA_DB_SUFFIX, jTextFieldSuffix.getText());
            }

            @Override
            public void removeUpdate(DocumentEvent e) {
                Daten.mVConfig.add(MVConfig.SYSTEM_MEDIA_DB_SUFFIX, jTextFieldSuffix.getText());
            }

            @Override
            public void changedUpdate(DocumentEvent e) {
                Daten.mVConfig.add(MVConfig.SYSTEM_MEDIA_DB_SUFFIX, jTextFieldSuffix.getText());
            }
        });
        jCheckBoxMediaDB.setSelected(Boolean.parseBoolean(Daten.mVConfig.get(MVConfig.SYSTEM_MEDIA_DB_ECHTZEITSUCHE)));
        jCheckBoxMediaDB.addActionListener(new ActionListener() {

            @Override
            public void actionPerformed(ActionEvent ae) {
                Daten.mVConfig.add(MVConfig.SYSTEM_MEDIA_DB_ECHTZEITSUCHE, Boolean.toString(jCheckBoxMediaDB.isSelected()));
            }
        });

        jButtonMakeIndex.addActionListener(new ActionListener() {

            @Override
            public void actionPerformed(ActionEvent e) {
                jLabelSizeIndex.setText("0");
                Daten.mVMediaDB.makeIndex();
            }
        });
        jButtonPath.setIcon(GetIcon.getProgramIcon("fileopen_16.png"));
        jButtonAdd.setIcon(GetIcon.getProgramIcon("add_16.png"));
        jButtonRemove.setIcon(GetIcon.getProgramIcon("remove_16.png"));
        jButtonPath.addActionListener(new BeobPfad());
        jButtonAdd.addActionListener(new ActionListener() {

            @Override
            public void actionPerformed(ActionEvent e) {
                addPath();
            }
        });
        jButtonRemove.addActionListener(new ActionListener() {

            @Override
            public void actionPerformed(ActionEvent e) {
                removePath();
            }
        }
        );
        jButtonHelp.setIcon(GetIcon.getProgramIcon("help_16.png"));
        jButtonHelp.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                new DialogHilfe(daten.mediathekGui, true, new GetFile().getHilfeSuchen(GetFile.PFAD_HILFETEXT_DIALOG_MEDIA_DB)).setVisible(true);
            }
        });
        setTablePath();
    }

    private void setIndex(boolean noIndex) {
        progress.setVisible(!noIndex);
        jTextFieldPath.setEnabled(noIndex);
        jButtonMakeIndex.setEnabled(noIndex);
        jButtonAdd.setEnabled(noIndex);
        jButtonPath.setEnabled(noIndex);
        jButtonRemove.setEnabled(noIndex);
    }

    private void addPath() {
        String db = Daten.mVConfig.get(MVConfig.SYSTEM_MEDIA_DB_PATH_MEDIA);
        String add = jTextFieldPath.getText();
        if (add.isEmpty()) {
            return;
        }
        for (String s : db.split(Daten.mVMediaDB.FILE_TRENNER)) {
            if (s.equals(add)) {
                return; // dann gibts den schon
            }
        }
        if (db.isEmpty()) {
            db = add;
        } else {
            db += Daten.mVMediaDB.FILE_TRENNER + add;
        }
        Daten.mVConfig.add(MVConfig.SYSTEM_MEDIA_DB_PATH_MEDIA, db);
        setTablePath(); //neu aufbauen
    }

    private void removePath() {
        int row = jTablePath.getSelectedRow();
        if (row >= 0) {
            String p = jTablePath.getModel().getValueAt(jTablePath.convertRowIndexToModel(row), 0).toString();
            String db = Daten.mVConfig.get(MVConfig.SYSTEM_MEDIA_DB_PATH_MEDIA);
            String dbNew = "";
            if (db.isEmpty()) {
                return;
            }
            for (String s : db.split(Daten.mVMediaDB.FILE_TRENNER)) {
                if (s.equals(p)) {
                    continue;
                }
                dbNew += dbNew.isEmpty() ? s : Daten.mVMediaDB.FILE_TRENNER + s;
            }
            Daten.mVConfig.add(MVConfig.SYSTEM_MEDIA_DB_PATH_MEDIA, dbNew);
            setTablePath(); //neu aufbauen
        } else {
            new HinweisKeineAuswahl().zeigen(daten.mediathekGui);
        }
    }

    private synchronized void setTablePath() {
        // Tabelle mit den Pfaden bauen
        String db = Daten.mVConfig.get(MVConfig.SYSTEM_MEDIA_DB_PATH_MEDIA);
        modelPath.setRowCount(0);
        if (!db.isEmpty()) {
            for (String s : db.split(Daten.mVMediaDB.FILE_TRENNER)) {
                modelPath.addRow(new Object[]{s});
            }
        }
    }


    // <editor-fold defaultstate="collapsed" desc="Generated Code">//GEN-BEGIN:initComponents
    private void initComponents() {

        jLabel1 = new javax.swing.JLabel();
        progress = new javax.swing.JProgressBar();
        jButtonMakeIndex = new javax.swing.JButton();
        jScrollPane1 = new javax.swing.JScrollPane();
        jTablePath = new javax.swing.JTable();
        jLabel2 = new javax.swing.JLabel();
        jLabelSizeIndex = new javax.swing.JLabel();
        jTextFieldPath = new javax.swing.JTextField();
        jButtonRemove = new javax.swing.JButton();
        jButtonAdd = new javax.swing.JButton();
        jButtonPath = new javax.swing.JButton();
        jCheckBoxMediaDB = new javax.swing.JCheckBox();
        jLabel3 = new javax.swing.JLabel();
        jTextFieldSuffix = new javax.swing.JTextField();
        jButtonHelp = new javax.swing.JButton();

        setMinimumSize(getPreferredSize());

        jLabel1.setText("In den Pfaden suchen:");

        jButtonMakeIndex.setText("Index neu aufbauen");
        jButtonMakeIndex.setToolTipText("");

        jTablePath.setModel(new javax.swing.table.DefaultTableModel(
            new Object [][] {

            },
            new String [] {

            }
        ));
        jScrollPane1.setViewportView(jTablePath);

        jLabel2.setText("Anzahl Dateien im Index:");

        jLabelSizeIndex.setText("0");

        jButtonRemove.setIcon(new javax.swing.ImageIcon(getClass().getResource("/mediathek/res/programm/remove_16.png"))); // NOI18N

        jButtonAdd.setIcon(new javax.swing.ImageIcon(getClass().getResource("/mediathek/res/programm/add_16.png"))); // NOI18N

        jButtonPath.setIcon(new javax.swing.ImageIcon(getClass().getResource("/mediathek/res/programm/fileopen_16.png"))); // NOI18N

        jCheckBoxMediaDB.setText("Echtzeitsuche in der Mediensammlung");

        jLabel3.setText("Suffix ausnehmen ( z.B. txt,xml):");

        jTextFieldSuffix.setText("txt,xml");

        jButtonHelp.setIcon(new javax.swing.ImageIcon(getClass().getResource("/mediathek/res/programm/help_16.png"))); // NOI18N

        javax.swing.GroupLayout layout = new javax.swing.GroupLayout(this);
        this.setLayout(layout);
        layout.setHorizontalGroup(
            layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(layout.createSequentialGroup()
                .addContainerGap()
                .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addComponent(jScrollPane1, javax.swing.GroupLayout.DEFAULT_SIZE, 568, Short.MAX_VALUE)
                    .addGroup(javax.swing.GroupLayout.Alignment.TRAILING, layout.createSequentialGroup()
                        .addComponent(jTextFieldPath)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                        .addComponent(jButtonPath)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                        .addComponent(jButtonAdd)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                        .addComponent(jButtonRemove)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                        .addComponent(jButtonHelp))
                    .addGroup(layout.createSequentialGroup()
                        .addComponent(jLabel2)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                        .addComponent(jLabelSizeIndex)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                        .addComponent(progress, javax.swing.GroupLayout.PREFERRED_SIZE, 162, javax.swing.GroupLayout.PREFERRED_SIZE)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                        .addComponent(jButtonMakeIndex))
                    .addGroup(layout.createSequentialGroup()
                        .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                            .addComponent(jLabel1)
                            .addComponent(jCheckBoxMediaDB))
                        .addGap(0, 0, Short.MAX_VALUE))
                    .addGroup(layout.createSequentialGroup()
                        .addComponent(jLabel3)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                        .addComponent(jTextFieldSuffix)))
                .addContainerGap())
        );
        layout.setVerticalGroup(
            layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(layout.createSequentialGroup()
                .addContainerGap()
                .addComponent(jCheckBoxMediaDB)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(jLabel3)
                    .addComponent(jTextFieldSuffix, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE))
                .addGap(18, 18, 18)
                .addComponent(jLabel1)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(jScrollPane1, javax.swing.GroupLayout.DEFAULT_SIZE, 203, Short.MAX_VALUE)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addGroup(javax.swing.GroupLayout.Alignment.TRAILING, layout.createSequentialGroup()
                        .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.CENTER)
                            .addComponent(jLabel2)
                            .addComponent(jLabelSizeIndex)
                            .addComponent(progress, javax.swing.GroupLayout.PREFERRED_SIZE, 14, javax.swing.GroupLayout.PREFERRED_SIZE)
                            .addComponent(jButtonMakeIndex))
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                        .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                            .addComponent(jTextFieldPath, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                            .addComponent(jButtonRemove)
                            .addComponent(jButtonAdd)
                            .addComponent(jButtonPath)))
                    .addComponent(jButtonHelp, javax.swing.GroupLayout.Alignment.TRAILING))
                .addContainerGap())
        );

        layout.linkSize(javax.swing.SwingConstants.VERTICAL, new java.awt.Component[] {jButtonAdd, jButtonPath, jButtonRemove, jTextFieldPath});

    }// </editor-fold>//GEN-END:initComponents


    // Variables declaration - do not modify//GEN-BEGIN:variables
    private javax.swing.JButton jButtonAdd;
    private javax.swing.JButton jButtonHelp;
    private javax.swing.JButton jButtonMakeIndex;
    private javax.swing.JButton jButtonPath;
    private javax.swing.JButton jButtonRemove;
    private javax.swing.JCheckBox jCheckBoxMediaDB;
    private javax.swing.JLabel jLabel1;
    private javax.swing.JLabel jLabel2;
    private javax.swing.JLabel jLabel3;
    private javax.swing.JLabel jLabelSizeIndex;
    private javax.swing.JScrollPane jScrollPane1;
    private javax.swing.JTable jTablePath;
    private javax.swing.JTextField jTextFieldPath;
    private javax.swing.JTextField jTextFieldSuffix;
    private javax.swing.JProgressBar progress;
    // End of variables declaration//GEN-END:variables

    private class BeobPfad implements ActionListener {

        @Override
        public void actionPerformed(ActionEvent e) {
            //we can use native directory chooser on Mac...
            if (SystemInfo.isMacOSX()) {
                //we want to select a directory only, so temporarily change properties
                System.setProperty("apple.awt.fileDialogForDirectories", "true");
                FileDialog chooser = new FileDialog(daten.mediathekGui, "Pfad zu den Filmen wählen");
                chooser.setVisible(true);
                if (chooser.getFile() != null) {
                    //A directory was selected, that means Cancel was not pressed
                    try {
                        jTextFieldPath.setText(new File(chooser.getDirectory() + chooser.getFile()).getAbsolutePath());
                    } catch (Exception ex) {
                        Log.fehlerMeldung(951024789, ex);
                    }
                }
                System.setProperty("apple.awt.fileDialogForDirectories", "false");
            } else {
                //use the cross-platform swing chooser
                int returnVal;
                JFileChooser chooser = new JFileChooser();
                chooser.setFileSelectionMode(JFileChooser.DIRECTORIES_ONLY);
                if (!jTextFieldPath.getText().equals("")) {
                    chooser.setCurrentDirectory(new File(jTextFieldPath.getText()));
                }
                returnVal = chooser.showOpenDialog(null);
                if (returnVal == JFileChooser.APPROVE_OPTION) {
                    try {
                        jTextFieldPath.setText(chooser.getSelectedFile().getPath());
                    } catch (Exception ex) {
                        Log.fehlerMeldung(765212369, ex);
                    }
                }
            }
        }
    }

}
