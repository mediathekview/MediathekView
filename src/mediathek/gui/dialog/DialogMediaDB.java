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

import com.jidesoft.utils.SystemInfo;
import java.awt.Cursor;
import java.awt.FileDialog;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.File;
import java.util.ArrayList;
import java.util.regex.Pattern;
import javax.swing.JFileChooser;
import javax.swing.JFrame;
import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;
import mediathek.controller.Log;
import mediathek.daten.Daten;
import mediathek.res.GetIcon;
import mediathek.tool.EscBeenden;
import mediathek.tool.Filter;
import static mediathek.tool.Filter.makePattern;
import mediathek.tool.HinweisKeineAuswahl;
import mediathek.tool.MVConfig;
import mediathek.tool.TModel;

public class DialogMediaDB extends javax.swing.JDialog {

    private final ArrayList<String[]> fileArray = new ArrayList<>();//name-path
    private final ArrayList<String[]> erg = new ArrayList<>();//name-path
    private final TModel modelFilm = new TModel(new Object[][]{}, new String[]{"Name", "Pfad"});
    private final TModel modelPath = new TModel(new Object[][]{}, new String[]{"Pfad"});
    private final JFrame parent;
    private final String FILE_TRENNER = "<>";

    public DialogMediaDB(JFrame pparent, String titel) {
        super(pparent, true);
        initComponents();
        this.parent = pparent;
        parent.setCursor(Cursor.getPredefinedCursor(Cursor.WAIT_CURSOR));
        jButtonPath.setIcon(GetIcon.getProgramIcon("fileopen_16.png"));
        jButtonAdd.setIcon(GetIcon.getProgramIcon("add_16.png"));
        jButtonRemove.setIcon(GetIcon.getProgramIcon("remove_16.png"));
        jButtonPath.addActionListener(new BeobPfad());
        jButtonAdd.addActionListener(new ActionListener() {

            @Override
            public void actionPerformed(ActionEvent e) {
                String db = Daten.mVConfig.get(MVConfig.SYSTEM_PATH_MEDIA);
                String add = jTextFieldPath.getText();
                if (add.isEmpty()) {
                    return;
                }
                for (String s : db.split(FILE_TRENNER)) {
                    if (s.equals(add)) {
                        return; // dann gibts den schon
                    }
                }
                if (db.isEmpty()) {
                    db = add;
                } else {
                    db += FILE_TRENNER + add;
                }
                Daten.mVConfig.add(MVConfig.SYSTEM_PATH_MEDIA, db);
                setTablePath(); //neu aufbauen
            }
        });
        jButtonRemove.addActionListener(new ActionListener() {

            @Override
            public void actionPerformed(ActionEvent e) {
                int row = jTablePath.getSelectedRow();
                if (row >= 0) {
                    String p = jTablePath.getModel().getValueAt(jTablePath.convertRowIndexToModel(row), 0).toString();
                    String db = Daten.mVConfig.get(MVConfig.SYSTEM_PATH_MEDIA);
                    String dbNew = "";
                    if (db.isEmpty()) {
                        return;
                    }
                    for (String s : db.split(FILE_TRENNER)) {
                        if (s.equals(p)) {
                            continue;
                        }
                        dbNew += dbNew.isEmpty() ? s : FILE_TRENNER + s;
                    }
                    Daten.mVConfig.add(MVConfig.SYSTEM_PATH_MEDIA, dbNew);
                    setTablePath(); //neu aufbauen
                } else {
                    new HinweisKeineAuswahl().zeigen(parent);
                }
            }
        }
        );
        // =====================
        jTextFieldTitle.setText(titel);
        jTextFieldTitle.getDocument()
                .addDocumentListener(new DocumentListener() {
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
                        Filter.checkPattern1(jTextFieldTitle);
                        if (Boolean.parseBoolean(Daten.mVConfig.get(MVConfig.SYSTEM_ECHTZEITSUCHE))) {
                            search();
                        }
                    }
                }
                );
        jButtonSearch.addActionListener(new ActionListener() {

            @Override
            public void actionPerformed(ActionEvent e) {
                search();
            }
        });
        jButtonBeenden.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                beenden();
            }
        });
        getRootPane().setDefaultButton(jButtonBeenden);
        new EscBeenden(this) {
            @Override
            public void beenden_() {
                beenden();
            }
        };
        jTableFilm.setModel(modelFilm);
        jTablePath.setModel(modelPath);
        setTablePath();
        parent.setCursor(Cursor.getPredefinedCursor(Cursor.DEFAULT_CURSOR));
    }

    private void setFileArray() {
        fileArray.clear();
        String db = Daten.mVConfig.get(MVConfig.SYSTEM_PATH_MEDIA);
        if (!db.isEmpty()) {
            for (String s : db.split(FILE_TRENNER)) {
                File f = new File(s);
                searchFile(f);
            }
        }
    }

    private synchronized void setTablePath() {
        String db = Daten.mVConfig.get(MVConfig.SYSTEM_PATH_MEDIA);
        modelPath.setRowCount(0);
        if (!db.isEmpty()) {
            for (String s : db.split(FILE_TRENNER)) {
                modelPath.addRow(new Object[]{s});
            }
        }
        setFileArray();
        search();
    }

    private void search() {
        String title = jTextFieldTitle.getText();
        modelFilm.setRowCount(0);
        erg.clear();
        Pattern p = makePattern(title);
        if (p != null) {
            // dann mit RegEx prüfen
            for (String[] s : fileArray) {
                if (p.matcher(s[0]).matches()) {
                    erg.add(s);
                }
            }
        } else {
            title = title.toLowerCase();
            for (String[] s : fileArray) {
                if (s[0].toLowerCase().contains(title)) {
                    erg.add(s);
                }
            }
        }
        if (erg.size() > 0) {
            Object[] o;
            for (int i = 0; i < erg.size(); ++i) {
                o = erg.get(i);
                modelFilm.addRow(o);
            }
        }
        jLabelCount.setText(modelFilm.getRowCount() + "");
    }

    private void beenden() {
        this.dispose();
    }

    private void searchFile(File dir) {
        if (dir == null) {
            return;
        }
        File[] files = dir.listFiles();
        if (files != null) {
            for (File file : files) {
                if (file.isDirectory()) {
                    searchFile(file);
                } else {
                    fileArray.add(new String[]{file.getName(), file.getParent()});
                }
            }
        }
    }

    // <editor-fold defaultstate="collapsed" desc="Generated Code">//GEN-BEGIN:initComponents
    private void initComponents() {

        jLabel2 = new javax.swing.JLabel();
        jTextFieldTitle = new javax.swing.JTextField();
        jButtonSearch = new javax.swing.JButton();
        jSplitPane1 = new javax.swing.JSplitPane();
        jPanel1 = new javax.swing.JPanel();
        jLabel3 = new javax.swing.JLabel();
        jScrollPane2 = new javax.swing.JScrollPane();
        jTablePath = new javax.swing.JTable();
        jTextFieldPath = new javax.swing.JTextField();
        jButtonPath = new javax.swing.JButton();
        jButtonAdd = new javax.swing.JButton();
        jButtonRemove = new javax.swing.JButton();
        jPanel2 = new javax.swing.JPanel();
        jScrollPane3 = new javax.swing.JScrollPane();
        jTableFilm = new javax.swing.JTable();
        jLabel1 = new javax.swing.JLabel();
        jLabelCount = new javax.swing.JLabel();
        jLabel4 = new javax.swing.JLabel();
        jButtonBeenden = new javax.swing.JButton();

        jLabel2.setText("jLabel2");

        setDefaultCloseOperation(javax.swing.WindowConstants.DISPOSE_ON_CLOSE);

        jButtonSearch.setText("suchen");

        jSplitPane1.setDividerLocation(200);
        jSplitPane1.setOrientation(javax.swing.JSplitPane.VERTICAL_SPLIT);

        jLabel3.setText("In den Pfaden suchen:");

        jTablePath.setModel(new javax.swing.table.DefaultTableModel(
            new Object [][] {
                {null}
            },
            new String [] {
                "Title 1"
            }
        ));
        jScrollPane2.setViewportView(jTablePath);

        jButtonPath.setIcon(new javax.swing.ImageIcon(getClass().getResource("/mediathek/res/programm/fileopen_16.png"))); // NOI18N

        jButtonAdd.setIcon(new javax.swing.ImageIcon(getClass().getResource("/mediathek/res/programm/add_16.png"))); // NOI18N

        jButtonRemove.setIcon(new javax.swing.ImageIcon(getClass().getResource("/mediathek/res/programm/remove_16.png"))); // NOI18N

        javax.swing.GroupLayout jPanel1Layout = new javax.swing.GroupLayout(jPanel1);
        jPanel1.setLayout(jPanel1Layout);
        jPanel1Layout.setHorizontalGroup(
            jPanel1Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel1Layout.createSequentialGroup()
                .addContainerGap()
                .addGroup(jPanel1Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addComponent(jScrollPane2, javax.swing.GroupLayout.Alignment.TRAILING)
                    .addGroup(jPanel1Layout.createSequentialGroup()
                        .addComponent(jTextFieldPath)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                        .addComponent(jButtonPath)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                        .addComponent(jButtonAdd)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                        .addComponent(jButtonRemove))
                    .addGroup(jPanel1Layout.createSequentialGroup()
                        .addComponent(jLabel3)
                        .addGap(0, 0, Short.MAX_VALUE)))
                .addContainerGap())
        );
        jPanel1Layout.setVerticalGroup(
            jPanel1Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel1Layout.createSequentialGroup()
                .addContainerGap()
                .addComponent(jLabel3)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(jScrollPane2, javax.swing.GroupLayout.DEFAULT_SIZE, 116, Short.MAX_VALUE)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                .addGroup(jPanel1Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(jTextFieldPath, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                    .addComponent(jButtonAdd)
                    .addComponent(jButtonPath)
                    .addComponent(jButtonRemove))
                .addContainerGap())
        );

        jPanel1Layout.linkSize(javax.swing.SwingConstants.VERTICAL, new java.awt.Component[] {jButtonAdd, jButtonPath, jTextFieldPath});

        jSplitPane1.setTopComponent(jPanel1);

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

        jLabel1.setText("Anzahl:");

        jLabelCount.setText("0");

        jLabel4.setText("Filme:");

        javax.swing.GroupLayout jPanel2Layout = new javax.swing.GroupLayout(jPanel2);
        jPanel2.setLayout(jPanel2Layout);
        jPanel2Layout.setHorizontalGroup(
            jPanel2Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel2Layout.createSequentialGroup()
                .addContainerGap()
                .addGroup(jPanel2Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addComponent(jScrollPane3, javax.swing.GroupLayout.DEFAULT_SIZE, 495, Short.MAX_VALUE)
                    .addGroup(jPanel2Layout.createSequentialGroup()
                        .addGroup(jPanel2Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                            .addGroup(jPanel2Layout.createSequentialGroup()
                                .addComponent(jLabel1)
                                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                                .addComponent(jLabelCount))
                            .addComponent(jLabel4))
                        .addGap(0, 0, Short.MAX_VALUE)))
                .addContainerGap())
        );
        jPanel2Layout.setVerticalGroup(
            jPanel2Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel2Layout.createSequentialGroup()
                .addContainerGap()
                .addComponent(jLabel4)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(jScrollPane3, javax.swing.GroupLayout.DEFAULT_SIZE, 279, Short.MAX_VALUE)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addGroup(jPanel2Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.CENTER)
                    .addComponent(jLabel1)
                    .addComponent(jLabelCount))
                .addContainerGap())
        );

        jSplitPane1.setRightComponent(jPanel2);

        jButtonBeenden.setText("Ok");

        javax.swing.GroupLayout layout = new javax.swing.GroupLayout(getContentPane());
        getContentPane().setLayout(layout);
        layout.setHorizontalGroup(
            layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(layout.createSequentialGroup()
                .addContainerGap()
                .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addGroup(javax.swing.GroupLayout.Alignment.TRAILING, layout.createSequentialGroup()
                        .addComponent(jTextFieldTitle)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                        .addComponent(jButtonSearch))
                    .addComponent(jSplitPane1)
                    .addGroup(javax.swing.GroupLayout.Alignment.TRAILING, layout.createSequentialGroup()
                        .addGap(0, 0, Short.MAX_VALUE)
                        .addComponent(jButtonBeenden, javax.swing.GroupLayout.PREFERRED_SIZE, 93, javax.swing.GroupLayout.PREFERRED_SIZE)))
                .addContainerGap())
        );
        layout.setVerticalGroup(
            layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(javax.swing.GroupLayout.Alignment.TRAILING, layout.createSequentialGroup()
                .addContainerGap()
                .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(jTextFieldTitle, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                    .addComponent(jButtonSearch))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                .addComponent(jSplitPane1)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                .addComponent(jButtonBeenden)
                .addContainerGap())
        );

        layout.linkSize(javax.swing.SwingConstants.VERTICAL, new java.awt.Component[] {jButtonSearch, jTextFieldTitle});

        pack();
    }// </editor-fold>//GEN-END:initComponents
    // Variables declaration - do not modify//GEN-BEGIN:variables
    private javax.swing.JButton jButtonAdd;
    private javax.swing.JButton jButtonBeenden;
    private javax.swing.JButton jButtonPath;
    private javax.swing.JButton jButtonRemove;
    private javax.swing.JButton jButtonSearch;
    private javax.swing.JLabel jLabel1;
    private javax.swing.JLabel jLabel2;
    private javax.swing.JLabel jLabel3;
    private javax.swing.JLabel jLabel4;
    private javax.swing.JLabel jLabelCount;
    private javax.swing.JPanel jPanel1;
    private javax.swing.JPanel jPanel2;
    private javax.swing.JScrollPane jScrollPane2;
    private javax.swing.JScrollPane jScrollPane3;
    private javax.swing.JSplitPane jSplitPane1;
    private javax.swing.JTable jTableFilm;
    private javax.swing.JTable jTablePath;
    private javax.swing.JTextField jTextFieldPath;
    private javax.swing.JTextField jTextFieldTitle;
    // End of variables declaration//GEN-END:variables

    private class BeobPfad implements ActionListener {

        @Override
        public void actionPerformed(ActionEvent e) {
            //we can use native directory chooser on Mac...
            if (SystemInfo.isMacOSX()) {
                //we want to select a directory only, so temporarily change properties
                System.setProperty("apple.awt.fileDialogForDirectories", "true");
                FileDialog chooser = new FileDialog(parent, "Pfad zu den Filmen wählen");
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
