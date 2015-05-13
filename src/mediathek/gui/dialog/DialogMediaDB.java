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

import java.awt.Cursor;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import javax.swing.JFrame;
import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;
import mediathek.daten.Daten;
import mediathek.file.GetFile;
import mediathek.res.GetIcon;
import mediathek.tool.EscBeenden;
import mediathek.tool.FilenameUtils;
import mediathek.tool.Filter;
import mediathek.tool.GuiFunktionen;
import mediathek.tool.ListenerMediathekView;
import mediathek.tool.MVConfig;
import mediathek.tool.TModel;

public class DialogMediaDB extends javax.swing.JDialog {

    private final TModel modelFilm = new TModel(new Object[][]{}, new String[]{"Name", "Pfad"});
    private final JFrame parent;
    private boolean init = false;

    public DialogMediaDB(JFrame pparent) {
        super(pparent, false);
        initComponents();
        this.parent = pparent;
        this.addWindowListener(new WindowAdapter() {
            @Override
            public void windowClosing(WindowEvent e) {
                beenden();
            }
        });
        this.setTitle("Mediensammlung durchsuchen");
        ListenerMediathekView.addListener(new ListenerMediathekView(ListenerMediathekView.EREIGNIS_MEDIA_DB_START, DialogMediaDB.class.getSimpleName()) {
            @Override
            public void ping() {
                // neue DB suchen
                setIndex(false);
                jLabelSum.setText("0");
            }
        });
        ListenerMediathekView.addListener(new ListenerMediathekView(ListenerMediathekView.EREIGNIS_MEDIA_DB_STOP, DialogMediaDB.class.getSimpleName()) {
            @Override
            public void ping() {
                // neue DB liegt vor
                setIndex(true);
                jLabelSum.setText(Daten.mVMediaDB.getSizeFileArray() + "");
                search();
            }
        });
        ListenerMediathekView.addListener(new ListenerMediathekView(ListenerMediathekView.EREIGNIS_DIALOG_MEDIA_DB, DialogMediaDB.class.getName()) {
            @Override
            public void ping() {
                setVis();
            }
        });
        jTableFilm.setModel(modelFilm);

        jTextFieldTitle.addActionListener(new ActionListener() {

            @Override
            public void actionPerformed(ActionEvent e) {
                Daten.mVMediaDB.search(modelFilm, jTextFieldTitle.getText());
            }
        });
        jTextFieldTitle.getDocument().addDocumentListener(new BeobDoc());

        jButtonIndex.addActionListener(new ActionListener() {

            @Override
            public void actionPerformed(ActionEvent e) {
                Daten.mVMediaDB.makeIndex();
            }
        });

        jButtonHelp.setIcon(GetIcon.getProgramIcon("help_16.png"));
        jButtonHelp.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                new DialogHilfe(parent, true, new GetFile().getHilfeSuchen(GetFile.PFAD_HILFETEXT_DIALOG_MEDIA_DB)).setVisible(true);
            }
        });
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
        new EscBeenden(this) {
            @Override
            public void beenden_() {
                beenden();
            }
        };

        GuiFunktionen.setSize(MVConfig.SYSTEM_MEDIA_DB_DIALOG_GROESSE, this, parent);
    }

    @Override
    public void setVisible(boolean vis) {
        super.setVisible(vis);
        if (!init) {
            // beim ersten anzeigen den Index bauen
            Daten.mVMediaDB.makeIndex();
            init = true;
        }
    }

    public final void setVis() {
        this.setVisible(Boolean.parseBoolean(Daten.mVConfig.get(MVConfig.SYSTEM_MEDIA_DB_DIALOG_ANZEIGEN)));
    }

    public void setFilter(String titel) {
        titel = FilenameUtils.replaceLeerDateiname(titel); // mit den eingestellten Ersetzungen bearbeiten
        jTextFieldTitle.setText(titel);
    }

    private void search() {
        Daten.mVMediaDB.search(modelFilm, jTextFieldTitle.getText());
        jLabelSizeFound.setText(modelFilm.getRowCount() + "");
    }

    private void setIndex(boolean noIndex) {
        if (noIndex) {
            setCursor(Cursor.getPredefinedCursor(Cursor.DEFAULT_CURSOR));
        } else {
            setCursor(Cursor.getPredefinedCursor(Cursor.WAIT_CURSOR));
        }

        jTextFieldTitle.setEnabled(noIndex);
        jButtonSearch.setEnabled(noIndex);
        jButtonIndex.setEnabled(noIndex);
    }

    private void beenden() {
        Daten.mVConfig.add(MVConfig.SYSTEM_MEDIA_DB_DIALOG_ANZEIGEN, Boolean.FALSE.toString());
        ListenerMediathekView.notify(ListenerMediathekView.EREIGNIS_DIALOG_MEDIA_DB, DialogMediaDB.class.getName());
        this.setVisible(false);
    }

    // <editor-fold defaultstate="collapsed" desc="Generated Code">//GEN-BEGIN:initComponents
    private void initComponents() {

        jLabel2 = new javax.swing.JLabel();
        jButtonBeenden = new javax.swing.JButton();
        jButtonHelp = new javax.swing.JButton();
        jTextFieldTitle = new javax.swing.JTextField();
        jButtonSearch = new javax.swing.JButton();
        jScrollPane3 = new javax.swing.JScrollPane();
        jTableFilm = new javax.swing.JTable();
        jLabel1 = new javax.swing.JLabel();
        jLabelSizeFound = new javax.swing.JLabel();
        jLabel4 = new javax.swing.JLabel();
        jButtonIndex = new javax.swing.JButton();
        jLabelSum = new javax.swing.JLabel();

        jLabel2.setText("jLabel2");

        setDefaultCloseOperation(javax.swing.WindowConstants.DISPOSE_ON_CLOSE);

        jButtonBeenden.setText("Ok");

        jButtonHelp.setIcon(new javax.swing.ImageIcon(getClass().getResource("/mediathek/res/programm/help_16.png"))); // NOI18N

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

        jLabel1.setText("Anzahl:");

        jLabelSizeFound.setText("0");

        jLabel4.setText("Filme gesamt:");

        jButtonIndex.setText("Index neu aufbauen");

        jLabelSum.setText("0");

        javax.swing.GroupLayout layout = new javax.swing.GroupLayout(getContentPane());
        getContentPane().setLayout(layout);
        layout.setHorizontalGroup(
            layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(layout.createSequentialGroup()
                .addContainerGap()
                .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addComponent(jScrollPane3, javax.swing.GroupLayout.DEFAULT_SIZE, 580, Short.MAX_VALUE)
                    .addGroup(layout.createSequentialGroup()
                        .addComponent(jTextFieldTitle)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                        .addComponent(jButtonSearch))
                    .addGroup(javax.swing.GroupLayout.Alignment.TRAILING, layout.createSequentialGroup()
                        .addGap(0, 0, Short.MAX_VALUE)
                        .addComponent(jButtonHelp)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                        .addComponent(jButtonIndex)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                        .addComponent(jButtonBeenden, javax.swing.GroupLayout.PREFERRED_SIZE, 93, javax.swing.GroupLayout.PREFERRED_SIZE))
                    .addGroup(layout.createSequentialGroup()
                        .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                            .addGroup(layout.createSequentialGroup()
                                .addComponent(jLabel1)
                                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                                .addComponent(jLabelSizeFound))
                            .addGroup(layout.createSequentialGroup()
                                .addComponent(jLabel4)
                                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                                .addComponent(jLabelSum)))
                        .addGap(0, 0, Short.MAX_VALUE)))
                .addContainerGap())
        );
        layout.setVerticalGroup(
            layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(javax.swing.GroupLayout.Alignment.TRAILING, layout.createSequentialGroup()
                .addContainerGap()
                .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(jTextFieldTitle, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                    .addComponent(jButtonSearch))
                .addGap(18, 18, 18)
                .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(jLabel4)
                    .addComponent(jLabelSum))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(jScrollPane3, javax.swing.GroupLayout.DEFAULT_SIZE, 499, Short.MAX_VALUE)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(jLabel1)
                    .addComponent(jLabelSizeFound))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(jButtonBeenden)
                    .addComponent(jButtonHelp)
                    .addComponent(jButtonIndex))
                .addContainerGap())
        );

        layout.linkSize(javax.swing.SwingConstants.VERTICAL, new java.awt.Component[] {jButtonSearch, jTextFieldTitle});

        pack();
    }// </editor-fold>//GEN-END:initComponents
    // Variables declaration - do not modify//GEN-BEGIN:variables
    private javax.swing.JButton jButtonBeenden;
    private javax.swing.JButton jButtonHelp;
    private javax.swing.JButton jButtonIndex;
    private javax.swing.JButton jButtonSearch;
    private javax.swing.JLabel jLabel1;
    private javax.swing.JLabel jLabel2;
    private javax.swing.JLabel jLabel4;
    private javax.swing.JLabel jLabelSizeFound;
    private javax.swing.JLabel jLabelSum;
    private javax.swing.JScrollPane jScrollPane3;
    private javax.swing.JTable jTableFilm;
    private javax.swing.JTextField jTextFieldTitle;
    // End of variables declaration//GEN-END:variables

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
            Filter.checkPattern1(jTextFieldTitle);
            if (Boolean.parseBoolean(Daten.mVConfig.get(MVConfig.SYSTEM_MEDIA_DB_ECHTZEITSUCHE))) {
                search();
            }
        }
    }

}
