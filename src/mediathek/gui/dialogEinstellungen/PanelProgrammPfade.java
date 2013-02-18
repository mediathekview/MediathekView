/*    
 *    MediathekView
 *    Copyright (C) 2012   W. Xaver
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
import java.awt.Color;
import java.awt.FileDialog;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.File;
import javax.swing.JDialog;
import javax.swing.JFileChooser;
import javax.swing.JPanel;
import javax.swing.JTextField;
import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;
import mediathek.daten.DDaten;
import mediathek.daten.Daten;
import mediathek.file.GetFile;
import mediathek.gui.dialog.DialogHilfe;
import mediathek.tool.BeobWeb;
import mediathek.tool.GuiFunktionen;
import mediathek.tool.GuiFunktionenProgramme;
import mediathek.tool.Konstanten;
import mediathek.tool.Log;

public class PanelProgrammPfade extends JPanel {

    public JDialog dialog = null;
    private boolean vlc, flvstreamer, mplayer;
    private DDaten ddaten;

    public PanelProgrammPfade(DDaten dd, boolean vvlc, boolean fflvstreamer, boolean mmplayer) {
        initComponents();
        ddaten = dd;
        vlc = vvlc;
        flvstreamer = fflvstreamer;
        mplayer = mmplayer;
        init();
        initBeob();
    }

    private void init() {
        jPanelVlc.setVisible(vlc);
        jPanelFlv.setVisible(flvstreamer);
        jPanelMplayer.setVisible(mplayer);
        if (Daten.system[Konstanten.SYSTEM_PFAD_MPLAYER_NR].equals("")) {
            Daten.system[Konstanten.SYSTEM_PFAD_MPLAYER_NR] = GuiFunktionenProgramme.getMusterPfadMplayer();
        }
        if (Daten.system[Konstanten.SYSTEM_PFAD_VLC_NR].equals("")) {
            Daten.system[Konstanten.SYSTEM_PFAD_VLC_NR] = GuiFunktionenProgramme.getMusterPfadVlc();
        }
        if (Daten.system[Konstanten.SYSTEM_PFAD_FLVSTREAMER_NR].equals("")) {
            Daten.system[Konstanten.SYSTEM_PFAD_FLVSTREAMER_NR] = GuiFunktionenProgramme.getMusterPfadFlv();
        }
        jTextFieldVlc.setText(Daten.system[Konstanten.SYSTEM_PFAD_VLC_NR]);
        jTextFieldFlv.setText(Daten.system[Konstanten.SYSTEM_PFAD_FLVSTREAMER_NR]);
        jTextFieldMplayer.setText(Daten.system[Konstanten.SYSTEM_PFAD_MPLAYER_NR]);
    }

    private void initBeob() {
        jTextFieldVlc.getDocument().addDocumentListener(new BeobDoc());
        jTextFieldFlv.getDocument().addDocumentListener(new BeobDoc());
        jTextFieldMplayer.getDocument().addDocumentListener(new BeobDoc());
        jTextFieldUrlMplayer.setText(Konstanten.ADRESSE_WEBSITE_MPLAYER);
        jTextFieldUrlMplayer.addActionListener(new BeobWeb(Konstanten.ADRESSE_WEBSITE_MPLAYER));
        jTextFieldUrlVlc.setText(Konstanten.ADRESSE_WEBSITE_VLC);
        jTextFieldUrlVlc.addActionListener(new BeobWeb(Konstanten.ADRESSE_WEBSITE_VLC));
        jTextFieldUrlFlv.setText(Konstanten.ADRESSE_WEBSITE_FLVSTREAMER);
        jTextFieldUrlFlv.addActionListener(new BeobWeb(Konstanten.ADRESSE_WEBSITE_FLVSTREAMER));
        jButtonMplayerPfad.addActionListener(new BeobPfad(jTextFieldMplayer));
        jButtonVlcPfad.addActionListener(new BeobPfad(jTextFieldVlc));
        jButtonFlvPfad.addActionListener(new BeobPfad(jTextFieldFlv));
        jButtonMplayerSuchen.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                Daten.system[Konstanten.SYSTEM_PFAD_MPLAYER_NR] = "";
                jTextFieldMplayer.setText(GuiFunktionenProgramme.getMusterPfadMplayer());
            }
        });
        jButtonVlcSuchen.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                Daten.system[Konstanten.SYSTEM_PFAD_VLC_NR] = "";
                jTextFieldVlc.setText(GuiFunktionenProgramme.getMusterPfadVlc());
            }
        });
        jButtonFlvSuchen.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                Daten.system[Konstanten.SYSTEM_PFAD_FLVSTREAMER_NR] = "";
                jTextFieldFlv.setText(GuiFunktionenProgramme.getMusterPfadFlv());
            }
        });
        jButtonHilfe.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                new DialogHilfe(null, true, new GetFile().getHilfeSuchen(GetFile.PFAD_HILFETEXT_STANDARD_PSET)).setVisible(true);
            }
        });
    }

    private void check() {
        Daten.system[Konstanten.SYSTEM_PFAD_MPLAYER_NR] = jTextFieldMplayer.getText();
        Daten.system[Konstanten.SYSTEM_PFAD_VLC_NR] = jTextFieldVlc.getText();
        Daten.system[Konstanten.SYSTEM_PFAD_FLVSTREAMER_NR] = jTextFieldFlv.getText();
        try {
            if (jTextFieldMplayer.getText().equals("")) {
                jTextFieldMplayer.setBackground(new Color(255, 200, 200));
            } else if (!new File(Daten.system[Konstanten.SYSTEM_PFAD_MPLAYER_NR]).exists()) {
                jTextFieldMplayer.setBackground(new Color(255, 200, 200));
            } else {
                jTextFieldMplayer.setBackground(javax.swing.UIManager.getDefaults().getColor("TextField.background"));
            }
        } catch (Exception ex) {
            jTextFieldMplayer.setBackground(new Color(255, 200, 200));
        }
        try {
            if (jTextFieldVlc.getText().equals("")) {
                jTextFieldVlc.setBackground(new Color(255, 200, 200));
            } else if (!new File(Daten.system[Konstanten.SYSTEM_PFAD_VLC_NR]).exists()) {
                jTextFieldVlc.setBackground(new Color(255, 200, 200));
            } else {
                jTextFieldVlc.setBackground(javax.swing.UIManager.getDefaults().getColor("TextField.background"));
            }
        } catch (Exception ex) {
            jTextFieldVlc.setBackground(new Color(255, 200, 200));
        }
        try {
            if (jTextFieldFlv.getText().equals("")) {
                jTextFieldFlv.setBackground(new Color(255, 200, 200));
            } else if (!new File(Daten.system[Konstanten.SYSTEM_PFAD_FLVSTREAMER_NR]).exists()) {
                jTextFieldFlv.setBackground(new Color(255, 200, 200));
            } else {
                jTextFieldFlv.setBackground(javax.swing.UIManager.getDefaults().getColor("TextField.background"));
            }
        } catch (Exception ex) {
            jTextFieldFlv.setBackground(new Color(255, 200, 200));
        }
    }

    /** This method is called from within the constructor to
     * initialize the form.
     * WARNING: Do NOT modify this code. The content of this method is
     * always regenerated by the Form Editor.
     */
    // <editor-fold defaultstate="collapsed" desc="Generated Code">//GEN-BEGIN:initComponents
    private void initComponents() {

        buttonGroup1 = new javax.swing.ButtonGroup();
        buttonGroup2 = new javax.swing.ButtonGroup();
        buttonGroup3 = new javax.swing.ButtonGroup();
        jScrollPane1 = new javax.swing.JScrollPane();
        jPanel1 = new javax.swing.JPanel();
        jPanelVlc = new javax.swing.JPanel();
        jTextFieldVlc = new javax.swing.JTextField();
        jButtonVlcPfad = new javax.swing.JButton();
        jButtonVlcSuchen = new javax.swing.JButton();
        jLabel1 = new javax.swing.JLabel();
        jTextFieldUrlVlc = new javax.swing.JTextField();
        jPanelFlv = new javax.swing.JPanel();
        jTextFieldFlv = new javax.swing.JTextField();
        jButtonFlvPfad = new javax.swing.JButton();
        jButtonFlvSuchen = new javax.swing.JButton();
        jLabel2 = new javax.swing.JLabel();
        jTextFieldUrlFlv = new javax.swing.JTextField();
        jButtonHilfe = new javax.swing.JButton();
        jPanelMplayer = new javax.swing.JPanel();
        jTextFieldMplayer = new javax.swing.JTextField();
        jButtonMplayerPfad = new javax.swing.JButton();
        jButtonMplayerSuchen = new javax.swing.JButton();
        jLabel3 = new javax.swing.JLabel();
        jTextFieldUrlMplayer = new javax.swing.JTextField();

        jPanelVlc.setBorder(javax.swing.BorderFactory.createTitledBorder("Pfad zum VLC-Player auswählen"));

        jButtonVlcPfad.setIcon(new javax.swing.ImageIcon(getClass().getResource("/mediathek/res/fileopen_16.png"))); // NOI18N

        jButtonVlcSuchen.setText("suchen");

        jLabel1.setText("Website:");

        jTextFieldUrlVlc.setEditable(false);
        jTextFieldUrlVlc.setFont(new java.awt.Font("Dialog", 1, 12)); // NOI18N
        jTextFieldUrlVlc.setForeground(new java.awt.Color(51, 51, 255));
        jTextFieldUrlVlc.setText("http://www.videolan.org/");
        jTextFieldUrlVlc.setBorder(null);

        javax.swing.GroupLayout jPanelVlcLayout = new javax.swing.GroupLayout(jPanelVlc);
        jPanelVlc.setLayout(jPanelVlcLayout);
        jPanelVlcLayout.setHorizontalGroup(
            jPanelVlcLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanelVlcLayout.createSequentialGroup()
                .addContainerGap()
                .addGroup(jPanelVlcLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addGroup(jPanelVlcLayout.createSequentialGroup()
                        .addComponent(jTextFieldVlc)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                        .addComponent(jButtonVlcPfad)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                        .addComponent(jButtonVlcSuchen))
                    .addGroup(jPanelVlcLayout.createSequentialGroup()
                        .addComponent(jLabel1)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                        .addComponent(jTextFieldUrlVlc, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                        .addGap(0, 0, Short.MAX_VALUE)))
                .addContainerGap())
        );
        jPanelVlcLayout.setVerticalGroup(
            jPanelVlcLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanelVlcLayout.createSequentialGroup()
                .addContainerGap()
                .addGroup(jPanelVlcLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.CENTER)
                    .addComponent(jTextFieldVlc, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                    .addComponent(jButtonVlcPfad)
                    .addComponent(jButtonVlcSuchen))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                .addGroup(jPanelVlcLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.CENTER)
                    .addComponent(jLabel1)
                    .addComponent(jTextFieldUrlVlc, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE))
                .addContainerGap())
        );

        jPanelVlcLayout.linkSize(javax.swing.SwingConstants.VERTICAL, new java.awt.Component[] {jButtonVlcPfad, jTextFieldVlc});

        jPanelFlv.setBorder(javax.swing.BorderFactory.createTitledBorder("Pfad zum flvstreamer auswählen"));

        jButtonFlvPfad.setIcon(new javax.swing.ImageIcon(getClass().getResource("/mediathek/res/fileopen_16.png"))); // NOI18N

        jButtonFlvSuchen.setText("suchen");

        jLabel2.setText("Website:");

        jTextFieldUrlFlv.setEditable(false);
        jTextFieldUrlFlv.setFont(new java.awt.Font("Dialog", 1, 12)); // NOI18N
        jTextFieldUrlFlv.setForeground(new java.awt.Color(51, 51, 255));
        jTextFieldUrlFlv.setText("https://savannah.nongnu.org/projects/flvstreamer");
        jTextFieldUrlFlv.setBorder(null);

        javax.swing.GroupLayout jPanelFlvLayout = new javax.swing.GroupLayout(jPanelFlv);
        jPanelFlv.setLayout(jPanelFlvLayout);
        jPanelFlvLayout.setHorizontalGroup(
            jPanelFlvLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanelFlvLayout.createSequentialGroup()
                .addContainerGap()
                .addGroup(jPanelFlvLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addGroup(jPanelFlvLayout.createSequentialGroup()
                        .addComponent(jLabel2)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                        .addComponent(jTextFieldUrlFlv, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                        .addGap(0, 34, Short.MAX_VALUE))
                    .addGroup(jPanelFlvLayout.createSequentialGroup()
                        .addComponent(jTextFieldFlv)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                        .addComponent(jButtonFlvPfad)))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(jButtonFlvSuchen)
                .addContainerGap())
        );
        jPanelFlvLayout.setVerticalGroup(
            jPanelFlvLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanelFlvLayout.createSequentialGroup()
                .addGap(1, 1, 1)
                .addGroup(jPanelFlvLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.CENTER)
                    .addComponent(jTextFieldFlv, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                    .addComponent(jButtonFlvPfad)
                    .addComponent(jButtonFlvSuchen))
                .addGap(18, 18, 18)
                .addGroup(jPanelFlvLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.CENTER)
                    .addComponent(jLabel2)
                    .addComponent(jTextFieldUrlFlv, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE))
                .addContainerGap())
        );

        jPanelFlvLayout.linkSize(javax.swing.SwingConstants.VERTICAL, new java.awt.Component[] {jButtonFlvPfad, jTextFieldFlv});

        jButtonHilfe.setIcon(new javax.swing.ImageIcon(getClass().getResource("/mediathek/res/help_16.png"))); // NOI18N

        jPanelMplayer.setBorder(javax.swing.BorderFactory.createTitledBorder("Pfad zum mplayer auswählen"));

        jButtonMplayerPfad.setIcon(new javax.swing.ImageIcon(getClass().getResource("/mediathek/res/fileopen_16.png"))); // NOI18N

        jButtonMplayerSuchen.setText("suchen");

        jLabel3.setText("Website:");

        jTextFieldUrlMplayer.setEditable(false);
        jTextFieldUrlMplayer.setFont(new java.awt.Font("Dialog", 1, 12)); // NOI18N
        jTextFieldUrlMplayer.setForeground(new java.awt.Color(51, 51, 255));
        jTextFieldUrlMplayer.setText("http://sourceforge.net/projects/smplayer/");
        jTextFieldUrlMplayer.setBorder(null);

        javax.swing.GroupLayout jPanelMplayerLayout = new javax.swing.GroupLayout(jPanelMplayer);
        jPanelMplayer.setLayout(jPanelMplayerLayout);
        jPanelMplayerLayout.setHorizontalGroup(
            jPanelMplayerLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanelMplayerLayout.createSequentialGroup()
                .addContainerGap()
                .addGroup(jPanelMplayerLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addGroup(jPanelMplayerLayout.createSequentialGroup()
                        .addComponent(jLabel3)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                        .addComponent(jTextFieldUrlMplayer, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                        .addGap(0, 96, Short.MAX_VALUE))
                    .addGroup(jPanelMplayerLayout.createSequentialGroup()
                        .addComponent(jTextFieldMplayer)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                        .addComponent(jButtonMplayerPfad)))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(jButtonMplayerSuchen)
                .addContainerGap())
        );
        jPanelMplayerLayout.setVerticalGroup(
            jPanelMplayerLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanelMplayerLayout.createSequentialGroup()
                .addGap(1, 1, 1)
                .addGroup(jPanelMplayerLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.CENTER)
                    .addComponent(jTextFieldMplayer, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                    .addComponent(jButtonMplayerPfad)
                    .addComponent(jButtonMplayerSuchen))
                .addGap(18, 18, 18)
                .addGroup(jPanelMplayerLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.CENTER)
                    .addComponent(jLabel3)
                    .addComponent(jTextFieldUrlMplayer, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE))
                .addContainerGap())
        );

        jPanelMplayerLayout.linkSize(javax.swing.SwingConstants.VERTICAL, new java.awt.Component[] {jButtonMplayerPfad, jButtonMplayerSuchen, jTextFieldMplayer});

        javax.swing.GroupLayout jPanel1Layout = new javax.swing.GroupLayout(jPanel1);
        jPanel1.setLayout(jPanel1Layout);
        jPanel1Layout.setHorizontalGroup(
            jPanel1Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel1Layout.createSequentialGroup()
                .addContainerGap()
                .addGroup(jPanel1Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addComponent(jPanelFlv, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                    .addComponent(jPanelVlc, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                    .addComponent(jButtonHilfe, javax.swing.GroupLayout.Alignment.TRAILING)
                    .addComponent(jPanelMplayer, javax.swing.GroupLayout.Alignment.TRAILING, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
                .addContainerGap())
        );
        jPanel1Layout.setVerticalGroup(
            jPanel1Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel1Layout.createSequentialGroup()
                .addContainerGap()
                .addComponent(jPanelVlc, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                .addComponent(jPanelFlv, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addGap(18, 18, 18)
                .addComponent(jPanelMplayer, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED, 62, Short.MAX_VALUE)
                .addComponent(jButtonHilfe)
                .addContainerGap())
        );

        jScrollPane1.setViewportView(jPanel1);

        javax.swing.GroupLayout layout = new javax.swing.GroupLayout(this);
        this.setLayout(layout);
        layout.setHorizontalGroup(
            layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(layout.createSequentialGroup()
                .addContainerGap()
                .addComponent(jScrollPane1)
                .addContainerGap())
        );
        layout.setVerticalGroup(
            layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(layout.createSequentialGroup()
                .addContainerGap()
                .addComponent(jScrollPane1)
                .addGap(18, 18, 18))
        );
    }// </editor-fold>//GEN-END:initComponents
    // Variables declaration - do not modify//GEN-BEGIN:variables
    private javax.swing.ButtonGroup buttonGroup1;
    private javax.swing.ButtonGroup buttonGroup2;
    private javax.swing.ButtonGroup buttonGroup3;
    private javax.swing.JButton jButtonFlvPfad;
    private javax.swing.JButton jButtonFlvSuchen;
    private javax.swing.JButton jButtonHilfe;
    private javax.swing.JButton jButtonMplayerPfad;
    private javax.swing.JButton jButtonMplayerSuchen;
    private javax.swing.JButton jButtonVlcPfad;
    private javax.swing.JButton jButtonVlcSuchen;
    private javax.swing.JLabel jLabel1;
    private javax.swing.JLabel jLabel2;
    private javax.swing.JLabel jLabel3;
    private javax.swing.JPanel jPanel1;
    private javax.swing.JPanel jPanelFlv;
    private javax.swing.JPanel jPanelMplayer;
    private javax.swing.JPanel jPanelVlc;
    private javax.swing.JScrollPane jScrollPane1;
    private javax.swing.JTextField jTextFieldFlv;
    private javax.swing.JTextField jTextFieldMplayer;
    private javax.swing.JTextField jTextFieldUrlFlv;
    private javax.swing.JTextField jTextFieldUrlMplayer;
    private javax.swing.JTextField jTextFieldUrlVlc;
    private javax.swing.JTextField jTextFieldVlc;
    // End of variables declaration//GEN-END:variables

    private class BeobDoc implements DocumentListener {

        @Override
        public void insertUpdate(DocumentEvent e) {
            check();
        }

        @Override
        public void removeUpdate(DocumentEvent e) {
            check();
        }

        @Override
        public void changedUpdate(DocumentEvent e) {
            check();
        }
    }

    private class BeobPfad implements ActionListener {

        private JTextField textField;

        public BeobPfad(JTextField ttextField) {
            textField = ttextField;
        }

        @Override
        public void actionPerformed(ActionEvent e) {
            //we can use native chooser on Mac...
            if (SystemInfo.isMacOSX()) {
                FileDialog chooser = new FileDialog(ddaten.mediathekGui, "Programmdatei auswählen");
                chooser.setMode(FileDialog.LOAD);
                chooser.setVisible(true);
                if (chooser.getFile() != null) {
                    try {
                        textField.setText(new File(chooser.getFile()).getAbsolutePath());
                    } catch (Exception ex) {
                        Log.fehlerMeldung(306087945, Log.FEHLER_ART_PROG, "PanelImportStandardProgramme.BeobPfad", ex);
                    }
                }
            } else {
                int returnVal;
                JFileChooser chooser = new JFileChooser();
                chooser.setFileSelectionMode(JFileChooser.FILES_ONLY);
                chooser.setFileHidingEnabled(false);
                if (textField.getText().equals("")) {
                    chooser.setCurrentDirectory(new File(GuiFunktionen.getHomePath()));
                } else {
                    chooser.setCurrentDirectory(new File(textField.getText()));
                }
                returnVal = chooser.showOpenDialog(null);
                if (returnVal == JFileChooser.APPROVE_OPTION) {
                    try {
                        textField.setText(chooser.getSelectedFile().getAbsolutePath());
                    } catch (Exception ex) {
                        Log.fehlerMeldung(643289561, Log.FEHLER_ART_PROG, "PanelImportStandardProgramme.BeobPfad", ex);
                    }
                }
            }
        }
    }
}
