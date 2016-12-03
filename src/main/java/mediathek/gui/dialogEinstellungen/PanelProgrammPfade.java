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
import mSearch.tool.Log;
import mediathek.config.Icons;
import mediathek.config.Konstanten;
import mediathek.config.MVConfig;
import mediathek.file.GetFile;
import mediathek.gui.actions.UrlHyperlinkAction;
import mediathek.gui.dialog.DialogHilfe;
import mediathek.tool.GuiFunktionen;
import mediathek.tool.GuiFunktionenProgramme;

import javax.swing.*;
import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;
import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.File;
import java.net.URISyntaxException;

@SuppressWarnings("serial")
public class PanelProgrammPfade extends JPanel {
    public JDialog dialog = null;
    private final boolean vlc, flvstreamer, ffmpeg;
    private final JFrame parentComponent;

    public PanelProgrammPfade(JFrame parentFrame, boolean vvlc, boolean fflvstreamer, boolean fffmpeg) {
        initComponents();
        vlc = vvlc;
        flvstreamer = fflvstreamer;
        ffmpeg = fffmpeg;
        parentComponent = parentFrame;
        init();
        initBeob();
    }

    private void init() {
        jButtonVlcPfad.setIcon(Icons.ICON_BUTTON_FILE_OPEN);
        jButtonFlvPfad.setIcon(Icons.ICON_BUTTON_FILE_OPEN);
        jButtonFFmpegPfad.setIcon(Icons.ICON_BUTTON_FILE_OPEN);
        jButtonHilfe.setIcon(Icons.ICON_BUTTON_HELP);
        jPanelVlc.setVisible(vlc);
        jPanelFlv.setVisible(flvstreamer);
        jPanelFFmpeg.setVisible(ffmpeg);
        if (MVConfig.get(MVConfig.Configs.SYSTEM_PFAD_VLC).equals("")) {
            MVConfig.add(MVConfig.Configs.SYSTEM_PFAD_VLC, GuiFunktionenProgramme.getMusterPfadVlc());
        }
        if (MVConfig.get(MVConfig.Configs.SYSTEM_PFAD_FLVSTREAMER).equals("")) {
            MVConfig.add(MVConfig.Configs.SYSTEM_PFAD_FLVSTREAMER, GuiFunktionenProgramme.getMusterPfadFlv());
        }
        if (MVConfig.get(MVConfig.Configs.SYSTEM_PFAD_FFMPEG).equals("")) {
            MVConfig.add(MVConfig.Configs.SYSTEM_PFAD_FFMPEG, GuiFunktionenProgramme.getMusterPfadFFmpeg());
        }
        jTextFieldVlc.setText(MVConfig.get(MVConfig.Configs.SYSTEM_PFAD_VLC));
        jTextFieldFlv.setText(MVConfig.get(MVConfig.Configs.SYSTEM_PFAD_FLVSTREAMER));
        jTextFieldFFmpeg.setText(MVConfig.get(MVConfig.Configs.SYSTEM_PFAD_FFMPEG));
    }

    private void initBeob() {
        jTextFieldVlc.getDocument().addDocumentListener(new BeobDoc());
        jTextFieldFlv.getDocument().addDocumentListener(new BeobDoc());
        jTextFieldFFmpeg.getDocument().addDocumentListener(new BeobDoc());
        try {
            jXHyperlinkVlc.setText(Konstanten.ADRESSE_WEBSITE_VLC);
            jXHyperlinkVlc.setAction(new UrlHyperlinkAction(parentComponent, Konstanten.ADRESSE_WEBSITE_VLC));
            jXHyperlinkflvstreamer.setText(Konstanten.ADRESSE_WEBSITE_FLVSTREAMER);
            jXHyperlinkflvstreamer.setAction(new UrlHyperlinkAction(parentComponent, Konstanten.ADRESSE_WEBSITE_FLVSTREAMER));
            jXHyperlinkFFmpeg.setText(Konstanten.ADRESSE_WEBSITE_FFMPEG);
            jXHyperlinkFFmpeg.setAction(new UrlHyperlinkAction(parentComponent, Konstanten.ADRESSE_WEBSITE_FFMPEG));
        } catch (URISyntaxException ignored) {
        }
        jButtonVlcPfad.addActionListener(new BeobPfad(jTextFieldVlc));
        jButtonFlvPfad.addActionListener(new BeobPfad(jTextFieldFlv));
        jButtonFFmpegPfad.addActionListener(new BeobPfad(jTextFieldFFmpeg));
        jButtonVlcSuchen.addActionListener(e -> {
            MVConfig.add(MVConfig.Configs.SYSTEM_PFAD_VLC, "");
            jTextFieldVlc.setText(GuiFunktionenProgramme.getMusterPfadVlc());
        });
        jButtonFlvSuchen.addActionListener(e -> {
            MVConfig.add(MVConfig.Configs.SYSTEM_PFAD_FLVSTREAMER, "");
            jTextFieldFlv.setText(GuiFunktionenProgramme.getMusterPfadFlv());
        });
        jButtonFFmpegSuchen.addActionListener(e -> {
            MVConfig.add(MVConfig.Configs.SYSTEM_PFAD_FFMPEG, "");
            jTextFieldFFmpeg.setText(GuiFunktionenProgramme.getMusterPfadFFmpeg());
        });
        jButtonHilfe.addActionListener(e -> new DialogHilfe(parentComponent, true, new GetFile().getHilfeSuchen(GetFile.PFAD_HILFETEXT_STANDARD_PSET)).setVisible(true));
    }

    private void check() {
        MVConfig.add(MVConfig.Configs.SYSTEM_PFAD_VLC, jTextFieldVlc.getText());
        MVConfig.add(MVConfig.Configs.SYSTEM_PFAD_FLVSTREAMER, jTextFieldFlv.getText());
        MVConfig.add(MVConfig.Configs.SYSTEM_PFAD_FFMPEG, jTextFieldFFmpeg.getText());
        try {
            if (jTextFieldVlc.getText().equals("")) {
                jTextFieldVlc.setBackground(new Color(255, 200, 200));
            } else if (!new File(MVConfig.get(MVConfig.Configs.SYSTEM_PFAD_VLC)).exists()) {
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
            } else if (!new File(MVConfig.get(MVConfig.Configs.SYSTEM_PFAD_FLVSTREAMER)).exists()) {
                jTextFieldFlv.setBackground(new Color(255, 200, 200));
            } else {
                jTextFieldFlv.setBackground(javax.swing.UIManager.getDefaults().getColor("TextField.background"));
            }
        } catch (Exception ex) {
            jTextFieldFlv.setBackground(new Color(255, 200, 200));
        }
        try {
            if (jTextFieldFFmpeg.getText().equals("")) {
                jTextFieldFFmpeg.setBackground(new Color(255, 200, 200));
            } else if (!new File(MVConfig.get(MVConfig.Configs.SYSTEM_PFAD_FFMPEG)).exists()) {
                jTextFieldFFmpeg.setBackground(new Color(255, 200, 200));
            } else {
                jTextFieldFFmpeg.setBackground(javax.swing.UIManager.getDefaults().getColor("TextField.background"));
            }
        } catch (Exception ex) {
            jTextFieldFFmpeg.setBackground(new Color(255, 200, 200));
        }
    }

    /** This method is called from within the constructor to
     * initialize the form.
     * WARNING: Do NOT modify this code. The content of this method is
     * always regenerated by the Form Editor.
     */
    // <editor-fold defaultstate="collapsed" desc="Generated Code">//GEN-BEGIN:initComponents
    private void initComponents() {

        javax.swing.ButtonGroup buttonGroup1 = new javax.swing.ButtonGroup();
        javax.swing.ButtonGroup buttonGroup2 = new javax.swing.ButtonGroup();
        javax.swing.ButtonGroup buttonGroup3 = new javax.swing.ButtonGroup();
        javax.swing.JScrollPane jScrollPane1 = new javax.swing.JScrollPane();
        javax.swing.JPanel jPanel1 = new javax.swing.JPanel();
        jPanelVlc = new javax.swing.JPanel();
        jTextFieldVlc = new javax.swing.JTextField();
        jButtonVlcPfad = new javax.swing.JButton();
        jButtonVlcSuchen = new javax.swing.JButton();
        javax.swing.JLabel jLabel1 = new javax.swing.JLabel();
        jXHyperlinkVlc = new org.jdesktop.swingx.JXHyperlink();
        jPanelFlv = new javax.swing.JPanel();
        jTextFieldFlv = new javax.swing.JTextField();
        jButtonFlvPfad = new javax.swing.JButton();
        jButtonFlvSuchen = new javax.swing.JButton();
        javax.swing.JLabel jLabel2 = new javax.swing.JLabel();
        jXHyperlinkflvstreamer = new org.jdesktop.swingx.JXHyperlink();
        jButtonHilfe = new javax.swing.JButton();
        jPanelFFmpeg = new javax.swing.JPanel();
        jTextFieldFFmpeg = new javax.swing.JTextField();
        jButtonFFmpegSuchen = new javax.swing.JButton();
        jButtonFFmpegPfad = new javax.swing.JButton();
        javax.swing.JLabel jLabel4 = new javax.swing.JLabel();
        jXHyperlinkFFmpeg = new org.jdesktop.swingx.JXHyperlink();

        jPanelVlc.setBorder(javax.swing.BorderFactory.createTitledBorder("Pfad zum VLC-Player auswählen"));

        jButtonVlcPfad.setIcon(new javax.swing.ImageIcon(getClass().getResource("/mediathek/res/muster/button-file-open.png"))); // NOI18N
        jButtonVlcPfad.setToolTipText("Programm auswählen");

        jButtonVlcSuchen.setText("suchen");
        jButtonVlcSuchen.setToolTipText("nach dem Programm suchen");

        jLabel1.setText("Website:");

        jXHyperlinkVlc.setText("http://www.videolan.org/");

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
                                                .addComponent(jXHyperlinkVlc, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
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
                                .addGroup(jPanelVlcLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                                        .addComponent(jLabel1)
                                        .addComponent(jXHyperlinkVlc, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE))
                                .addContainerGap())
        );

        jPanelVlcLayout.linkSize(javax.swing.SwingConstants.VERTICAL, new java.awt.Component[] {jButtonVlcPfad, jTextFieldVlc});

        jPanelFlv.setBorder(javax.swing.BorderFactory.createTitledBorder("Pfad zum flvstreamer auswählen"));

        jButtonFlvPfad.setIcon(new javax.swing.ImageIcon(getClass().getResource("/mediathek/res/muster/button-file-open.png"))); // NOI18N
        jButtonFlvPfad.setToolTipText("Programm auswählen");

        jButtonFlvSuchen.setText("suchen");
        jButtonFlvSuchen.setToolTipText("nach dem Programm suchen");

        jLabel2.setText("Website:");

        jXHyperlinkflvstreamer.setText("https://savannah.nongnu.org/projects/flvstreamer");

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
                                                .addComponent(jXHyperlinkflvstreamer, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                                                .addGap(0, 0, Short.MAX_VALUE))
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
                                .addGroup(jPanelFlvLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                                        .addComponent(jLabel2)
                                        .addComponent(jXHyperlinkflvstreamer, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE))
                                .addContainerGap())
        );

        jPanelFlvLayout.linkSize(javax.swing.SwingConstants.VERTICAL, new java.awt.Component[] {jButtonFlvPfad, jTextFieldFlv});

        jButtonHilfe.setIcon(new javax.swing.ImageIcon(getClass().getResource("/mediathek/res/muster/button-help.png"))); // NOI18N
        jButtonHilfe.setToolTipText("Hilfe anzeigen");

        jPanelFFmpeg.setBorder(javax.swing.BorderFactory.createTitledBorder("Pfad zu ffmpeg auswählen"));

        jButtonFFmpegSuchen.setText("suchen");
        jButtonFFmpegSuchen.setToolTipText("nach dem Programm suchen");

        jButtonFFmpegPfad.setIcon(new javax.swing.ImageIcon(getClass().getResource("/mediathek/res/muster/button-file-open.png"))); // NOI18N
        jButtonFFmpegPfad.setToolTipText("Programm auswählen");

        jLabel4.setText("Website:");

        jXHyperlinkFFmpeg.setText("http://ffmpeg.org");

        javax.swing.GroupLayout jPanelFFmpegLayout = new javax.swing.GroupLayout(jPanelFFmpeg);
        jPanelFFmpeg.setLayout(jPanelFFmpegLayout);
        jPanelFFmpegLayout.setHorizontalGroup(
                jPanelFFmpegLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                        .addGroup(jPanelFFmpegLayout.createSequentialGroup()
                                .addContainerGap()
                                .addGroup(jPanelFFmpegLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                                        .addGroup(jPanelFFmpegLayout.createSequentialGroup()
                                                .addComponent(jTextFieldFFmpeg)
                                                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                                                .addComponent(jButtonFFmpegPfad)
                                                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                                                .addComponent(jButtonFFmpegSuchen))
                                        .addGroup(jPanelFFmpegLayout.createSequentialGroup()
                                                .addComponent(jLabel4)
                                                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                                                .addComponent(jXHyperlinkFFmpeg, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                                                .addGap(0, 0, Short.MAX_VALUE)))
                                .addContainerGap())
        );
        jPanelFFmpegLayout.setVerticalGroup(
                jPanelFFmpegLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                        .addGroup(jPanelFFmpegLayout.createSequentialGroup()
                                .addContainerGap()
                                .addGroup(jPanelFFmpegLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.CENTER)
                                        .addComponent(jTextFieldFFmpeg, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                                        .addComponent(jButtonFFmpegPfad)
                                        .addComponent(jButtonFFmpegSuchen))
                                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                                .addGroup(jPanelFFmpegLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                                        .addComponent(jLabel4)
                                        .addComponent(jXHyperlinkFFmpeg, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE))
                                .addContainerGap(javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
        );

        jPanelFFmpegLayout.linkSize(javax.swing.SwingConstants.VERTICAL, new java.awt.Component[] {jButtonFFmpegPfad, jButtonFFmpegSuchen, jTextFieldFFmpeg});

        javax.swing.GroupLayout jPanel1Layout = new javax.swing.GroupLayout(jPanel1);
        jPanel1.setLayout(jPanel1Layout);
        jPanel1Layout.setHorizontalGroup(
                jPanel1Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                        .addGroup(jPanel1Layout.createSequentialGroup()
                                .addContainerGap()
                                .addGroup(jPanel1Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                                        .addComponent(jPanelFlv, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                                        .addComponent(jPanelVlc, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                                        .addGroup(javax.swing.GroupLayout.Alignment.TRAILING, jPanel1Layout.createSequentialGroup()
                                                .addGap(0, 0, Short.MAX_VALUE)
                                                .addComponent(jButtonHilfe))
                                        .addComponent(jPanelFFmpeg, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
                                .addContainerGap())
        );
        jPanel1Layout.setVerticalGroup(
                jPanel1Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                        .addGroup(jPanel1Layout.createSequentialGroup()
                                .addContainerGap()
                                .addComponent(jPanelVlc, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                                .addComponent(jPanelFlv, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                                .addComponent(jPanelFFmpeg, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                                .addGap(18, 18, Short.MAX_VALUE)
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
                                .addContainerGap())
        );
    }// </editor-fold>//GEN-END:initComponents

    // Variables declaration - do not modify//GEN-BEGIN:variables
    private javax.swing.JButton jButtonFFmpegPfad;
    private javax.swing.JButton jButtonFFmpegSuchen;
    private javax.swing.JButton jButtonFlvPfad;
    private javax.swing.JButton jButtonFlvSuchen;
    private javax.swing.JButton jButtonHilfe;
    private javax.swing.JButton jButtonVlcPfad;
    private javax.swing.JButton jButtonVlcSuchen;
    private javax.swing.JPanel jPanelFFmpeg;
    private javax.swing.JPanel jPanelFlv;
    private javax.swing.JPanel jPanelVlc;
    private javax.swing.JTextField jTextFieldFFmpeg;
    private javax.swing.JTextField jTextFieldFlv;
    private javax.swing.JTextField jTextFieldVlc;
    private org.jdesktop.swingx.JXHyperlink jXHyperlinkFFmpeg;
    private org.jdesktop.swingx.JXHyperlink jXHyperlinkVlc;
    private org.jdesktop.swingx.JXHyperlink jXHyperlinkflvstreamer;
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

        private final JTextField textField;

        public BeobPfad(JTextField ttextField) {
            textField = ttextField;
        }

        @Override
        public void actionPerformed(ActionEvent e) {
            //we can use native chooser on Mac...
            if (SystemInfo.isMacOSX()) {
                FileDialog chooser = new FileDialog(parentComponent, "Programmdatei auswählen");
                chooser.setMode(FileDialog.LOAD);
                chooser.setVisible(true);
                if (chooser.getFile() != null) {
                    try {
                        textField.setText(new File(chooser.getDirectory() + chooser.getFile()).getAbsolutePath());
                    } catch (Exception ex) {
                        Log.errorLog(306087945, ex);
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
                        Log.errorLog(643289561, ex);
                    }
                }
            }
        }
    }
}
