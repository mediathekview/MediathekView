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
import java.net.URISyntaxException;
import javax.swing.JDialog;
import javax.swing.JFileChooser;
import javax.swing.JFrame;
import javax.swing.JPanel;
import javax.swing.JTextField;
import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;
import mediathek.daten.Daten;
import mediathek.file.GetFile;
import mediathek.gui.dialog.DialogHilfe;
import mediathek.res.GetIcon;
import mediathek.tool.GuiFunktionen;
import mediathek.tool.GuiFunktionenProgramme;
import mediathek.tool.Konstanten;
import mediathek.controller.Log;
import mediathek.tool.UrlHyperlinkAction;

public class PanelProgrammPfade extends JPanel {

    public JDialog dialog = null;
    private boolean vlc, flvstreamer, mplayer, ffmpeg;
    private Daten ddaten;
    private JFrame parentComponent;

    public PanelProgrammPfade(JFrame parentFrame, Daten dd, boolean vvlc, boolean fflvstreamer, boolean mmplayer, boolean fffmpeg) {
        initComponents();
        ddaten = dd;
        vlc = vvlc;
        flvstreamer = fflvstreamer;
        ffmpeg = fffmpeg;
        mplayer = mmplayer;
        parentComponent = parentFrame;
        init();
        initBeob();
    }

    private void init() {
        jButtonVlcPfad.setIcon(GetIcon.getIcon("fileopen_16.png"));
        jButtonFlvPfad.setIcon(GetIcon.getIcon("fileopen_16.png"));
        jButtonHilfe.setIcon(GetIcon.getIcon("help_16.png"));
        jButtonMplayerPfad.setIcon(GetIcon.getIcon("fileopen_16.png"));
        jPanelVlc.setVisible(vlc);
        jPanelFlv.setVisible(flvstreamer);
        jPanelMplayer.setVisible(mplayer);
        jPanelFFmpeg.setVisible(ffmpeg);
        if (Daten.mVConfig.get(Konstanten.SYSTEM_PFAD_MPLAYER).equals("")) {
          Daten.mVConfig.add(Konstanten.SYSTEM_PFAD_MPLAYER, GuiFunktionenProgramme.getMusterPfadMplayer());
        }
        if (Daten.mVConfig.get(Konstanten.SYSTEM_PFAD_VLC).equals("")) {
            Daten.mVConfig.add(Konstanten.SYSTEM_PFAD_VLC, GuiFunktionenProgramme.getMusterPfadVlc());
        }
        if (Daten.mVConfig.get(Konstanten.SYSTEM_PFAD_FLVSTREAMER).equals("")) {
            Daten.mVConfig.add(Konstanten.SYSTEM_PFAD_FLVSTREAMER, GuiFunktionenProgramme.getMusterPfadFlv());
        }
        if (Daten.mVConfig.get(Konstanten.SYSTEM_PFAD_FFMPEG).equals("")) {
            Daten.mVConfig.add(Konstanten.SYSTEM_PFAD_FFMPEG, GuiFunktionenProgramme.getMusterPfadFFmpeg());
        }
        jTextFieldVlc.setText(Daten.mVConfig.get(Konstanten.SYSTEM_PFAD_VLC));
        jTextFieldFlv.setText(Daten.mVConfig.get(Konstanten.SYSTEM_PFAD_FLVSTREAMER));
        jTextFieldFFmpeg.setText(Daten.mVConfig.get(Konstanten.SYSTEM_PFAD_FFMPEG));
        jTextFieldMplayer.setText(Daten.mVConfig.get(Konstanten.SYSTEM_PFAD_MPLAYER));
    }

    private void initBeob() {
        jTextFieldVlc.getDocument().addDocumentListener(new BeobDoc());
        jTextFieldFlv.getDocument().addDocumentListener(new BeobDoc());
        jTextFieldFFmpeg.getDocument().addDocumentListener(new BeobDoc());
        jTextFieldMplayer.getDocument().addDocumentListener(new BeobDoc());
        try {
            jXHyperlinkVlc.setAction(new UrlHyperlinkAction(parentComponent, ddaten, Konstanten.ADRESSE_WEBSITE_VLC));
            jXHyperlinkflvstreamer.setAction(new UrlHyperlinkAction(parentComponent, ddaten, Konstanten.ADRESSE_WEBSITE_FLVSTREAMER));
            jXHyperlinkSmplayer.setAction(new UrlHyperlinkAction(parentComponent, ddaten, Konstanten.ADRESSE_WEBSITE_MPLAYER));
            jXHyperlinkFFmpeg.setAction(new UrlHyperlinkAction(parentComponent, ddaten, Konstanten.ADRESSE_WEBSITE_FFMPEG));
        } catch (URISyntaxException ignored) {
        }
        jButtonMplayerPfad.addActionListener(new BeobPfad(jTextFieldMplayer));
        jButtonVlcPfad.addActionListener(new BeobPfad(jTextFieldVlc));
        jButtonFlvPfad.addActionListener(new BeobPfad(jTextFieldFlv));
        jButtonFFmpegPfad.addActionListener(new BeobPfad(jTextFieldFFmpeg));
        jButtonMplayerSuchen.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
              Daten.mVConfig.add(Konstanten.SYSTEM_PFAD_MPLAYER, "");
                jTextFieldMplayer.setText(GuiFunktionenProgramme.getMusterPfadMplayer());
            }
        });
        jButtonVlcSuchen.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                Daten.mVConfig.add(Konstanten.SYSTEM_PFAD_VLC, "");
                jTextFieldVlc.setText(GuiFunktionenProgramme.getMusterPfadVlc());
            }
        });
        jButtonFlvSuchen.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                Daten.mVConfig.add(Konstanten.SYSTEM_PFAD_FLVSTREAMER, "");
                jTextFieldFlv.setText(GuiFunktionenProgramme.getMusterPfadFlv());
            }
        });
        jButtonFFmpegSuchen.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                Daten.mVConfig.add(Konstanten.SYSTEM_PFAD_FFMPEG,"");
                jTextFieldFFmpeg.setText(GuiFunktionenProgramme.getMusterPfadFFmpeg());
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
        Daten.mVConfig.add(Konstanten.SYSTEM_PFAD_MPLAYER, jTextFieldMplayer.getText());
        Daten.mVConfig.add(Konstanten.SYSTEM_PFAD_VLC, jTextFieldVlc.getText());
        Daten.mVConfig.add(Konstanten.SYSTEM_PFAD_FLVSTREAMER, jTextFieldFlv.getText());
        Daten.mVConfig.add(Konstanten.SYSTEM_PFAD_FFMPEG,jTextFieldFFmpeg.getText());
        try {
            if (jTextFieldMplayer.getText().equals("")) {
                jTextFieldMplayer.setBackground(new Color(255, 200, 200));
            } else if (!new File(Daten.mVConfig.get(Konstanten.SYSTEM_PFAD_MPLAYER)).exists()) {
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
            } else if (!new File(Daten.mVConfig.get(Konstanten.SYSTEM_PFAD_VLC)).exists()) {
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
            } else if (!new File(Daten.mVConfig.get(Konstanten.SYSTEM_PFAD_FLVSTREAMER)).exists()) {
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
            } else if (!new File(Daten.mVConfig.get(Konstanten.SYSTEM_PFAD_FFMPEG)).exists()) {
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
        jPanelMplayer = new javax.swing.JPanel();
        jTextFieldMplayer = new javax.swing.JTextField();
        jButtonMplayerPfad = new javax.swing.JButton();
        jButtonMplayerSuchen = new javax.swing.JButton();
        javax.swing.JLabel jLabel3 = new javax.swing.JLabel();
        jXHyperlinkSmplayer = new org.jdesktop.swingx.JXHyperlink();
        jPanelFFmpeg = new javax.swing.JPanel();
        jTextFieldFFmpeg = new javax.swing.JTextField();
        jButtonFFmpegSuchen = new javax.swing.JButton();
        jButtonFFmpegPfad = new javax.swing.JButton();
        jLabel4 = new javax.swing.JLabel();
        jXHyperlinkFFmpeg = new org.jdesktop.swingx.JXHyperlink();

        jPanelVlc.setBorder(javax.swing.BorderFactory.createTitledBorder("Pfad zum VLC-Player auswählen"));

        jButtonVlcPfad.setIcon(new javax.swing.ImageIcon(getClass().getResource("/mediathek/res/fileopen_16.png"))); // NOI18N

        jButtonVlcSuchen.setText("suchen");

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

        jButtonFlvPfad.setIcon(new javax.swing.ImageIcon(getClass().getResource("/mediathek/res/fileopen_16.png"))); // NOI18N

        jButtonFlvSuchen.setText("suchen");

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

        jButtonHilfe.setIcon(new javax.swing.ImageIcon(getClass().getResource("/mediathek/res/help_16.png"))); // NOI18N

        jPanelMplayer.setBorder(javax.swing.BorderFactory.createTitledBorder("Pfad zum mplayer auswählen"));

        jButtonMplayerPfad.setIcon(new javax.swing.ImageIcon(getClass().getResource("/mediathek/res/fileopen_16.png"))); // NOI18N

        jButtonMplayerSuchen.setText("suchen");

        jLabel3.setText("Website:");

        jXHyperlinkSmplayer.setText("http://sourceforge.net/projects/smplayer/");

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
                        .addComponent(jXHyperlinkSmplayer, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                        .addGap(0, 0, Short.MAX_VALUE))
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
                .addGroup(jPanelMplayerLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(jLabel3)
                    .addComponent(jXHyperlinkSmplayer, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE))
                .addContainerGap())
        );

        jPanelMplayerLayout.linkSize(javax.swing.SwingConstants.VERTICAL, new java.awt.Component[] {jButtonMplayerPfad, jButtonMplayerSuchen, jTextFieldMplayer});

        jPanelFFmpeg.setBorder(javax.swing.BorderFactory.createTitledBorder("Pfad zu ffmpeg auswählen"));

        jButtonFFmpegSuchen.setText("suchen");

        jButtonFFmpegPfad.setIcon(new javax.swing.ImageIcon(getClass().getResource("/mediathek/res/fileopen_16.png"))); // NOI18N

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
                    .addComponent(jPanelMplayer, javax.swing.GroupLayout.Alignment.TRAILING, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
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
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(jPanelMplayer, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
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
                .addComponent(jScrollPane1, javax.swing.GroupLayout.DEFAULT_SIZE, 459, Short.MAX_VALUE)
                .addContainerGap())
        );
    }// </editor-fold>//GEN-END:initComponents
    // Variables declaration - do not modify//GEN-BEGIN:variables
    private javax.swing.JButton jButtonFFmpegPfad;
    private javax.swing.JButton jButtonFFmpegSuchen;
    private javax.swing.JButton jButtonFlvPfad;
    private javax.swing.JButton jButtonFlvSuchen;
    private javax.swing.JButton jButtonHilfe;
    private javax.swing.JButton jButtonMplayerPfad;
    private javax.swing.JButton jButtonMplayerSuchen;
    private javax.swing.JButton jButtonVlcPfad;
    private javax.swing.JButton jButtonVlcSuchen;
    private javax.swing.JLabel jLabel4;
    private javax.swing.JPanel jPanelFFmpeg;
    private javax.swing.JPanel jPanelFlv;
    private javax.swing.JPanel jPanelMplayer;
    private javax.swing.JPanel jPanelVlc;
    private javax.swing.JTextField jTextFieldFFmpeg;
    private javax.swing.JTextField jTextFieldFlv;
    private javax.swing.JTextField jTextFieldMplayer;
    private javax.swing.JTextField jTextFieldVlc;
    private org.jdesktop.swingx.JXHyperlink jXHyperlinkFFmpeg;
    private org.jdesktop.swingx.JXHyperlink jXHyperlinkSmplayer;
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
                        textField.setText(new File(chooser.getDirectory() + chooser.getFile()).getAbsolutePath());
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
