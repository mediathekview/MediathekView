package mediathek.gui.dialogEinstellungen;

import mediathek.config.Konstanten;
import mediathek.config.application.ApplicationConfiguration;
import mediathek.gui.dialog.HelpTextDialog;
import mediathek.tool.GetFile;
import mediathek.tool.GuiFunktionenProgramme;
import mediathek.tool.SVGIconUtilities;
import org.apache.commons.lang3.SystemUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import javax.swing.*;
import javax.swing.border.TitledBorder;
import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;
import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.File;

public class PanelProgrammPfade extends JPanel {
    private static final Logger logger = LogManager.getLogger();
    private static final Color COLOR_PINK = new Color(255, 200, 200);
    private final boolean vlc, ffmpeg;
    private final JFrame parentComponent;

    public PanelProgrammPfade(JFrame parentFrame, boolean vvlc, boolean fffmpeg) {
        initComponents();
        vlc = vvlc;
        ffmpeg = fffmpeg;
        parentComponent = parentFrame;
        init();
        initBeob();
    }

    private void init() {
        jButtonVlcPfad.setIcon(SVGIconUtilities.createSVGIcon("icons/fontawesome/folder-open.svg"));
        jButtonFFmpegPfad.setIcon(SVGIconUtilities.createSVGIcon("icons/fontawesome/folder-open.svg"));
        jButtonHilfe.setIcon(SVGIconUtilities.createSVGIcon("icons/fontawesome/circle-question.svg"));
        jPanelVlc.setVisible(vlc);

        jPanelFFmpeg.setVisible(ffmpeg);
        var applicationConfiguration = ApplicationConfiguration.getInstance();
        if (applicationConfiguration.getStandardVlcPath().isEmpty()) {
            applicationConfiguration.setStandardVlcPath(GuiFunktionenProgramme.getMusterPfadVlc());
        }
        if (applicationConfiguration.getStandardFFmpegPath().isEmpty()) {
            applicationConfiguration.setStandardFFmpegPath(GuiFunktionenProgramme.getMusterPfadFFmpeg());
        }
        jTextFieldVlc.setText(applicationConfiguration.getStandardVlcPath());
        jTextFieldFFmpeg.setText(applicationConfiguration.getStandardFFmpegPath());
    }

    private void initBeob() {
        jTextFieldVlc.getDocument().addDocumentListener(new BeobDoc());
        jTextFieldFFmpeg.getDocument().addDocumentListener(new BeobDoc());

        jButtonVlcPfad.addActionListener(new BeobPfad(jTextFieldVlc));
        jButtonFFmpegPfad.addActionListener(new BeobPfad(jTextFieldFFmpeg));
        jButtonVlcSuchen.addActionListener(_ -> {
            ApplicationConfiguration.getInstance().setStandardVlcPath("");
            jTextFieldVlc.setText(GuiFunktionenProgramme.getMusterPfadVlc());
        });

        jButtonFFmpegSuchen.addActionListener(_ -> {
            ApplicationConfiguration.getInstance().setStandardFFmpegPath("");
            jTextFieldFFmpeg.setText(GuiFunktionenProgramme.getMusterPfadFFmpeg());
        });
        jButtonHilfe.addActionListener(_ -> HelpTextDialog.show(parentComponent, GetFile.getHilfeSuchen(Konstanten.PFAD_HILFETEXT_STANDARD_PSET)));
    }

    private void check() {
        var applicationConfiguration = ApplicationConfiguration.getInstance();
        applicationConfiguration.setStandardVlcPath(jTextFieldVlc.getText());
        applicationConfiguration.setStandardFFmpegPath(jTextFieldFFmpeg.getText());

        try {
            if (jTextFieldVlc.getText().isEmpty()) {
                jTextFieldVlc.setBackground(COLOR_PINK);
            }
            else if (!new File(applicationConfiguration.getStandardVlcPath()).exists()) {
                jTextFieldVlc.setBackground(COLOR_PINK);
            }
            else {
                jTextFieldVlc.setBackground(javax.swing.UIManager.getDefaults().getColor("TextField.background"));
            }
        }
        catch (Exception ex) {
            jTextFieldVlc.setBackground(COLOR_PINK);
        }

        try {
            if (jTextFieldFFmpeg.getText().isEmpty()) {
                jTextFieldFFmpeg.setBackground(COLOR_PINK);
            }
            else if (!new File(applicationConfiguration.getStandardFFmpegPath()).exists()) {
                jTextFieldFFmpeg.setBackground(COLOR_PINK);
            }
            else {
                jTextFieldFFmpeg.setBackground(javax.swing.UIManager.getDefaults().getColor("TextField.background"));
            }
        }
        catch (Exception ex) {
            jTextFieldFFmpeg.setBackground(COLOR_PINK);
        }
    }

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
            if (SystemUtils.IS_OS_MAC_OSX) {
                FileDialog chooser = new FileDialog(parentComponent, "Programmdatei auswählen");
                chooser.setMode(FileDialog.LOAD);
                chooser.setVisible(true);
                if (chooser.getFile() != null) {
                    try {
                        textField.setText(new File(chooser.getDirectory() + chooser.getFile()).getAbsolutePath());
                    }
                    catch (Exception ex) {
                        logger.error(ex);
                    }
                }
            }
            else {
                int returnVal;
                JFileChooser chooser = new JFileChooser();
                chooser.setFileSelectionMode(JFileChooser.FILES_ONLY);
                chooser.setFileHidingEnabled(false);
                if (textField.getText().isEmpty()) {
                    chooser.setCurrentDirectory(new File(SystemUtils.USER_HOME));
                }
                else {
                    chooser.setCurrentDirectory(new File(textField.getText()));
                }
                returnVal = chooser.showOpenDialog(null);
                if (returnVal == JFileChooser.APPROVE_OPTION) {
                    try {
                        textField.setText(chooser.getSelectedFile().getAbsolutePath());
                    }
                    catch (Exception ex) {
                        logger.error(ex);
                    }
                }
            }
        }
    }

    /** This method is called from within the constructor to
     * initialize the form.
     * WARNING: Do NOT modify this code. The content of this method is
     * always regenerated by the Form Editor.
     */
    // <editor-fold defaultstate="collapsed" desc="Generated Code">//GEN-BEGIN:initComponents
    // Generated using JFormDesigner non-commercial license
    private void initComponents() {
        var jScrollPane1 = new JScrollPane();
        var jPanel1 = new JPanel();
        jPanelVlc = new JPanel();
        jTextFieldVlc = new JTextField();
        jButtonVlcPfad = new JButton();
        jButtonVlcSuchen = new JButton();
        jButtonHilfe = new JButton();
        jPanelFFmpeg = new JPanel();
        jTextFieldFFmpeg = new JTextField();
        jButtonFFmpegSuchen = new JButton();
        jButtonFFmpegPfad = new JButton();

        //======== this ========

        //======== jScrollPane1 ========
        {

            //======== jPanel1 ========
            {

                //======== jPanelVlc ========
                {
                    jPanelVlc.setBorder(new TitledBorder("Pfad zum VLC-Player ausw\u00e4hlen"));

                    //---- jButtonVlcPfad ----
                    jButtonVlcPfad.setIcon(new ImageIcon(getClass().getResource("/mediathek/res/muster/button-file-open.png")));
                    jButtonVlcPfad.setToolTipText("Programm ausw\u00e4hlen");

                    //---- jButtonVlcSuchen ----
                    jButtonVlcSuchen.setText("suchen");
                    jButtonVlcSuchen.setToolTipText("nach dem Programm suchen");

                    GroupLayout jPanelVlcLayout = new GroupLayout(jPanelVlc);
                    jPanelVlc.setLayout(jPanelVlcLayout);
                    jPanelVlcLayout.setHorizontalGroup(
                        jPanelVlcLayout.createParallelGroup()
                            .addGroup(jPanelVlcLayout.createSequentialGroup()
                                .addContainerGap()
                                .addComponent(jTextFieldVlc)
                                .addPreferredGap(LayoutStyle.ComponentPlacement.RELATED)
                                .addComponent(jButtonVlcPfad)
                                .addPreferredGap(LayoutStyle.ComponentPlacement.RELATED)
                                .addComponent(jButtonVlcSuchen)
                                .addContainerGap())
                    );
                    jPanelVlcLayout.setVerticalGroup(
                        jPanelVlcLayout.createParallelGroup()
                            .addGroup(jPanelVlcLayout.createSequentialGroup()
                                .addContainerGap()
                                .addGroup(jPanelVlcLayout.createParallelGroup(GroupLayout.Alignment.CENTER)
                                    .addComponent(jTextFieldVlc, GroupLayout.PREFERRED_SIZE, GroupLayout.DEFAULT_SIZE, GroupLayout.PREFERRED_SIZE)
                                    .addComponent(jButtonVlcPfad)
                                    .addComponent(jButtonVlcSuchen))
                                .addContainerGap())
                    );
                    jPanelVlcLayout.linkSize(SwingConstants.VERTICAL, new Component[] {jButtonVlcPfad, jTextFieldVlc});
                }

                //---- jButtonHilfe ----
                jButtonHilfe.setIcon(new ImageIcon(getClass().getResource("/mediathek/res/muster/button-help.png")));
                jButtonHilfe.setToolTipText("Hilfe anzeigen");

                //======== jPanelFFmpeg ========
                {
                    jPanelFFmpeg.setBorder(new TitledBorder("Pfad zu ffmpeg ausw\u00e4hlen"));

                    //---- jButtonFFmpegSuchen ----
                    jButtonFFmpegSuchen.setText("suchen");
                    jButtonFFmpegSuchen.setToolTipText("nach dem Programm suchen");

                    //---- jButtonFFmpegPfad ----
                    jButtonFFmpegPfad.setIcon(new ImageIcon(getClass().getResource("/mediathek/res/muster/button-file-open.png")));
                    jButtonFFmpegPfad.setToolTipText("Programm ausw\u00e4hlen");

                    GroupLayout jPanelFFmpegLayout = new GroupLayout(jPanelFFmpeg);
                    jPanelFFmpeg.setLayout(jPanelFFmpegLayout);
                    jPanelFFmpegLayout.setHorizontalGroup(
                        jPanelFFmpegLayout.createParallelGroup()
                            .addGroup(jPanelFFmpegLayout.createSequentialGroup()
                                .addContainerGap()
                                .addComponent(jTextFieldFFmpeg, GroupLayout.DEFAULT_SIZE, 345, Short.MAX_VALUE)
                                .addPreferredGap(LayoutStyle.ComponentPlacement.RELATED)
                                .addComponent(jButtonFFmpegPfad)
                                .addPreferredGap(LayoutStyle.ComponentPlacement.RELATED)
                                .addComponent(jButtonFFmpegSuchen)
                                .addContainerGap())
                    );
                    jPanelFFmpegLayout.setVerticalGroup(
                        jPanelFFmpegLayout.createParallelGroup()
                            .addGroup(jPanelFFmpegLayout.createSequentialGroup()
                                .addContainerGap()
                                .addGroup(jPanelFFmpegLayout.createParallelGroup(GroupLayout.Alignment.CENTER)
                                    .addComponent(jTextFieldFFmpeg, GroupLayout.PREFERRED_SIZE, GroupLayout.DEFAULT_SIZE, GroupLayout.PREFERRED_SIZE)
                                    .addComponent(jButtonFFmpegPfad)
                                    .addComponent(jButtonFFmpegSuchen))
                                .addContainerGap(GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
                    );
                    jPanelFFmpegLayout.linkSize(SwingConstants.VERTICAL, new Component[] {jButtonFFmpegPfad, jButtonFFmpegSuchen, jTextFieldFFmpeg});
                }

                GroupLayout jPanel1Layout = new GroupLayout(jPanel1);
                jPanel1.setLayout(jPanel1Layout);
                jPanel1Layout.setHorizontalGroup(
                    jPanel1Layout.createParallelGroup()
                        .addGroup(jPanel1Layout.createSequentialGroup()
                            .addContainerGap()
                            .addGroup(jPanel1Layout.createParallelGroup()
                                .addComponent(jPanelVlc, GroupLayout.DEFAULT_SIZE, GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                                .addGroup(GroupLayout.Alignment.TRAILING, jPanel1Layout.createSequentialGroup()
                                    .addGap(0, 0, Short.MAX_VALUE)
                                    .addComponent(jButtonHilfe))
                                .addComponent(jPanelFFmpeg, GroupLayout.DEFAULT_SIZE, GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
                            .addContainerGap())
                );
                jPanel1Layout.setVerticalGroup(
                    jPanel1Layout.createParallelGroup()
                        .addGroup(jPanel1Layout.createSequentialGroup()
                            .addContainerGap()
                            .addComponent(jPanelVlc, GroupLayout.PREFERRED_SIZE, GroupLayout.DEFAULT_SIZE, GroupLayout.PREFERRED_SIZE)
                            .addPreferredGap(LayoutStyle.ComponentPlacement.RELATED)
                            .addComponent(jPanelFFmpeg, GroupLayout.PREFERRED_SIZE, GroupLayout.DEFAULT_SIZE, GroupLayout.PREFERRED_SIZE)
                            .addPreferredGap(LayoutStyle.ComponentPlacement.RELATED)
                            .addComponent(jButtonHilfe)
                            .addContainerGap(84, Short.MAX_VALUE))
                );
            }
            jScrollPane1.setViewportView(jPanel1);
        }

        GroupLayout layout = new GroupLayout(this);
        setLayout(layout);
        layout.setHorizontalGroup(
            layout.createParallelGroup()
                .addGroup(layout.createSequentialGroup()
                    .addContainerGap()
                    .addComponent(jScrollPane1)
                    .addContainerGap())
        );
        layout.setVerticalGroup(
            layout.createParallelGroup()
                .addGroup(layout.createSequentialGroup()
                    .addContainerGap()
                    .addComponent(jScrollPane1)
                    .addContainerGap())
        );
    }// </editor-fold>//GEN-END:initComponents

    // Variables declaration - do not modify//GEN-BEGIN:variables
    // Generated using JFormDesigner non-commercial license
    private JPanel jPanelVlc;
    private JTextField jTextFieldVlc;
    private JButton jButtonVlcPfad;
    private JButton jButtonVlcSuchen;
    private JButton jButtonHilfe;
    private JPanel jPanelFFmpeg;
    private JTextField jTextFieldFFmpeg;
    private JButton jButtonFFmpegSuchen;
    private JButton jButtonFFmpegPfad;
    // End of variables declaration//GEN-END:variables
}
