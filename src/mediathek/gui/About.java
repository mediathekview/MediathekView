/*
 * MediathekView
 * Copyright (C) 2014 W. Xaver
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
package mediathek.gui;

import com.jidesoft.swing.MarqueePane;
import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.event.ActionEvent;
import java.net.URISyntaxException;
import java.nio.file.Path;
import javax.swing.*;
import javax.swing.border.EmptyBorder;
import javax.swing.border.TitledBorder;
import mediathek.daten.Daten;
import mediathek.tool.*;


public class About extends javax.swing.JDialog {
    
    private MarqueePane marqueePane;
    private final Boolean isRunningOnMac;
    private final JFrame parentFrame;
    
    public About(JFrame parent, final Boolean isRunningOnMac) {
        super(parent);
        initComponents();
        
        parentFrame = parent;
        setModal(true);
        setDefaultCloseOperation(JDialog.DISPOSE_ON_CLOSE);
        this.isRunningOnMac = isRunningOnMac;
        new EscBeenden(this) {
            @Override
            public void beenden_(JDialog d) {
                d.dispose();
            }
        };
        
        setResizable(false);
        setModalityType(ModalityType.APPLICATION_MODAL);
        setBounds(100, 100, 790, 491);
        
        initialize();
        
        this.setBackground(Color.WHITE);
        contentPanel.setBackground(Color.WHITE);
        jPanel1.setBackground(Color.WHITE);
        jPanel2.setBackground(Color.WHITE);
        contentPanel.setBorder(new EmptyBorder(5, 5, 5, 5));
        
        lblProgramIcon.setIcon(new ImageIcon(About.class.getResource("/mediathek/res/MediathekView.png")));
        lblProgramIcon.setText("");
        
        lblProgramName.setFont(new Font("Lucida Grande", Font.BOLD, 24));
        
        hprlnkWebsite.setHorizontalAlignment(SwingConstants.LEFT);
        try {
            hprlnkWebsite.setAction(new WebsiteHyperlinkAction());
        } catch (URISyntaxException e) {
            e.printStackTrace();
        }
        hprlnkWebsite.setText("Website");
        hprlnkWebsite.addMouseListener(new BeobMausUrl(Konstanten.ADRESSE_WEBSITE));
        
        hprlnkDonation.setHorizontalAlignment(SwingConstants.LEFT);
        try {
            hprlnkDonation.setAction(new DonationHyperlinkAction());
        } catch (URISyntaxException e) {
            e.printStackTrace();
        }
        hprlnkDonation.setText("Spende");
        hprlnkDonation.addMouseListener(new BeobMausUrl(Konstanten.ADRESSE_DONATION));
        
        hprlnkAnleitung.setHorizontalAlignment(SwingConstants.LEFT);
        try {
            hprlnkAnleitung.setAction(new AnleitungHyperlinkAction());
        } catch (URISyntaxException e1) {
            e1.printStackTrace();
        }
        hprlnkAnleitung.setText("Anleitung");
        hprlnkAnleitung.addMouseListener(new BeobMausUrl(Konstanten.ADRESSE_ANLEITUNG));
        
        hprlnkForum.setHorizontalAlignment(SwingConstants.LEFT);
        try {
            hprlnkForum.setAction(new ForumHyperlinkAction());
        } catch (URISyntaxException e) {
            e.printStackTrace();
        }
        hprlnkForum.setText("Forum");
        hprlnkForum.addMouseListener(new BeobMausUrl(Konstanten.ADRESSE_FORUM));
        
        okButton.setAction(new CloseDialogAction(this));
        pack();
    }
    
    private void initMarqueePane() {
        final JEditorPane messagePane = new JEditorPane();
        messagePane.setEditable(false);
        messagePane.setFocusable(false);
        messagePane.setContentType("text/html");
        messagePane.setText("<html xmlns=\"http://www.w3.org/1999/xhtml\">\n"
                + "<head><style type=\"text/css\"> .sans { font-family: Verdana, Geneva, sans-serif; }</style></head>\n"
                + "<body>\n"
                + "<span class=\"sans\"><b>http://zdfmediathk.sourceforge.net</b><br /></span>\n"
                + "<p><span class=\"sans\"><b>Autoren:</b><br />\n"
                + "Xaver W. (W.Xaver [at] googlemail [dot] com)<br />\n"
                + "Christian F. (crystalpalace1977 [at] googlemail [dot] com)<br />\n"
                + "Patrick<br />\n"
                + "thausherr<br />\n"
                + "Andreas M.<br />\n"
                + "siedlerchr<br /></span><p>\n"
                + "<span class=\"sans\"><b>Dokumentation / Test:</b><br />\n"
                + "styrol<br />\n"
                + "hostis<br />\n"
                + "pmshell<br />\n"
                + "thausherr<br />\n"
                + "apoleon<br />\n"
                + "siedlerchr<br />\n"
                + "werner252<br />\n"
                + "thomas5<br />\n"
                + "frankypsilon</span><p>\n"
                + "<span class=\"sans\"><b>Ein Dankeschön an alle, die zu dieser Software beigetragen haben.</b></span>\n"
                + "<br /><br /><br /></body></html>");
        
        marqueePane = new MarqueePane(messagePane);
        marqueePane.setStayDelay(3000);
        marqueePane.setScrollDirection(MarqueePane.SCROLL_DIRECTION_UP);
        marqueePane.setScrollAmount(1);
        
    }
    
    private void setupJavaInformation() {
        lblJavaVersion.setText(System.getProperty("java.version"));
        String strVmType = System.getProperty("java.vm.name");
        strVmType += " (";
        strVmType += System.getProperty("java.vendor");
        strVmType += ")";
        lblVmType.setText(strVmType);
    }
    
    private void setupVersionString() {
        String strVersion = "Version ";
        strVersion += Konstanten.VERSION;
        strVersion += " (";
        strVersion += MVFunctionSys.getBuildNr();
        strVersion += ")";
        lblVersion.setText(strVersion);
    }
    
    private void initialize() {
        initMarqueePane();
        jPanelLauftext.setLayout(new BorderLayout(0, 0));
        jPanelLauftext.add(marqueePane, BorderLayout.CENTER);
        jPanelLauftext.setPreferredSize(new Dimension(100, 135));
        jPanelLauftext.setMaximumSize(new Dimension(100, 135));
        
        TitledBorder border = new TitledBorder("Programmpfade");
        pnlProgramPaths.setBorder(border);
        pnlProgramPaths.setBackground(Color.WHITE);
        
        border = new TitledBorder("Java Information");
        pnlJavaInformation.setBorder(border);
        pnlJavaInformation.setBackground(Color.WHITE);
        
        Color greyColor = new Color(159, 159, 159);
        lblVersion.setForeground(greyColor);
        lblVersion.setFont(new Font("Lucida Grande", Font.BOLD, 13));
        
        lblJavaVersion_.setText("Version:");
        lblJavaVersion_.setForeground(greyColor);
        lblJavaVersion.setForeground(greyColor);
        
        lblVmType_.setText("Type:");
        lblVmType_.setForeground(greyColor);
        lblVmType.setForeground(greyColor);
        
        lblFilmlistPath_.setText("Filmliste:");
        lblFilmlistPath_.setHorizontalAlignment(SwingConstants.RIGHT);
        lblFilmlistPath_.setForeground(greyColor);
        lblFilmlistPath.setForeground(greyColor);
        
        lblSettingsFilePath_.setText("Einstellungen:");
        lblSettingsFilePath_.setForeground(greyColor);
        lblSettingsFilePath.setForeground(greyColor);
        
        try {
            setupVersionString();
            setupJavaInformation();
            // Programmpfade
            final Path xmlFilePath = Daten.getMediathekXmlFilePath();
            lblSettingsFilePath.setText(xmlFilePath.toAbsolutePath().toString());
            lblFilmlistPath.setText(Daten.getDateiFilmliste());

            // auf dem Mac brauchen wir den Schließen Button nicht..
            if (isRunningOnMac) {
                this.remove(buttonPane);
            }
            
        } catch (Exception e) {
            e.printStackTrace();
        }
    }
    
    @SuppressWarnings("unchecked")
    // <editor-fold defaultstate="collapsed" desc="Generated Code">//GEN-BEGIN:initComponents
    private void initComponents() {

        contentPanel = new javax.swing.JPanel();
        jPanel1 = new javax.swing.JPanel();
        lblProgramIcon = new javax.swing.JLabel();
        hprlnkWebsite = new org.jdesktop.swingx.JXHyperlink();
        hprlnkDonation = new org.jdesktop.swingx.JXHyperlink();
        hprlnkForum = new org.jdesktop.swingx.JXHyperlink();
        hprlnkAnleitung = new org.jdesktop.swingx.JXHyperlink();
        jPanel2 = new javax.swing.JPanel();
        lblProgramName = new javax.swing.JLabel();
        lblVersion = new javax.swing.JLabel();
        jPanelLauftext = new javax.swing.JPanel();
        pnlProgramPaths = new javax.swing.JPanel();
        lblFilmlistPath_ = new javax.swing.JLabel();
        lblSettingsFilePath_ = new javax.swing.JLabel();
        lblSettingsFilePath = new javax.swing.JLabel();
        lblFilmlistPath = new javax.swing.JLabel();
        pnlJavaInformation = new javax.swing.JPanel();
        lblJavaVersion_ = new javax.swing.JLabel();
        lblVmType_ = new javax.swing.JLabel();
        lblJavaVersion = new javax.swing.JLabel();
        lblVmType = new javax.swing.JLabel();
        buttonPane = new javax.swing.JPanel();
        okButton = new javax.swing.JButton();

        setDefaultCloseOperation(javax.swing.WindowConstants.DISPOSE_ON_CLOSE);
        setResizable(false);

        contentPanel.setLayout(new java.awt.BorderLayout());

        lblProgramIcon.setText("icon");

        hprlnkWebsite.setText("Website");

        hprlnkDonation.setText("Spende");

        hprlnkForum.setText("Forum");

        hprlnkAnleitung.setText("Anleitung");

        javax.swing.GroupLayout jPanel1Layout = new javax.swing.GroupLayout(jPanel1);
        jPanel1.setLayout(jPanel1Layout);
        jPanel1Layout.setHorizontalGroup(
            jPanel1Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel1Layout.createSequentialGroup()
                .addContainerGap()
                .addGroup(jPanel1Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addComponent(lblProgramIcon)
                    .addComponent(hprlnkWebsite, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                    .addComponent(hprlnkDonation, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                    .addComponent(hprlnkForum, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                    .addComponent(hprlnkAnleitung, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE))
                .addContainerGap(javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
        );
        jPanel1Layout.setVerticalGroup(
            jPanel1Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel1Layout.createSequentialGroup()
                .addContainerGap()
                .addComponent(lblProgramIcon)
                .addGap(18, 18, 18)
                .addComponent(hprlnkWebsite, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(hprlnkDonation, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(hprlnkForum, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(hprlnkAnleitung, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addContainerGap(javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
        );

        contentPanel.add(jPanel1, java.awt.BorderLayout.LINE_START);

        lblProgramName.setText("MediathekView");

        lblVersion.setText("Version");

        jPanelLauftext.setBorder(javax.swing.BorderFactory.createLineBorder(new java.awt.Color(204, 204, 204)));

        javax.swing.GroupLayout jPanelLauftextLayout = new javax.swing.GroupLayout(jPanelLauftext);
        jPanelLauftext.setLayout(jPanelLauftextLayout);
        jPanelLauftextLayout.setHorizontalGroup(
            jPanelLauftextLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGap(0, 0, Short.MAX_VALUE)
        );
        jPanelLauftextLayout.setVerticalGroup(
            jPanelLauftextLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGap(0, 216, Short.MAX_VALUE)
        );

        pnlProgramPaths.setBorder(javax.swing.BorderFactory.createTitledBorder(javax.swing.BorderFactory.createLineBorder(new java.awt.Color(204, 204, 204)), "Programmpfade"));

        lblFilmlistPath_.setText("Filmliste");

        lblSettingsFilePath_.setText("Einstellungen");

        lblSettingsFilePath.setText("jLabel1");

        lblFilmlistPath.setText("jLabel2");

        javax.swing.GroupLayout pnlProgramPathsLayout = new javax.swing.GroupLayout(pnlProgramPaths);
        pnlProgramPaths.setLayout(pnlProgramPathsLayout);
        pnlProgramPathsLayout.setHorizontalGroup(
            pnlProgramPathsLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(pnlProgramPathsLayout.createSequentialGroup()
                .addContainerGap()
                .addGroup(pnlProgramPathsLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addComponent(lblSettingsFilePath_)
                    .addComponent(lblFilmlistPath_))
                .addGap(18, 18, 18)
                .addGroup(pnlProgramPathsLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addComponent(lblFilmlistPath)
                    .addComponent(lblSettingsFilePath))
                .addContainerGap(322, Short.MAX_VALUE))
        );
        pnlProgramPathsLayout.setVerticalGroup(
            pnlProgramPathsLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(pnlProgramPathsLayout.createSequentialGroup()
                .addContainerGap()
                .addGroup(pnlProgramPathsLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(lblFilmlistPath_)
                    .addComponent(lblFilmlistPath))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                .addGroup(pnlProgramPathsLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(lblSettingsFilePath_)
                    .addComponent(lblSettingsFilePath))
                .addContainerGap(javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
        );

        pnlJavaInformation.setBorder(javax.swing.BorderFactory.createTitledBorder(javax.swing.BorderFactory.createLineBorder(new java.awt.Color(204, 204, 204)), "Java Informationen", javax.swing.border.TitledBorder.DEFAULT_JUSTIFICATION, javax.swing.border.TitledBorder.DEFAULT_POSITION, new java.awt.Font("Dialog", 1, 12), new java.awt.Color(0, 0, 0))); // NOI18N

        lblJavaVersion_.setText("Java");

        lblVmType_.setText("VM-Type");

        lblJavaVersion.setText("jLabel1");

        lblVmType.setText("jLabel2");

        javax.swing.GroupLayout pnlJavaInformationLayout = new javax.swing.GroupLayout(pnlJavaInformation);
        pnlJavaInformation.setLayout(pnlJavaInformationLayout);
        pnlJavaInformationLayout.setHorizontalGroup(
            pnlJavaInformationLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(pnlJavaInformationLayout.createSequentialGroup()
                .addContainerGap()
                .addGroup(pnlJavaInformationLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addComponent(lblVmType_)
                    .addComponent(lblJavaVersion_))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                .addGroup(pnlJavaInformationLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addComponent(lblJavaVersion)
                    .addComponent(lblVmType))
                .addContainerGap(javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
        );
        pnlJavaInformationLayout.setVerticalGroup(
            pnlJavaInformationLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(pnlJavaInformationLayout.createSequentialGroup()
                .addContainerGap()
                .addGroup(pnlJavaInformationLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(lblJavaVersion_)
                    .addComponent(lblJavaVersion))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                .addGroup(pnlJavaInformationLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(lblVmType_)
                    .addComponent(lblVmType))
                .addContainerGap(javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
        );

        javax.swing.GroupLayout jPanel2Layout = new javax.swing.GroupLayout(jPanel2);
        jPanel2.setLayout(jPanel2Layout);
        jPanel2Layout.setHorizontalGroup(
            jPanel2Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel2Layout.createSequentialGroup()
                .addContainerGap()
                .addGroup(jPanel2Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addComponent(pnlProgramPaths, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                    .addComponent(pnlJavaInformation, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                    .addComponent(jPanelLauftext, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                    .addComponent(lblProgramName)
                    .addComponent(lblVersion))
                .addContainerGap())
        );
        jPanel2Layout.setVerticalGroup(
            jPanel2Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel2Layout.createSequentialGroup()
                .addContainerGap()
                .addComponent(lblProgramName)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(lblVersion)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                .addComponent(jPanelLauftext, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                .addComponent(pnlProgramPaths, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                .addComponent(pnlJavaInformation, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE))
        );

        contentPanel.add(jPanel2, java.awt.BorderLayout.CENTER);

        okButton.setText("Schließen");

        javax.swing.GroupLayout buttonPaneLayout = new javax.swing.GroupLayout(buttonPane);
        buttonPane.setLayout(buttonPaneLayout);
        buttonPaneLayout.setHorizontalGroup(
            buttonPaneLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(javax.swing.GroupLayout.Alignment.TRAILING, buttonPaneLayout.createSequentialGroup()
                .addContainerGap(javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                .addComponent(okButton)
                .addContainerGap())
        );
        buttonPaneLayout.setVerticalGroup(
            buttonPaneLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(buttonPaneLayout.createSequentialGroup()
                .addGap(3, 3, 3)
                .addComponent(okButton)
                .addGap(3, 3, 3))
        );

        javax.swing.GroupLayout layout = new javax.swing.GroupLayout(getContentPane());
        getContentPane().setLayout(layout);
        layout.setHorizontalGroup(
            layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(layout.createSequentialGroup()
                .addContainerGap()
                .addComponent(buttonPane, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
            .addComponent(contentPanel, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
        );
        layout.setVerticalGroup(
            layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(layout.createSequentialGroup()
                .addComponent(contentPanel, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(buttonPane, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE))
        );

        pack();
    }// </editor-fold>//GEN-END:initComponents

    private class WebsiteHyperlinkAction extends AbstractAction {
        
        public WebsiteHyperlinkAction() throws URISyntaxException {
            putValue(SHORT_DESCRIPTION, Konstanten.ADRESSE_WEBSITE);
            putValue(LONG_DESCRIPTION, Konstanten.ADRESSE_WEBSITE);
        }
        
        @Override
        public void actionPerformed(ActionEvent e) {
            try {
                UrlHyperlinkAction.urlOeffnen(parentFrame, Konstanten.ADRESSE_WEBSITE);
            } catch (URISyntaxException ignored) {
            }
        }
    }
    
    private class DonationHyperlinkAction extends AbstractAction {
        
        public DonationHyperlinkAction() throws URISyntaxException {
            putValue(SHORT_DESCRIPTION, Konstanten.ADRESSE_DONATION);
            putValue(LONG_DESCRIPTION, Konstanten.ADRESSE_DONATION);
        }
        
        @Override
        public void actionPerformed(ActionEvent e) {
            try {
                UrlHyperlinkAction.urlOeffnen(parentFrame, Konstanten.ADRESSE_DONATION);
            } catch (URISyntaxException ignored) {
            }
        }
    }
    
    private class ForumHyperlinkAction extends AbstractAction {
        
        public ForumHyperlinkAction() throws URISyntaxException {
            putValue(SHORT_DESCRIPTION, Konstanten.ADRESSE_FORUM);
            putValue(LONG_DESCRIPTION, Konstanten.ADRESSE_FORUM);
        }
        
        @Override
        public void actionPerformed(ActionEvent e) {
            try {
                UrlHyperlinkAction.urlOeffnen(parentFrame, Konstanten.ADRESSE_FORUM);
            } catch (URISyntaxException ignored) {
            }
        }
    }
    
    private class AnleitungHyperlinkAction extends AbstractAction {
        
        public AnleitungHyperlinkAction() throws URISyntaxException {
            putValue(SHORT_DESCRIPTION, Konstanten.ADRESSE_ANLEITUNG);
            putValue(LONG_DESCRIPTION, Konstanten.ADRESSE_ANLEITUNG);
        }
        
        @Override
        public void actionPerformed(ActionEvent e) {
            try {
                UrlHyperlinkAction.urlOeffnen(parentFrame, Konstanten.ADRESSE_ANLEITUNG);
            } catch (URISyntaxException ignored) {
            }
        }
    }
    
    private class CloseDialogAction extends AbstractAction {
        
        private final About dlg;
        
        public CloseDialogAction(About dlg) {
            super();
            putValue(NAME, "Schließen");
            putValue(SHORT_DESCRIPTION, "Dialog schließen");
            this.dlg = dlg;
        }
        
        public void actionPerformed(ActionEvent e) {
            dlg.dispose();
        }
    }


    // Variables declaration - do not modify//GEN-BEGIN:variables
    private javax.swing.JPanel buttonPane;
    private javax.swing.JPanel contentPanel;
    private org.jdesktop.swingx.JXHyperlink hprlnkAnleitung;
    private org.jdesktop.swingx.JXHyperlink hprlnkDonation;
    private org.jdesktop.swingx.JXHyperlink hprlnkForum;
    private org.jdesktop.swingx.JXHyperlink hprlnkWebsite;
    private javax.swing.JPanel jPanel1;
    private javax.swing.JPanel jPanel2;
    private javax.swing.JPanel jPanelLauftext;
    private javax.swing.JLabel lblFilmlistPath;
    private javax.swing.JLabel lblFilmlistPath_;
    private javax.swing.JLabel lblJavaVersion;
    private javax.swing.JLabel lblJavaVersion_;
    private javax.swing.JLabel lblProgramIcon;
    private javax.swing.JLabel lblProgramName;
    private javax.swing.JLabel lblSettingsFilePath;
    private javax.swing.JLabel lblSettingsFilePath_;
    private javax.swing.JLabel lblVersion;
    private javax.swing.JLabel lblVmType;
    private javax.swing.JLabel lblVmType_;
    private javax.swing.JButton okButton;
    private javax.swing.JPanel pnlJavaInformation;
    private javax.swing.JPanel pnlProgramPaths;
    // End of variables declaration//GEN-END:variables

}
