/*
 *    MVAboutDialog
 *    Copyright (C) 2013 CrystalPalace
 *    crystalpalace1977@googlemail.com
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
package mediathek.gui;

import com.jidesoft.swing.MarqueePane;
import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.event.ActionEvent;
import java.net.URISyntaxException;
import javax.swing.*;
import javax.swing.border.EmptyBorder;
import mSearch.Const;
import mSearch.tool.Functions;
import static mSearch.tool.Functions.getPathJar;
import mediathek.config.Daten;
import mediathek.config.Konstanten;
import mediathek.res.GetIcon;
import mediathek.tool.BeobMausUrl;
import mediathek.tool.UrlHyperlinkAction;

public class AboutPanel extends javax.swing.JPanel {

    private MarqueePane marqueePane;
    private final JFrame parentFrame;
    final private Color greyColor = new Color(159, 159, 159);

    public AboutPanel(JFrame parent) {
        initComponents();

        parentFrame = parent;

        initHpLink();
        initMarqueePane();
        initProgramPath();
        iniJavaPath();

        this.setBackground(Color.WHITE);
        contentPanel.setBackground(Color.WHITE);
        jPanel1.setBackground(Color.WHITE);
        jPanel2.setBackground(Color.WHITE);
        contentPanel.setBorder(new EmptyBorder(5, 5, 5, 5));

        ImageIcon ic = GetIcon.getIcon("MediathekView.png", "/mediathek/res/", 150, 150);
        lblProgramIcon.setIcon(ic);
        lblProgramIcon.setText("");

        lblProgramName.setFont(new Font("Lucida Grande", Font.BOLD, 24));

    }

    private void initHpLink() {
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
                + "siedlerchr<br /></span><p>\n"
                + "<span class=\"sans\"><b>Dokumentation / Test:</b><br />\n"
                + "styrol<br />\n"
                + "zxsd<br />\n"
                + "siedlerchr<br />\n"
                + "apoleon<br />\n"
                + "hostis<br />\n"
                + "pmshell<br />\n"
                + "clel<br />\n"
                + "derreisende77<br />\n"
                + "thausherr</span><p>\n"
                + "<span class=\"sans\"><b>Ein Dankeschön an alle,<br />"
                + "die zu dieser Software beigetragen haben.</b></span>\n"
                + "<br /><br /><br /></body></html>");

        marqueePane = new MarqueePane(messagePane);
        marqueePane.setStayDelay(3000);
        marqueePane.setScrollDirection(MarqueePane.SCROLL_DIRECTION_UP);
        marqueePane.setScrollAmount(1);

        jPanelLauftext.setLayout(new BorderLayout(0, 0));
        jPanelLauftext.add(marqueePane, BorderLayout.CENTER);
        jPanelLauftext.setPreferredSize(new Dimension(100, 135));
        jPanelLauftext.setMaximumSize(new Dimension(100, 135));
    }

    private void iniJavaPath() {

        pnlJavaInformation.setBackground(Color.WHITE);

        lblJavaVersion_.setText("Version:");
        lblJavaVersion_.setForeground(greyColor);
        lblJavaVersion.setForeground(greyColor);

        lblVmType_.setText("Type:");
        lblVmType_.setForeground(greyColor);
        lblVmType.setForeground(greyColor);

        try {
            lblJavaVersion.setText(System.getProperty("java.version"));
            lblVmType.setText(System.getProperty("java.vm.name") + " (" + System.getProperty("java.vendor") + ")");
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    private void initProgramPath() {

        pnlProgramPaths.setBackground(Color.WHITE);

        lblVersion.setForeground(greyColor);
        lblVersion.setFont(new Font("Lucida Grande", Font.BOLD, 13));

        lblFilmlistPath_.setForeground(greyColor);
        lblFilmlistPath.setForeground(greyColor);

        lblSettingsFilePath_.setForeground(greyColor);
        lblSettingsFilePath.setForeground(greyColor);

        lblProgramPath_.setForeground(greyColor);
        lblProgramPath.setForeground(greyColor);

        try {
            lblVersion.setText("Version " + Const.VERSION + " (" + Functions.getBuildNr() + ")");

            lblSettingsFilePath.setText(Daten.getMediathekXmlFilePath().toAbsolutePath().toString());
            lblFilmlistPath.setText(Daten.getDateiFilmliste());
            lblProgramPath.setText(getPathJar());
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
        lblProgramPath_ = new javax.swing.JLabel();
        lblProgramPath = new javax.swing.JLabel();
        pnlJavaInformation = new javax.swing.JPanel();
        lblJavaVersion_ = new javax.swing.JLabel();
        lblVmType_ = new javax.swing.JLabel();
        lblJavaVersion = new javax.swing.JLabel();
        lblVmType = new javax.swing.JLabel();

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

        lblFilmlistPath_.setText("Filmliste:");

        lblSettingsFilePath_.setText("Einstellungen:");

        lblSettingsFilePath.setText("jLabel1");

        lblFilmlistPath.setText("jLabel2");

        lblProgramPath_.setText("Programmpfad:");

        lblProgramPath.setText("jLabel2");

        javax.swing.GroupLayout pnlProgramPathsLayout = new javax.swing.GroupLayout(pnlProgramPaths);
        pnlProgramPaths.setLayout(pnlProgramPathsLayout);
        pnlProgramPathsLayout.setHorizontalGroup(
            pnlProgramPathsLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(pnlProgramPathsLayout.createSequentialGroup()
                .addGroup(pnlProgramPathsLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.TRAILING)
                    .addComponent(lblProgramPath_)
                    .addGroup(pnlProgramPathsLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                        .addGroup(pnlProgramPathsLayout.createSequentialGroup()
                            .addGap(50, 50, 50)
                            .addComponent(lblFilmlistPath_))
                        .addGroup(pnlProgramPathsLayout.createSequentialGroup()
                            .addContainerGap()
                            .addComponent(lblSettingsFilePath_))))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                .addGroup(pnlProgramPathsLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addComponent(lblFilmlistPath)
                    .addComponent(lblSettingsFilePath)
                    .addComponent(lblProgramPath))
                .addContainerGap(javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
        );
        pnlProgramPathsLayout.setVerticalGroup(
            pnlProgramPathsLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(pnlProgramPathsLayout.createSequentialGroup()
                .addContainerGap()
                .addGroup(pnlProgramPathsLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(lblFilmlistPath_)
                    .addComponent(lblFilmlistPath))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addGroup(pnlProgramPathsLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addComponent(lblSettingsFilePath)
                    .addComponent(lblSettingsFilePath_))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addGroup(pnlProgramPathsLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(lblProgramPath_)
                    .addComponent(lblProgramPath))
                .addContainerGap(javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
        );

        pnlJavaInformation.setBorder(javax.swing.BorderFactory.createTitledBorder(javax.swing.BorderFactory.createLineBorder(new java.awt.Color(204, 204, 204)), "Java Information", javax.swing.border.TitledBorder.DEFAULT_JUSTIFICATION, javax.swing.border.TitledBorder.DEFAULT_POSITION, new java.awt.Font("Dialog", 1, 12), new java.awt.Color(0, 0, 0))); // NOI18N

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
                .addGap(17, 17, 17)
                .addComponent(pnlJavaInformation, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addContainerGap(javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
        );

        contentPanel.add(jPanel2, java.awt.BorderLayout.CENTER);

        javax.swing.GroupLayout layout = new javax.swing.GroupLayout(this);
        this.setLayout(layout);
        layout.setHorizontalGroup(
            layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addComponent(contentPanel, javax.swing.GroupLayout.DEFAULT_SIZE, 316, Short.MAX_VALUE)
        );
        layout.setVerticalGroup(
            layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addComponent(contentPanel, javax.swing.GroupLayout.DEFAULT_SIZE, 510, Short.MAX_VALUE)
        );
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


    // Variables declaration - do not modify//GEN-BEGIN:variables
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
    private javax.swing.JLabel lblProgramPath;
    private javax.swing.JLabel lblProgramPath_;
    private javax.swing.JLabel lblSettingsFilePath;
    private javax.swing.JLabel lblSettingsFilePath_;
    private javax.swing.JLabel lblVersion;
    private javax.swing.JLabel lblVmType;
    private javax.swing.JLabel lblVmType_;
    private javax.swing.JPanel pnlJavaInformation;
    private javax.swing.JPanel pnlProgramPaths;
    // End of variables declaration//GEN-END:variables

}
