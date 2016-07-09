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
import java.awt.FlowLayout;
import java.awt.Font;
import java.awt.event.ActionEvent;
import java.net.URISyntaxException;
import java.nio.file.Path;
import javax.swing.GroupLayout.Alignment;
import javax.swing.LayoutStyle.ComponentPlacement;
import javax.swing.*;
import javax.swing.border.EmptyBorder;
import javax.swing.border.TitledBorder;
import mediathek.daten.Daten;
import mediathek.tool.*;
import org.jdesktop.swingx.JXHyperlink;

@SuppressWarnings("serial")
public class MVAboutDialog extends JDialog {

    private final JLabel lblVersion = new JLabel();
    private final JPanel buttonPane = new JPanel();
    private final JLabel lblFilmlistPath = new JLabel();
    private final JLabel lblSettingsFilePath = new JLabel();
    private final Boolean isRunningOnMac;
    private final JLabel lblJavaVersion = new JLabel();
    private final JLabel lblVmType = new JLabel();
    private MarqueePane marqueePane;
    private final JFrame parentFrame;

    private void setupVersionString() {
        String strVersion = "Version ";
        strVersion += Konstanten.VERSION;
        strVersion += " (";
        strVersion += MVFunctionSys.getBuildNr();
        strVersion += ")";

        lblVersion.setText(strVersion);
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

    private void initialize() {
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

    public MVAboutDialog(JFrame parent, final Boolean isRunningOnMac) {
        super(parent);
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

        initMarqueePane();

        setResizable(false);
        setModalityType(ModalityType.APPLICATION_MODAL);
        setBounds(100, 100, 790, 491);
        getContentPane().setLayout(new BorderLayout());
        JPanel contentPanel = new JPanel();
        contentPanel.setBackground(Color.WHITE);
        contentPanel.setBorder(new EmptyBorder(5, 5, 5, 5));
        getContentPane().add(contentPanel, BorderLayout.CENTER);

        JLabel lblProgramIcon = new JLabel();
        lblProgramIcon.setIcon(new ImageIcon(MVAboutDialog.class
                .getResource("/mediathek/res/MediathekView.png")));

        JLabel lblProgramName = new JLabel("MediathekView");
        lblProgramName.setFont(new Font("Lucida Grande", Font.BOLD, 24));

        Color greyColor = new Color(159, 159, 159);
        lblVersion.setForeground(greyColor);
        lblVersion.setFont(new Font("Lucida Grande", Font.BOLD, 13));

        JXHyperlink hprlnkWebsite = new JXHyperlink();
        hprlnkWebsite.setHorizontalAlignment(SwingConstants.LEFT);
        try {
            hprlnkWebsite.setAction(new WebsiteHyperlinkAction());
        } catch (URISyntaxException e) {
            e.printStackTrace();
        }
        hprlnkWebsite.setText("Website");
        hprlnkWebsite.addMouseListener(new BeobMausUrl(Konstanten.ADRESSE_WEBSITE));

        JXHyperlink hprlnkDonation = new JXHyperlink();
        hprlnkDonation.setHorizontalAlignment(SwingConstants.LEFT);
        try {
            hprlnkDonation.setAction(new DonationHyperlinkAction());
        } catch (URISyntaxException e) {
            e.printStackTrace();
        }
        hprlnkDonation.setText("Spende");
        hprlnkDonation.addMouseListener(new BeobMausUrl(Konstanten.ADRESSE_DONATION));

        JXHyperlink hprlnkAnleitung = new JXHyperlink();
        hprlnkAnleitung.setHorizontalAlignment(SwingConstants.LEFT);
        try {
            hprlnkAnleitung.setAction(new AnleitungHyperlinkAction());
        } catch (URISyntaxException e1) {
            e1.printStackTrace();
        }
        hprlnkAnleitung.setText("Anleitung");
        hprlnkAnleitung.addMouseListener(new BeobMausUrl(Konstanten.ADRESSE_ANLEITUNG));

        JXHyperlink hprlnkForum = new JXHyperlink();
        hprlnkForum.setHorizontalAlignment(SwingConstants.LEFT);
        try {
            hprlnkForum.setAction(new ForumHyperlinkAction());
        } catch (URISyntaxException e) {
            e.printStackTrace();
        }
        hprlnkForum.setText("Forum");
        hprlnkForum.addMouseListener(new BeobMausUrl(Konstanten.ADRESSE_FORUM));

        JPanel pnlProgramPaths = new JPanel();
        TitledBorder border = new TitledBorder("Programmpfade");
        pnlProgramPaths.setBorder(border);
        pnlProgramPaths.setBackground(Color.WHITE);

        JPanel pnlJavaInformation = new JPanel();
        border = new TitledBorder("Java Information");
        pnlJavaInformation.setBorder(border);
        pnlJavaInformation.setBackground(Color.WHITE);

        GroupLayout gl_contentPanel = new GroupLayout(contentPanel);

        gl_contentPanel.setHorizontalGroup(gl_contentPanel.createParallelGroup(
                Alignment.LEADING).addGroup(
                        gl_contentPanel
                        .createSequentialGroup()
                        .addContainerGap()
                        .addGroup(gl_contentPanel
                                .createParallelGroup(Alignment.LEADING).addComponent(lblProgramIcon)
                                .addComponent(hprlnkWebsite,
                                        GroupLayout.PREFERRED_SIZE,
                                        GroupLayout.DEFAULT_SIZE,
                                        GroupLayout.PREFERRED_SIZE)
                                .addComponent(hprlnkDonation,
                                        GroupLayout.PREFERRED_SIZE,
                                        GroupLayout.DEFAULT_SIZE,
                                        GroupLayout.PREFERRED_SIZE)
                                .addComponent(hprlnkForum,
                                        GroupLayout.PREFERRED_SIZE,
                                        GroupLayout.DEFAULT_SIZE,
                                        GroupLayout.PREFERRED_SIZE)
                                .addComponent(hprlnkAnleitung,
                                        GroupLayout.PREFERRED_SIZE,
                                        GroupLayout.DEFAULT_SIZE,
                                        GroupLayout.PREFERRED_SIZE))
                        .addPreferredGap(ComponentPlacement.RELATED)
                        .addGroup(gl_contentPanel.createParallelGroup(Alignment.LEADING)
                                .addComponent(marqueePane, GroupLayout.DEFAULT_SIZE, 464, Short.MAX_VALUE)
                                .addComponent(pnlJavaInformation, Alignment.TRAILING, GroupLayout.DEFAULT_SIZE, 464, Short.MAX_VALUE)
                                .addComponent(pnlProgramPaths, GroupLayout.DEFAULT_SIZE, 464, Short.MAX_VALUE)
                                .addComponent(lblProgramName)
                                .addComponent(lblVersion, Alignment.TRAILING, GroupLayout.DEFAULT_SIZE, 464, Short.MAX_VALUE))
                        .addContainerGap()));
        gl_contentPanel.setVerticalGroup(
                gl_contentPanel.createParallelGroup(Alignment.LEADING)
                .addGroup(gl_contentPanel.createSequentialGroup()
                        .addContainerGap()
                        .addGroup(gl_contentPanel.createParallelGroup(Alignment.LEADING)
                                .addGroup(gl_contentPanel.createSequentialGroup()
                                        .addComponent(lblProgramName)
                                        .addPreferredGap(ComponentPlacement.RELATED)
                                        .addComponent(lblVersion)
                                        .addPreferredGap(ComponentPlacement.UNRELATED)
                                        .addComponent(marqueePane, GroupLayout.PREFERRED_SIZE, 131, GroupLayout.PREFERRED_SIZE)
                                        .addPreferredGap(ComponentPlacement.UNRELATED)
                                        .addComponent(pnlProgramPaths, GroupLayout.PREFERRED_SIZE, 78, GroupLayout.PREFERRED_SIZE)
                                        .addPreferredGap(ComponentPlacement.RELATED)
                                        .addComponent(pnlJavaInformation, GroupLayout.PREFERRED_SIZE, 80, GroupLayout.PREFERRED_SIZE))
                                .addGroup(gl_contentPanel.createSequentialGroup()
                                        .addComponent(lblProgramIcon)
                                        .addPreferredGap(ComponentPlacement.RELATED)
                                        .addComponent(hprlnkWebsite, GroupLayout.PREFERRED_SIZE, GroupLayout.DEFAULT_SIZE, GroupLayout.PREFERRED_SIZE)
                                        .addPreferredGap(ComponentPlacement.RELATED)
                                        .addComponent(hprlnkDonation, GroupLayout.PREFERRED_SIZE, GroupLayout.DEFAULT_SIZE, GroupLayout.PREFERRED_SIZE)
                                        .addPreferredGap(ComponentPlacement.RELATED)
                                        .addComponent(hprlnkForum, GroupLayout.PREFERRED_SIZE, GroupLayout.DEFAULT_SIZE, GroupLayout.PREFERRED_SIZE)
                                        .addPreferredGap(ComponentPlacement.RELATED)
                                        .addComponent(hprlnkAnleitung, GroupLayout.PREFERRED_SIZE, GroupLayout.DEFAULT_SIZE, GroupLayout.PREFERRED_SIZE)))
                        .addContainerGap(28, Short.MAX_VALUE)));

        JLabel lblVersion_1 = new JLabel("Version:");
        lblVersion_1.setForeground(greyColor);

        JLabel lblJavaType = new JLabel("Type:");
        lblJavaType.setForeground(greyColor);

        lblJavaVersion.setForeground(greyColor);

        lblVmType.setForeground(greyColor);
        GroupLayout gl_panel;
        GroupLayout gl_pnlJavaInformation = new GroupLayout(pnlJavaInformation);
        gl_pnlJavaInformation.setHorizontalGroup(
                gl_pnlJavaInformation.createParallelGroup(Alignment.LEADING)
                .addGroup(gl_pnlJavaInformation.createSequentialGroup()
                        .addContainerGap()
                        .addGroup(gl_pnlJavaInformation.createParallelGroup(Alignment.TRAILING)
                                .addComponent(lblJavaType)
                                .addComponent(lblVersion_1))
                        .addPreferredGap(ComponentPlacement.RELATED)
                        .addGroup(gl_pnlJavaInformation.createParallelGroup(Alignment.LEADING)
                                .addComponent(lblVmType, GroupLayout.DEFAULT_SIZE, 395, Short.MAX_VALUE)
                                .addComponent(lblJavaVersion, GroupLayout.DEFAULT_SIZE, 395, Short.MAX_VALUE))
                        .addContainerGap()));
        gl_pnlJavaInformation.setVerticalGroup(
                gl_pnlJavaInformation.createParallelGroup(Alignment.LEADING)
                .addGroup(gl_pnlJavaInformation.createSequentialGroup()
                        .addContainerGap()
                        .addGroup(gl_pnlJavaInformation.createParallelGroup(Alignment.BASELINE)
                                .addComponent(lblVersion_1)
                                .addComponent(lblJavaVersion))
                        .addPreferredGap(ComponentPlacement.RELATED)
                        .addGroup(gl_pnlJavaInformation.createParallelGroup(Alignment.BASELINE)
                                .addComponent(lblJavaType)
                                .addComponent(lblVmType))
                        .addContainerGap(52, Short.MAX_VALUE)));
        pnlJavaInformation.setLayout(gl_pnlJavaInformation);

        JLabel lblFilmliste = new JLabel("Filmliste:");
        lblFilmliste.setHorizontalAlignment(SwingConstants.RIGHT);
        lblFilmliste.setForeground(greyColor);

        lblFilmlistPath.setForeground(greyColor);

        JLabel lblEinstellungen = new JLabel("Einstellungen:");
        lblEinstellungen.setForeground(greyColor);

        lblSettingsFilePath.setForeground(greyColor);
        gl_panel = new GroupLayout(pnlProgramPaths);
        gl_panel.setHorizontalGroup(gl_panel
                .createParallelGroup(Alignment.LEADING)
                .addGroup(
                        gl_panel.createSequentialGroup()
                        .addContainerGap()
                        .addGroup(
                                gl_panel.createParallelGroup(
                                        Alignment.LEADING, false)
                                .addComponent(
                                        lblFilmliste,
                                        GroupLayout.DEFAULT_SIZE,
                                        GroupLayout.DEFAULT_SIZE,
                                        Short.MAX_VALUE)
                                .addComponent(
                                        lblEinstellungen,
                                        GroupLayout.DEFAULT_SIZE,
                                        GroupLayout.DEFAULT_SIZE,
                                        Short.MAX_VALUE))
                        .addPreferredGap(ComponentPlacement.RELATED)
                        .addGroup(
                                gl_panel.createParallelGroup(
                                        Alignment.TRAILING)
                                .addComponent(
                                        lblSettingsFilePath,
                                        GroupLayout.DEFAULT_SIZE,
                                        345, Short.MAX_VALUE)
                                .addComponent(
                                        lblFilmlistPath,
                                        GroupLayout.DEFAULT_SIZE,
                                        345, Short.MAX_VALUE))
                        .addContainerGap()));
        gl_panel.setVerticalGroup(gl_panel.createParallelGroup(
                Alignment.LEADING)
                .addGroup(
                        gl_panel.createSequentialGroup()
                        .addContainerGap()
                        .addGroup(
                                gl_panel.createParallelGroup(
                                        Alignment.BASELINE)
                                .addComponent(lblFilmliste)
                                .addComponent(lblFilmlistPath))
                        .addPreferredGap(ComponentPlacement.RELATED)
                        .addGroup(
                                gl_panel.createParallelGroup(
                                        Alignment.BASELINE)
                                .addComponent(lblEinstellungen)
                                .addComponent(
                                        lblSettingsFilePath))
                        .addContainerGap(10, Short.MAX_VALUE)));
        pnlProgramPaths.setLayout(gl_panel);
        contentPanel.setLayout(gl_contentPanel);
        {
            buttonPane.setLayout(new FlowLayout(FlowLayout.RIGHT));
            getContentPane().add(buttonPane, BorderLayout.SOUTH);
            {
                JButton okButton = new JButton("Schlie\u00DFen");
                okButton.setAction(new CloseDialogAction(this));
                buttonPane.add(okButton);
                getRootPane().setDefaultButton(okButton);
            }
        }
        initialize();

        pack();
    }

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

        private final MVAboutDialog dlg;

        public CloseDialogAction(MVAboutDialog dlg) {
            super();
            putValue(NAME, "Schließen");
            putValue(SHORT_DESCRIPTION, "Dialog schließen");
            this.dlg = dlg;
        }

        public void actionPerformed(ActionEvent e) {
            dlg.dispose();
        }
    }
}
