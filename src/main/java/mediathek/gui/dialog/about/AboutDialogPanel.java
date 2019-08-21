/*
 * Created by JFormDesigner on Wed Aug 21 17:08:14 CEST 2019
 */

package mediathek.gui.dialog.about;

import com.jidesoft.swing.JideButton;
import mediathek.MediathekGui;
import mediathek.config.Daten;
import mediathek.config.Konstanten;
import mediathek.gui.actions.UrlHyperlinkAction;
import net.miginfocom.layout.AC;
import net.miginfocom.layout.CC;
import net.miginfocom.layout.LC;
import net.miginfocom.swing.MigLayout;

import javax.swing.*;
import javax.swing.border.TitledBorder;
import java.awt.*;
import java.net.URISyntaxException;
import java.nio.file.Path;

/**
 * @author Christian Franzke
 */
public class AboutDialogPanel extends JPanel {
    public AboutDialogPanel() {
        initComponents();

        setupVersionString();
        setupProgramPaths();
        setupHyperlinks();
    }

    private void setupHyperlinks() {
        btnWebsite.addActionListener(l -> {
            try {
                UrlHyperlinkAction.openURL(MediathekGui.ui(), Konstanten.ADRESSE_WEBSITE);
            } catch (URISyntaxException ignored) {
            }
        });

        btnDonation.addActionListener(l -> {
            try {
                UrlHyperlinkAction.openURL(MediathekGui.ui(), Konstanten.ADRESSE_DONATION);
            } catch (URISyntaxException ignored) {
            }
        });

        btnSupportClientDev.addActionListener(l -> {
            try {
                UrlHyperlinkAction.openURL(MediathekGui.ui(), "https://paypal.me/ChristianFranzke?locale.x=de_DE");
            } catch (URISyntaxException ignored) {
            }
        });

        btnForum.addActionListener(l -> {
            try {
                UrlHyperlinkAction.openURL(MediathekGui.ui(), Konstanten.ADRESSE_FORUM);
            } catch (URISyntaxException ignored) {
            }
        });

        btnInstructions.addActionListener(l -> {
            try {
                UrlHyperlinkAction.openURL(MediathekGui.ui(), Konstanten.ADRESSE_ANLEITUNG);
            } catch (URISyntaxException ignored) {
            }
        });

        btnJetbrainsLink.addActionListener(l -> {
            try {
                UrlHyperlinkAction.openURL(MediathekGui.ui(), "https://www.jetbrains.com");
            } catch (URISyntaxException ignored) {
            }
        });

        btnEjTechLink.addActionListener(l -> {
            try {
                UrlHyperlinkAction.openURL(MediathekGui.ui(), "https://www.ej-technologies.com");
            } catch (URISyntaxException ignored) {
            }
        });
    }

    private void setupProgramPaths() {
        // Programmpfade
        final Path xmlFilePath = Daten.getMediathekXmlFilePath();
        lblSettingsDirectory.setText(xmlFilePath.toAbsolutePath().toString());
        lblFilmListPath.setText(Daten.getDateiFilmliste());
    }

    private void setupVersionString() {
        String strVersion = "Version ";
        strVersion += Konstanten.MVVERSION;

        lblVersion.setText(strVersion);
    }

    private void initComponents() {
        // JFormDesigner - Component initialization - DO NOT MODIFY  //GEN-BEGIN:initComponents
        // Generated using JFormDesigner non-commercial license
        var lblIcon = new JLabel();
        var panel3 = new JPanel();
        var lblAppName = new JLabel();
        lblVersion = new JLabel();
        var tabbedPane1 = new JTabbedPane();
        var panel6 = new JPanel();
        var scrollPane1 = new JScrollPane();
        var editorPane1 = new JEditorPane();
        var panel7 = new JPanel();
        var scrollPane2 = new JScrollPane();
        var editorPane2 = new JEditorPane();
        var pnlProgramPath = new JPanel();
        var label13 = new JLabel();
        lblFilmListPath = new JLabel();
        var label14 = new JLabel();
        lblSettingsDirectory = new JLabel();
        var pnlHyperlinks = new JPanel();
        btnWebsite = new JideButton();
        btnDonation = new JideButton();
        btnSupportClientDev = new JideButton();
        btnForum = new JideButton();
        btnInstructions = new JideButton();
        var panel2 = new JPanel();
        var label7 = new JLabel();
        btnJetbrainsLink = new JideButton();
        btnEjTechLink = new JideButton();

        //======== this ========
        setLayout(new MigLayout(
            new LC().insets("5").hideMode(3), //NON-NLS
            // columns
            new AC()
                .fill().gap()
                .grow().fill(),
            // rows
            new AC()
                .gap()
                .gap()
                ));

        //---- lblIcon ----
        lblIcon.setIcon(new ImageIcon(getClass().getResource("/mediathek/res/MediathekView.png"))); //NON-NLS
        add(lblIcon, new CC().cell(0, 0));

        //======== panel3 ========
        {
            panel3.setLayout(new MigLayout(
                new LC().hideMode(3),
                // columns
                new AC()
                    .grow().fill(),
                // rows
                new AC()
                    .gap()
                    .gap()
                    .gap()
                    .gap()
                    ));

            //---- lblAppName ----
            lblAppName.setText("MediathekView"); //NON-NLS
            lblAppName.setFont(lblAppName.getFont().deriveFont(lblAppName.getFont().getStyle() | Font.BOLD, 24f));
            panel3.add(lblAppName, new CC().cell(0, 0));

            //---- lblVersion ----
            lblVersion.setText("Version 13.4"); //NON-NLS
            lblVersion.setFont(lblVersion.getFont().deriveFont(lblVersion.getFont().getStyle() | Font.BOLD));
            lblVersion.setForeground(Color.gray);
            panel3.add(lblVersion, new CC().cell(0, 1));

            //======== tabbedPane1 ========
            {

                //======== panel6 ========
                {
                    panel6.setMinimumSize(new Dimension(23, 250));
                    panel6.setPreferredSize(new Dimension(147, 300));
                    panel6.setLayout(new BorderLayout());

                    //======== scrollPane1 ========
                    {

                        //---- editorPane1 ----
                        editorPane1.setEditable(false);
                        editorPane1.setText("MediathekView-Client:\nChristian Franzke (derreisende77)\n\nMediathekView-Server:\nNicklas Wiegandt (nicklas2751)\nPeter W. (pidoubleyou)\nSascha Wiegandt (thesasch)\n\nServer-Administration:\nAlexander Finkh\u00e4user (alex1702)"); //NON-NLS
                        editorPane1.setCaretPosition(1);
                        scrollPane1.setViewportView(editorPane1);
                    }
                    panel6.add(scrollPane1, BorderLayout.CENTER);
                }
                tabbedPane1.addTab("Aktive Entwickler", panel6); //NON-NLS

                //======== panel7 ========
                {
                    panel7.setMinimumSize(new Dimension(23, 250));
                    panel7.setPreferredSize(new Dimension(490, 300));
                    panel7.setLayout(new BorderLayout());

                    //======== scrollPane2 ========
                    {

                        //---- editorPane2 ----
                        editorPane2.setEditable(false);
                        editorPane2.setText("Gr\u00fcnder des Programms:\nXaver W. (xaverW)\n\nWeitere Beteiligte:\nsiedlerchr\nstyrol\nzxsd\nsiedlerchr\napoleon\nhostis\npmshell\nclel\nthausherr"); //NON-NLS
                        editorPane2.setCaretPosition(1);
                        scrollPane2.setViewportView(editorPane2);
                    }
                    panel7.add(scrollPane2, BorderLayout.CENTER);
                }
                tabbedPane1.addTab("Ehemalige", panel7); //NON-NLS
            }
            panel3.add(tabbedPane1, new CC().cell(0, 2).width("250:550").height("200:200:200")); //NON-NLS

            //======== pnlProgramPath ========
            {
                pnlProgramPath.setBorder(new TitledBorder("Programmpfade")); //NON-NLS
                pnlProgramPath.setLayout(new MigLayout(
                    new LC().hideMode(3),
                    // columns
                    new AC()
                        .fill().gap()
                        .grow().fill(),
                    // rows
                    new AC()
                        .gap()
                        ));

                //---- label13 ----
                label13.setText("Filmliste:"); //NON-NLS
                label13.setHorizontalAlignment(SwingConstants.RIGHT);
                label13.setForeground(Color.gray);
                pnlProgramPath.add(label13, new CC().cell(0, 0));

                //---- lblFilmListPath ----
                lblFilmListPath.setText("path to filmlist"); //NON-NLS
                lblFilmListPath.setForeground(Color.gray);
                pnlProgramPath.add(lblFilmListPath, new CC().cell(1, 0).growX().width("300:500")); //NON-NLS

                //---- label14 ----
                label14.setText("Einstellungen:"); //NON-NLS
                label14.setHorizontalAlignment(SwingConstants.RIGHT);
                label14.setForeground(Color.gray);
                pnlProgramPath.add(label14, new CC().cell(0, 1));

                //---- lblSettingsDirectory ----
                lblSettingsDirectory.setText("path to settings directory"); //NON-NLS
                lblSettingsDirectory.setForeground(Color.gray);
                pnlProgramPath.add(lblSettingsDirectory, new CC().cell(1, 1).growX());
            }
            panel3.add(pnlProgramPath, new CC().cell(0, 4).growX());
        }
        add(panel3, new CC().cell(1, 0, 1, 2).alignY("top").growY(0)); //NON-NLS

        //======== pnlHyperlinks ========
        {
            pnlHyperlinks.setLayout(new MigLayout(
                new LC().hideMode(3),
                // columns
                new AC()
                    .fill(),
                // rows
                new AC()
                    .gap()
                    .gap()
                    .gap()
                    .gap()
                    ));

            //---- btnWebsite ----
            btnWebsite.setText("Website"); //NON-NLS
            btnWebsite.setHorizontalAlignment(SwingConstants.LEFT);
            btnWebsite.setButtonStyle(3);
            btnWebsite.setForeground(new Color(88, 157, 246));
            pnlHyperlinks.add(btnWebsite, new CC().cell(0, 0).alignX("left").growX(0)); //NON-NLS

            //---- btnDonation ----
            btnDonation.setText("Spende f\u00fcr den Betrieb"); //NON-NLS
            btnDonation.setButtonStyle(3);
            btnDonation.setForeground(new Color(88, 157, 246));
            pnlHyperlinks.add(btnDonation, new CC().cell(0, 1).alignX("left").growX(0)); //NON-NLS

            //---- btnSupportClientDev ----
            btnSupportClientDev.setText("Spende an den Client-Entwickler"); //NON-NLS
            btnSupportClientDev.setButtonStyle(3);
            btnSupportClientDev.setForeground(new Color(88, 157, 246));
            pnlHyperlinks.add(btnSupportClientDev, new CC().cell(0, 2).alignX("left").growX(0)); //NON-NLS

            //---- btnForum ----
            btnForum.setText("Forum"); //NON-NLS
            btnForum.setButtonStyle(3);
            btnForum.setForeground(new Color(88, 157, 246));
            pnlHyperlinks.add(btnForum, new CC().cell(0, 3).alignX("left").growX(0)); //NON-NLS

            //---- btnInstructions ----
            btnInstructions.setText("Anleitung"); //NON-NLS
            btnInstructions.setButtonStyle(3);
            btnInstructions.setForeground(new Color(88, 157, 246));
            pnlHyperlinks.add(btnInstructions, new CC().cell(0, 4).alignX("left").growX(0)); //NON-NLS
        }
        add(pnlHyperlinks, new CC().cell(0, 1).alignY("top").growY(0)); //NON-NLS

        //======== panel2 ========
        {
            panel2.setLayout(new MigLayout(
                new LC().hideMode(3),
                // columns
                new AC()
                    .fill().gap()
                    .grow().fill(),
                // rows
                new AC()
                    .gap()
                    ));

            //---- label7 ----
            label7.setText("Development supported by:"); //NON-NLS
            label7.setFont(label7.getFont().deriveFont(label7.getFont().getSize() - 2f));
            panel2.add(label7, new CC().cell(0, 0));

            //---- btnJetbrainsLink ----
            btnJetbrainsLink.setText("JetBrains IntelliJ"); //NON-NLS
            btnJetbrainsLink.setButtonStyle(3);
            btnJetbrainsLink.setFont(btnJetbrainsLink.getFont().deriveFont(btnJetbrainsLink.getFont().getSize() - 2f));
            btnJetbrainsLink.setForeground(new Color(88, 157, 246));
            panel2.add(btnJetbrainsLink, new CC().cell(1, 0).alignX("left").growX(0)); //NON-NLS

            //---- btnEjTechLink ----
            btnEjTechLink.setText("ej-technologies JProfiler and install4j"); //NON-NLS
            btnEjTechLink.setButtonStyle(3);
            btnEjTechLink.setFont(btnEjTechLink.getFont().deriveFont(btnEjTechLink.getFont().getSize() - 2f));
            btnEjTechLink.setForeground(new Color(88, 157, 246));
            panel2.add(btnEjTechLink, new CC().cell(1, 1).alignX("left").growX(0)); //NON-NLS
        }
        add(panel2, new CC().cell(0, 2, 2, 1));
        // JFormDesigner - End of component initialization  //GEN-END:initComponents
    }

    // JFormDesigner - Variables declaration - DO NOT MODIFY  //GEN-BEGIN:variables
    // Generated using JFormDesigner non-commercial license
    private JLabel lblVersion;
    private JLabel lblFilmListPath;
    private JLabel lblSettingsDirectory;
    private JideButton btnWebsite;
    private JideButton btnDonation;
    private JideButton btnSupportClientDev;
    private JideButton btnForum;
    private JideButton btnInstructions;
    private JideButton btnJetbrainsLink;
    private JideButton btnEjTechLink;
    // JFormDesigner - End of variables declaration  //GEN-END:variables
}
