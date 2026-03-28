/*
 * Copyright (c) 2026 derreisende77.
 * This code was developed as part of the MediathekView project https://github.com/mediathekview/MediathekView
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package mediathek.gui.dialog;

import mediathek.config.BuildInfo;
import mediathek.config.Konstanten;
import mediathek.gui.actions.UrlHyperlinkAction;
import net.miginfocom.layout.AC;
import net.miginfocom.layout.CC;
import net.miginfocom.layout.LC;
import net.miginfocom.swing.MigLayout;
import org.apache.commons.lang3.SystemUtils;
import org.jdesktop.swingx.JXHyperlink;

import javax.swing.*;
import java.awt.*;

/**
 * @author Christian Franzke
 */
public class AboutDialog extends JDialog {

    public AboutDialog(Window owner) {
        super(owner);
        initComponents();

        var buildInfo = BuildInfo.current();
        var versionText = String.format("Version %s (%s)", Konstanten.MVVERSION, SystemUtils.OS_ARCH);
        if (buildInfo.hasGitMetadata()) {
            versionText = String.format("<html>%s<br/>Build: %s</html>", versionText, buildInfo.formatForDisplay());
        }
        lblVersion.setText(versionText);

        hyperlinkHomepage.addActionListener(_ -> UrlHyperlinkAction.openURL(Konstanten.ADRESSE_WEBSITE));
        hyperlinkGuiDonation.addActionListener(_ -> UrlHyperlinkAction.openURL("https://paypal.me/ChristianFranzke"));
        hyperlinkServerDonation.addActionListener(_ -> UrlHyperlinkAction.openURL(Konstanten.ADRESSE_DONATION));
        hyperlinkForum.addActionListener(_ -> UrlHyperlinkAction.openURL(Konstanten.ADRESSE_FORUM));
        hyperlinkOnlineHelp.addActionListener(_ -> UrlHyperlinkAction.openURL(Konstanten.ADRESSE_ONLINE_HELP));
        hyperlinkFaq.addActionListener(_ -> UrlHyperlinkAction.openURL(Konstanten.ADRESSE_ONLINE_FAQ));

        hyperlinkJetBrains.addActionListener(_ -> UrlHyperlinkAction.openURL("https://www.jetbrains.com"));
        hyperlinkEjTechnologies.addActionListener(_ -> UrlHyperlinkAction.openURL("https://www.ej-technologies.com"));


        SwingUtilities.invokeLater(() -> scrollPane1.getVerticalScrollBar().setValue(0));
    }

    private void initComponents() {
        // JFormDesigner - Component initialization - DO NOT MODIFY  //GEN-BEGIN:initComponents
        // Generated using JFormDesigner non-commercial license
        var label1 = new JLabel();
        var panel1 = new JPanel();
        var label2 = new JLabel();
        lblVersion = new JLabel();
        var tabbedPane1 = new JTabbedPane();
        var panel2 = new JPanel();
        var scrollPane3 = new JScrollPane();
        var textPane1 = new JTextPane();
        var panel3 = new JPanel();
        scrollPane1 = new JScrollPane();
        var textPane2 = new JTextPane();
        var panel5 = new JPanel();
        hyperlinkHomepage = new JXHyperlink();
        hyperlinkGuiDonation = new JXHyperlink();
        hyperlinkServerDonation = new JXHyperlink();
        hyperlinkForum = new JXHyperlink();
        hyperlinkOnlineHelp = new JXHyperlink();
        hyperlinkFaq = new JXHyperlink();
        var panel6 = new JPanel();
        var label4 = new JLabel();
        var panel4 = new JPanel();
        hyperlinkJetBrains = new JXHyperlink();
        hyperlinkEjTechnologies = new JXHyperlink();

        //======== this ========
        setTitle("\u00dcber dieses Programm");
        setModal(true);
        setMinimumSize(new Dimension(725, 470));
        setDefaultCloseOperation(WindowConstants.DISPOSE_ON_CLOSE);
        setResizable(false);
        var contentPane = getContentPane();
        contentPane.setLayout(new MigLayout(
            new LC().fill().insets("5").hideMode(3).gridGap("5", "5"),
            // columns
            new AC()
                .size("316").fill().gap()
                .fill(),
            // rows
            new AC()
                .fill().gap()
                .fill().gap()
                .fill()));

        //---- label1 ----
        label1.setIcon(new ImageIcon(getClass().getResource("/mediathek/res/MediathekView.png")));
        contentPane.add(label1, new CC().cell(0, 0));

        //======== panel1 ========
        {
            panel1.setLayout(new MigLayout(
                new LC().insets("0").hideMode(3).gridGap("0", "0"),
                // columns
                new AC()
                    .grow().align("left"),
                // rows
                new AC()
                    .fill().gap()
                    .fill().gap("rel")
                    .grow().fill()));

            //---- label2 ----
            label2.setText("MediathekView");
            label2.setFont(label2.getFont().deriveFont(label2.getFont().getStyle() | Font.BOLD, label2.getFont().getSize() + 35f));
            panel1.add(label2, new CC().cell(0, 0));

            //---- lblVersion ----
            lblVersion.setText("Version");
            panel1.add(lblVersion, new CC().cell(0, 1));

            //======== tabbedPane1 ========
            {
                tabbedPane1.setTabLayoutPolicy(JTabbedPane.SCROLL_TAB_LAYOUT);

                //======== panel2 ========
                {
                    panel2.setPreferredSize(new Dimension(196, 220));
                    panel2.setLayout(new BorderLayout());

                    //======== scrollPane3 ========
                    {
                        scrollPane3.setOpaque(false);
                        scrollPane3.setMinimumSize(new Dimension(196, 162));

                        //---- textPane1 ----
                        textPane1.setText("MediathekView-Client:\nChristian Franzke (derreisende77)\n\nMediathekView-Server:\nNicklas Wiegandt (nicklas2751)\nPeter W. (pidoubleyou)\nSascha Wiegandt (thesasch)\n\nServer-Administration:\nAlexander Finkh\u00e4user (alex1702)");
                        textPane1.setEditable(false);
                        scrollPane3.setViewportView(textPane1);
                    }
                    panel2.add(scrollPane3, BorderLayout.CENTER);
                }
                tabbedPane1.addTab("Aktive Entwickler", panel2);

                //======== panel3 ========
                {
                    panel3.setLayout(new BorderLayout());

                    //======== scrollPane1 ========
                    {

                        //---- textPane2 ----
                        textPane2.setText("Gr\u00fcnder des Programms:\nXaver W. (xaverW)\n\nWeitere Beteiligte:\nsiedlerchr\nstyrol\nzxsd\napoleon\nhostis\npmshell\nclel\nthausherr\nklauswich");
                        textPane2.setEditable(false);
                        scrollPane1.setViewportView(textPane2);
                    }
                    panel3.add(scrollPane1, BorderLayout.CENTER);
                }
                tabbedPane1.addTab("Ehemalige Mitwirkende", panel3);
            }
            panel1.add(tabbedPane1, new CC().cell(0, 2).grow());
        }
        contentPane.add(panel1, new CC().cell(1, 0, 1, 2).grow());

        //======== panel5 ========
        {
            panel5.setLayout(new MigLayout(
                new LC().fillX().insets("5").hideMode(3).gridGap("5", "5"),
                // columns
                new AC()
                    .size("316").fill(),
                // rows
                new AC()
                    .fill().gap()
                    .fill().gap()
                    .fill().gap()
                    .fill().gap()
                    .fill().gap()
                    ));

            //---- hyperlinkHomepage ----
            hyperlinkHomepage.setText("Homepage");
            panel5.add(hyperlinkHomepage, new CC().cell(0, 0));

            //---- hyperlinkGuiDonation ----
            hyperlinkGuiDonation.setText("Spende an den Entwickler des Programms");
            panel5.add(hyperlinkGuiDonation, new CC().cell(0, 1));

            //---- hyperlinkServerDonation ----
            hyperlinkServerDonation.setText("Spende f\u00fcr den Server-Betrieb");
            panel5.add(hyperlinkServerDonation, new CC().cell(0, 2));

            //---- hyperlinkForum ----
            hyperlinkForum.setText("Hilfe-Forum");
            panel5.add(hyperlinkForum, new CC().cell(0, 3));

            //---- hyperlinkOnlineHelp ----
            hyperlinkOnlineHelp.setText("Online-Anleitung");
            panel5.add(hyperlinkOnlineHelp, new CC().cell(0, 4));

            //---- hyperlinkFaq ----
            hyperlinkFaq.setText("Frequently Asked Questions (FAQ)");
            panel5.add(hyperlinkFaq, new CC().cell(0, 5));
        }
        contentPane.add(panel5, new CC().cell(0, 1).growX());

        //======== panel6 ========
        {
            panel6.setLayout(new MigLayout(
                new LC().fill().insets("0 5 5 5").hideMode(3).gridGap("5", "5"),
                // columns
                new AC()
                    .size("316").fill(),
                // rows
                new AC()
                    .align("top")));

            //---- label4 ----
            label4.setText("Die Entwicklung wird unterst\u00fctzt von:");
            label4.setVerticalAlignment(SwingConstants.TOP);
            panel6.add(label4, new CC().cell(0, 0));
        }
        contentPane.add(panel6, new CC().cell(0, 2));

        //======== panel4 ========
        {
            panel4.setLayout(new MigLayout(
                new LC().insets("0").hideMode(3).gridGap("0", "0"),
                // columns
                new AC()
                    .grow().align("left"),
                // rows
                new AC()
                    .fill().gap()
                    .fill()));

            //---- hyperlinkJetBrains ----
            hyperlinkJetBrains.setText("JetBrains IntelliJ");
            hyperlinkJetBrains.setVerticalAlignment(SwingConstants.TOP);
            panel4.add(hyperlinkJetBrains, new CC().cell(0, 0).growX());

            //---- hyperlinkEjTechnologies ----
            hyperlinkEjTechnologies.setText("ej-technologies install4j & JProfiler");
            panel4.add(hyperlinkEjTechnologies, new CC().cell(0, 1).growX());
        }
        contentPane.add(panel4, new CC().cell(1, 2));
        pack();
        setLocationRelativeTo(getOwner());
        // JFormDesigner - End of component initialization  //GEN-END:initComponents
    }

    // JFormDesigner - Variables declaration - DO NOT MODIFY  //GEN-BEGIN:variables
    // Generated using JFormDesigner non-commercial license
    private JLabel lblVersion;
    private JScrollPane scrollPane1;
    private JXHyperlink hyperlinkHomepage;
    private JXHyperlink hyperlinkGuiDonation;
    private JXHyperlink hyperlinkServerDonation;
    private JXHyperlink hyperlinkForum;
    private JXHyperlink hyperlinkOnlineHelp;
    private JXHyperlink hyperlinkFaq;
    private JXHyperlink hyperlinkJetBrains;
    private JXHyperlink hyperlinkEjTechnologies;
    // JFormDesigner - End of variables declaration  //GEN-END:variables
}
