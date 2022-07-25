/*
 * Created by JFormDesigner on Sat Jul 23 12:40:16 CEST 2022
 */

package mediathek.gui.dialog;

import mediathek.config.Konstanten;
import net.miginfocom.layout.AC;
import net.miginfocom.layout.CC;
import net.miginfocom.layout.LC;
import net.miginfocom.swing.MigLayout;
import org.apache.commons.lang3.SystemUtils;
import org.jdesktop.swingx.JXHyperlink;
import org.jetbrains.annotations.NotNull;

import javax.swing.*;
import java.awt.*;
import java.net.URL;

/**
 * @author Christian Franzke
 */
public class AboutDialog extends JDialog {

    public AboutDialog(Window owner) {
        super(owner);
        initComponents();

        lblVersion.setText(String.format("Version %s (%s)", Konstanten.MVVERSION, SystemUtils.OS_ARCH));

        hyperlinkHomepage.addActionListener(l -> browseToUrl(Konstanten.ADRESSE_WEBSITE));
        hyperlinkGuiDonation.addActionListener(l -> browseToUrl("https://paypal.me/ChristianFranzke"));
        hyperlinkServerDonation.addActionListener(l -> browseToUrl(Konstanten.ADRESSE_DONATION));
        hyperlinkForum.addActionListener(l -> browseToUrl(Konstanten.ADRESSE_FORUM));
        hyperlinkOnlineHelp.addActionListener(l -> browseToUrl(Konstanten.ADRESSE_ONLINE_HELP));

        hyperlinkJetBrains.addActionListener(l -> browseToUrl("https://www.jetbrains.com"));
        hyperlinkEjTechnologies.addActionListener(l -> browseToUrl("https://www.ej-technologies.com"));


        SwingUtilities.invokeLater(() -> scrollPane1.getVerticalScrollBar().setValue(0));
    }

    private void showError() {
        JOptionPane.showMessageDialog(this, "Es konnte kein Browser geöffnet werden.",
                Konstanten.PROGRAMMNAME, JOptionPane.ERROR_MESSAGE);
    }

    private void browseToUrl(@NotNull String url) {
        if (Desktop.isDesktopSupported()) {
            var desktop = Desktop.getDesktop();
            if (desktop.isSupported(Desktop.Action.BROWSE)) {
                try {
                    desktop.browse(new URL(url).toURI());
                } catch (Exception e) {
                    throw new RuntimeException(e);
                }
            }
            else {
                showError();
            }
        }
        else {
            showError();
        }
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
        var label4 = new JLabel();
        var panel4 = new JPanel();
        hyperlinkJetBrains = new JXHyperlink();
        hyperlinkEjTechnologies = new JXHyperlink();

        //======== this ========
        setTitle("\u00dcber dieses Programm"); //NON-NLS
        setModal(true);
        setMinimumSize(new Dimension(725, 470));
        setDefaultCloseOperation(WindowConstants.DISPOSE_ON_CLOSE);
        setResizable(false);
        var contentPane = getContentPane();
        contentPane.setLayout(new MigLayout(
            new LC().fill().insets("5").hideMode(3).gridGap("5", "5"), //NON-NLS
            // columns
            new AC()
                .size("316").fill().gap() //NON-NLS
                .fill(),
            // rows
            new AC()
                .fill().gap()
                .fill().gap()
                .fill()));

        //---- label1 ----
        label1.setIcon(new ImageIcon(getClass().getResource("/mediathek/res/MediathekView.png"))); //NON-NLS
        contentPane.add(label1, new CC().cell(0, 0));

        //======== panel1 ========
        {
            panel1.setLayout(new MigLayout(
                new LC().insets("0").hideMode(3).gridGap("0", "0"), //NON-NLS
                // columns
                new AC()
                    .grow().align("left"), //NON-NLS
                // rows
                new AC()
                    .fill().gap()
                    .fill().gap("rel") //NON-NLS
                    .grow().fill()));

            //---- label2 ----
            label2.setText("MediathekView"); //NON-NLS
            label2.setFont(label2.getFont().deriveFont(label2.getFont().getStyle() | Font.BOLD, label2.getFont().getSize() + 35f));
            panel1.add(label2, new CC().cell(0, 0));

            //---- lblVersion ----
            lblVersion.setText("Version"); //NON-NLS
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
                        textPane1.setText("MediathekView-Client:\nChristian Franzke (derreisende77)\n\nMediathekView-Server:\nNicklas Wiegandt (nicklas2751)\nPeter W. (pidoubleyou)\nSascha Wiegandt (thesasch)\n\nServer-Administration:\nAlexander Finkh\u00e4user (alex1702)"); //NON-NLS
                        scrollPane3.setViewportView(textPane1);
                    }
                    panel2.add(scrollPane3, BorderLayout.CENTER);
                }
                tabbedPane1.addTab("aktive Entwickler", panel2); //NON-NLS

                //======== panel3 ========
                {
                    panel3.setLayout(new BorderLayout());

                    //======== scrollPane1 ========
                    {

                        //---- textPane2 ----
                        textPane2.setText("Gr\u00fcnder des Programms:\nXaver W. (xaverW)\n\nWeitere Beteiligte:\nsiedlerchr\nstyrol\nzxsd\napoleon\nhostis\npmshell\nclel\nthausherr\nklauswich"); //NON-NLS
                        scrollPane1.setViewportView(textPane2);
                    }
                    panel3.add(scrollPane1, BorderLayout.CENTER);
                }
                tabbedPane1.addTab("ehemalige Mitwirkende", panel3); //NON-NLS
            }
            panel1.add(tabbedPane1, new CC().cell(0, 2).grow());
        }
        contentPane.add(panel1, new CC().cell(1, 0, 1, 2).grow());

        //======== panel5 ========
        {
            panel5.setLayout(new MigLayout(
                new LC().fillX().insets("5").hideMode(3).gridGap("5", "5"), //NON-NLS
                // columns
                new AC()
                    .size("316").fill(), //NON-NLS
                // rows
                new AC()
                    .fill().gap()
                    .fill().gap()
                    .fill().gap()
                    .fill().gap()
                    .fill()));

            //---- hyperlinkHomepage ----
            hyperlinkHomepage.setText("Homepage"); //NON-NLS
            panel5.add(hyperlinkHomepage, new CC().cell(0, 0));

            //---- hyperlinkGuiDonation ----
            hyperlinkGuiDonation.setText("Spende an den Entwickler des Programms"); //NON-NLS
            panel5.add(hyperlinkGuiDonation, new CC().cell(0, 1));

            //---- hyperlinkServerDonation ----
            hyperlinkServerDonation.setText("Spende f\u00fcr den Server-Betrieb"); //NON-NLS
            panel5.add(hyperlinkServerDonation, new CC().cell(0, 2));

            //---- hyperlinkForum ----
            hyperlinkForum.setText("Hilfe-Forum"); //NON-NLS
            panel5.add(hyperlinkForum, new CC().cell(0, 3));

            //---- hyperlinkOnlineHelp ----
            hyperlinkOnlineHelp.setText("Online-Anleitung"); //NON-NLS
            panel5.add(hyperlinkOnlineHelp, new CC().cell(0, 4));
        }
        contentPane.add(panel5, new CC().cell(0, 1).growX());

        //---- label4 ----
        label4.setText("Die Entwicklung wird unterst\u00fctzt von:"); //NON-NLS
        label4.setVerticalAlignment(SwingConstants.TOP);
        contentPane.add(label4, new CC().cell(0, 2));

        //======== panel4 ========
        {
            panel4.setLayout(new MigLayout(
                new LC().insets("0").hideMode(3).gridGap("0", "0"), //NON-NLS
                // columns
                new AC()
                    .grow().align("left"), //NON-NLS
                // rows
                new AC()
                    .fill().gap()
                    .fill()));

            //---- hyperlinkJetBrains ----
            hyperlinkJetBrains.setText("JetBrains IntelliJ"); //NON-NLS
            hyperlinkJetBrains.setVerticalAlignment(SwingConstants.TOP);
            panel4.add(hyperlinkJetBrains, new CC().cell(0, 0).growX());

            //---- hyperlinkEjTechnologies ----
            hyperlinkEjTechnologies.setText("ej-technologies install4j & JProfiler"); //NON-NLS
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
    private JXHyperlink hyperlinkJetBrains;
    private JXHyperlink hyperlinkEjTechnologies;
    // JFormDesigner - End of variables declaration  //GEN-END:variables
}
