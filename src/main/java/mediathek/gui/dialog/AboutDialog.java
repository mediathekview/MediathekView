/*
 * Created by JFormDesigner on Sat Jul 23 12:40:16 CEST 2022
 */

package mediathek.gui.dialog;

import mediathek.config.Konstanten;
import mediathek.gui.actions.UrlHyperlinkAction;
import net.miginfocom.layout.AC;
import net.miginfocom.layout.CC;
import net.miginfocom.layout.LC;
import net.miginfocom.swing.MigLayout;
import org.apache.commons.lang3.SystemUtils;
import org.jdesktop.swingx.JXHyperlink;
import org.jetbrains.annotations.NotNull;

import javax.swing.*;
import java.awt.*;
import java.net.URISyntaxException;

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
        hyperlinkFaq.addActionListener(l -> browseToUrl(Konstanten.ADRESSE_ONLINE_FAQ));

        hyperlinkJetBrains.addActionListener(l -> browseToUrl("https://www.jetbrains.com"));
        hyperlinkEjTechnologies.addActionListener(l -> browseToUrl("https://www.ej-technologies.com"));


        SwingUtilities.invokeLater(() -> scrollPane1.getVerticalScrollBar().setValue(0));
    }

    private void showError() {
        JOptionPane.showMessageDialog(this, "Es konnte kein Browser geöffnet werden.",
                Konstanten.PROGRAMMNAME, JOptionPane.ERROR_MESSAGE);
    }

    private void browseToUrl(@NotNull String url) {
        try {
            UrlHyperlinkAction.openURL(url);
        } catch (URISyntaxException e) {
            showError();
        }
    }

    private void initComponents() {
        // JFormDesigner - Component initialization - DO NOT MODIFY  //GEN-BEGIN:initComponents
        // Generated using JFormDesigner Educational license - Markus Jannek
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
          "fill,insets 5,hidemode 3,gap 5 5",
          // columns
          "[316,fill]" +
          "[fill]",
          // rows
          "[fill]" +
          "[fill]" +
          "[fill]"));

        //---- label1 ----
        label1.setIcon(new ImageIcon(getClass().getResource("/mediathek/res/MediathekView.png")));
        contentPane.add(label1, "cell 0 0");

        //======== panel1 ========
        {
          panel1.setLayout(new MigLayout(
            "insets 0,hidemode 3,gap 0 0",
            // columns
            "[grow,left]",
            // rows
            "[fill]" +
            "[fill]rel" +
            "[grow,fill]"));

          //---- label2 ----
          label2.setText("MediathekView");
          label2.setFont(label2.getFont().deriveFont(label2.getFont().getStyle() | Font.BOLD, label2.getFont().getSize() + 35f));
          panel1.add(label2, "cell 0 0");

          //---- lblVersion ----
          lblVersion.setText("Version");
          panel1.add(lblVersion, "cell 0 1");

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
            tabbedPane1.addTab("aktive Entwickler", panel2);

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
            tabbedPane1.addTab("ehemalige Mitwirkende", panel3);
          }
          panel1.add(tabbedPane1, "cell 0 2,grow");
        }
        contentPane.add(panel1, "cell 1 0 1 2,grow");

        //======== panel5 ========
        {
          panel5.setLayout(new MigLayout(
            "fillx,insets 5,hidemode 3,gap 5 5",
            // columns
            "[316,fill]",
            // rows
            "[fill]" +
            "[fill]" +
            "[fill]" +
            "[fill]" +
            "[fill]" +
            "[]"));

          //---- hyperlinkHomepage ----
          hyperlinkHomepage.setText("Homepage");
          panel5.add(hyperlinkHomepage, "cell 0 0");

          //---- hyperlinkGuiDonation ----
          hyperlinkGuiDonation.setText("Spende an den Entwickler des Programms");
          panel5.add(hyperlinkGuiDonation, "cell 0 1");

          //---- hyperlinkServerDonation ----
          hyperlinkServerDonation.setText("Spende f\u00fcr den Server-Betrieb");
          panel5.add(hyperlinkServerDonation, "cell 0 2");

          //---- hyperlinkForum ----
          hyperlinkForum.setText("Hilfe-Forum");
          panel5.add(hyperlinkForum, "cell 0 3");

          //---- hyperlinkOnlineHelp ----
          hyperlinkOnlineHelp.setText("Online-Anleitung");
          panel5.add(hyperlinkOnlineHelp, "cell 0 4");

          //---- hyperlinkFaq ----
          hyperlinkFaq.setText("Frequently Asked Questions (FAQ)");
          panel5.add(hyperlinkFaq, "cell 0 5");
        }
        contentPane.add(panel5, "cell 0 1,growx");

        //======== panel6 ========
        {
          panel6.setLayout(new MigLayout(
            "fill,insets 0 5 5 5,hidemode 3,gap 5 5",
            // columns
            "[316,fill]",
            // rows
            "[top]"));

          //---- label4 ----
          label4.setText("Die Entwicklung wird unterst\u00fctzt von:");
          label4.setVerticalAlignment(SwingConstants.TOP);
          panel6.add(label4, "cell 0 0");
        }
        contentPane.add(panel6, "cell 0 2");

        //======== panel4 ========
        {
          panel4.setLayout(new MigLayout(
            "insets 0,hidemode 3,gap 0 0",
            // columns
            "[grow,left]",
            // rows
            "[fill]" +
            "[fill]"));

          //---- hyperlinkJetBrains ----
          hyperlinkJetBrains.setText("JetBrains IntelliJ");
          hyperlinkJetBrains.setVerticalAlignment(SwingConstants.TOP);
          panel4.add(hyperlinkJetBrains, "cell 0 0,growx");

          //---- hyperlinkEjTechnologies ----
          hyperlinkEjTechnologies.setText("ej-technologies install4j & JProfiler");
          panel4.add(hyperlinkEjTechnologies, "cell 0 1,growx");
        }
        contentPane.add(panel4, "cell 1 2");
        pack();
        setLocationRelativeTo(getOwner());
        // JFormDesigner - End of component initialization  //GEN-END:initComponents
    }

    // JFormDesigner - Variables declaration - DO NOT MODIFY  //GEN-BEGIN:variables
    // Generated using JFormDesigner Educational license - Markus Jannek
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
