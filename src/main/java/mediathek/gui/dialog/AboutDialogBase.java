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

import java.util.*;
import net.miginfocom.layout.AC;
import net.miginfocom.layout.CC;
import net.miginfocom.layout.LC;
import net.miginfocom.swing.MigLayout;
import org.jdesktop.swingx.JXHyperlink;

import javax.swing.*;
import java.awt.*;

/**
 * @author Christian Franzke
 */
public class AboutDialogBase extends JDialog {

    public AboutDialogBase(Window owner) {
        super(owner);
        initComponents();
    }

    private void initComponents() {
        // JFormDesigner - Component initialization - DO NOT MODIFY  //GEN-BEGIN:initComponents
        ResourceBundle bundle = mediathek.i18n.UiMessages.getBundle("mediathek.i18n.Messages");
        var bundle = mediathek.i18n.UiMessages.getBundle("mediathek.i18n.Messages");
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
        setTitle(bundle.getString("dialog.AboutDialog.this.title"));
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
            label2.setText(bundle.getString("dialog.AboutDialog.label2.text"));
            label2.setFont(label2.getFont().deriveFont(label2.getFont().getStyle() | Font.BOLD, label2.getFont().getSize() + 35f));
            panel1.add(label2, "cell 0 0");

            //---- lblVersion ----
            lblVersion.setText(bundle.getString("dialog.AboutDialog.lblVersion.text"));
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
                        textPane1.setText(bundle.getString("dialog.AboutDialog.textPane1.text"));
                        textPane1.setEditable(false);
                        scrollPane3.setViewportView(textPane1);
                    }
                    panel2.add(scrollPane3, BorderLayout.CENTER);
                }
                tabbedPane1.addTab(bundle.getString("dialog.AboutDialog.textPane1.title"), panel2);

                //======== panel3 ========
                {
                    panel3.setLayout(new BorderLayout());

                    //======== scrollPane1 ========
                    {

                        //---- textPane2 ----
                        textPane2.setText(bundle.getString("dialog.AboutDialog.textPane2.text"));
                        textPane2.setEditable(false);
                        scrollPane1.setViewportView(textPane2);
                    }
                    panel3.add(scrollPane1, BorderLayout.CENTER);
                }
                tabbedPane1.addTab(bundle.getString("dialog.AboutDialog.textPane2.title"), panel3);
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
            hyperlinkHomepage.setText(bundle.getString("dialog.AboutDialog.hyperlinkHomepage.text"));
            panel5.add(hyperlinkHomepage, "cell 0 0");

            //---- hyperlinkGuiDonation ----
            hyperlinkGuiDonation.setText(bundle.getString("dialog.AboutDialog.hyperlinkGuiDonation.text"));
            panel5.add(hyperlinkGuiDonation, "cell 0 1");

            //---- hyperlinkServerDonation ----
            hyperlinkServerDonation.setText(bundle.getString("dialog.AboutDialog.hyperlinkServerDonation.text"));
            panel5.add(hyperlinkServerDonation, "cell 0 2");

            //---- hyperlinkForum ----
            hyperlinkForum.setText(bundle.getString("dialog.AboutDialog.hyperlinkForum.text"));
            panel5.add(hyperlinkForum, "cell 0 3");

            //---- hyperlinkOnlineHelp ----
            hyperlinkOnlineHelp.setText(bundle.getString("dialog.AboutDialog.hyperlinkOnlineHelp.text"));
            panel5.add(hyperlinkOnlineHelp, "cell 0 4");

            //---- hyperlinkFaq ----
            hyperlinkFaq.setText(bundle.getString("dialog.AboutDialog.hyperlinkFaq.text"));
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
            label4.setText(bundle.getString("dialog.AboutDialog.label4.text"));
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
            hyperlinkJetBrains.setText(bundle.getString("dialog.AboutDialog.hyperlinkJetBrains.text"));
            hyperlinkJetBrains.setVerticalAlignment(SwingConstants.TOP);
            panel4.add(hyperlinkJetBrains, "cell 0 0,growx");

            //---- hyperlinkEjTechnologies ----
            hyperlinkEjTechnologies.setText(bundle.getString("dialog.AboutDialog.hyperlinkEjTechnologies.text"));
            panel4.add(hyperlinkEjTechnologies, "cell 0 1,growx");
        }
        contentPane.add(panel4, "cell 1 2");
        pack();
        setLocationRelativeTo(getOwner());
        // JFormDesigner - End of component initialization  //GEN-END:initComponents
    }

    // JFormDesigner - Variables declaration - DO NOT MODIFY  //GEN-BEGIN:variables
    protected JLabel lblVersion;
    protected JScrollPane scrollPane1;
    protected JXHyperlink hyperlinkHomepage;
    protected JXHyperlink hyperlinkGuiDonation;
    protected JXHyperlink hyperlinkServerDonation;
    protected JXHyperlink hyperlinkForum;
    protected JXHyperlink hyperlinkOnlineHelp;
    protected JXHyperlink hyperlinkFaq;
    protected JXHyperlink hyperlinkJetBrains;
    protected JXHyperlink hyperlinkEjTechnologies;
    // JFormDesigner - End of variables declaration  //GEN-END:variables
}
