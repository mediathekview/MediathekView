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
package mediathek.gui.dialog;

import java.awt.*;
import java.awt.Desktop.Action;
import java.awt.event.ActionEvent;
import java.net.URI;
import java.net.URISyntaxException;
import javax.swing.*;
import javax.swing.GroupLayout.Alignment;
import javax.swing.LayoutStyle.ComponentPlacement;
import javax.swing.border.EmptyBorder;
import javax.swing.border.TitledBorder;
import mediathek.daten.Daten;
import mediathek.tool.Funktionen;
import mediathek.tool.Konstanten;
import org.jdesktop.swingx.JXHyperlink;
import org.jdesktop.swingx.hyperlink.HyperlinkAction;

@SuppressWarnings("serial")
public class MVAboutDialog extends JDialog {

    private final JPanel contentPanel = new JPanel();
    private final JLabel lblVersion = new JLabel();
    private final JPanel buttonPane = new JPanel();
    private final JLabel lblFilmlistPath = new JLabel();
    private final JLabel lblSettingsFilePath = new JLabel();
    private final Boolean isRunningOnMac;

    private void setupVersionString() {
        String strVersion = "Version ";
        strVersion += Konstanten.VERSION;
        strVersion += " (";
        strVersion += Funktionen.getBuildNr();
        strVersion += ")";

        lblVersion.setText(strVersion);
    }

    private void initialize() {
        try {
            setupVersionString();
            //Programmpfade
            lblSettingsFilePath.setText(Daten.getBasisVerzeichnis(false) + Konstanten.XML_DATEI);
            lblFilmlistPath.setText(Daten.getBasisVerzeichnis(false) + Konstanten.XML_DATEI_FILME);

            //auf dem Mac brauchen wir den Schließen Button nicht..
            if (isRunningOnMac) {
                this.remove(buttonPane);
            }

        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    public MVAboutDialog(JFrame parent, final Boolean isRunningOnMac) {
        super(parent);
        this.isRunningOnMac = isRunningOnMac;

        setResizable(false);
        setModalityType(ModalityType.APPLICATION_MODAL);
        setBounds(100, 100, 748, 342);
        getContentPane().setLayout(new BorderLayout());
        contentPanel.setBackground(Color.WHITE);
        contentPanel.setBorder(new EmptyBorder(5, 5, 5, 5));
        getContentPane().add(contentPanel, BorderLayout.CENTER);

        JLabel lblProgramIcon = new JLabel();
        lblProgramIcon.setIcon(new ImageIcon(MVAboutDialog.class
                .getResource("/mediathek/res/MediathekView.png")));

        JLabel lblProgramName = new JLabel("Mediathek View");
        lblProgramName.setFont(new Font("Lucida Grande", Font.BOLD, 24));

        lblVersion.setForeground(new Color(159, 159, 159));
        lblVersion.setFont(new Font("Lucida Grande", Font.BOLD, 13));

        JXHyperlink hprlnkWebsite = new JXHyperlink();
        hprlnkWebsite.setHorizontalAlignment(SwingConstants.CENTER);
        try {
            hprlnkWebsite.setAction(new WebsiteHyperlinkAction());
        } catch (URISyntaxException e) {
            e.printStackTrace();
        }
        hprlnkWebsite.setText("Website");

        JXHyperlink hprlnkAnleitung = new JXHyperlink();
        hprlnkAnleitung.setHorizontalAlignment(SwingConstants.CENTER);
        try {
            hprlnkAnleitung.setAction(new AnleitungHyperlinkAction());
        } catch (URISyntaxException e1) {
            e1.printStackTrace();
        }
        hprlnkAnleitung.setText("Anleitung");

        JXHyperlink hprlnkForum = new JXHyperlink();
        hprlnkForum.setHorizontalAlignment(SwingConstants.CENTER);
        try {
            hprlnkForum.setAction(new ForumHyperlinkAction());
        } catch (URISyntaxException e) {
            e.printStackTrace();
        }
        hprlnkForum.setText("Forum");

        JPanel pnlProgramPaths = new JPanel();
        TitledBorder border = new TitledBorder("Programmpfade");
        pnlProgramPaths.setBorder(border);
        pnlProgramPaths.setBackground(Color.WHITE);

        GroupLayout gl_contentPanel = new GroupLayout(contentPanel);
        gl_contentPanel.setHorizontalGroup(gl_contentPanel.createParallelGroup(
                Alignment.LEADING).addGroup(
                gl_contentPanel
                .createSequentialGroup()
                .addContainerGap()
                .addComponent(lblProgramIcon)
                .addPreferredGap(ComponentPlacement.RELATED)
                .addGroup(
                gl_contentPanel
                .createParallelGroup(Alignment.LEADING)
                .addComponent(pnlProgramPaths,
                GroupLayout.DEFAULT_SIZE, 464,
                Short.MAX_VALUE)
                .addComponent(lblProgramName)
                .addComponent(lblVersion,
                Alignment.TRAILING,
                GroupLayout.DEFAULT_SIZE, 464,
                Short.MAX_VALUE)
                .addComponent(hprlnkAnleitung,
                GroupLayout.DEFAULT_SIZE, 464,
                Short.MAX_VALUE)
                .addComponent(hprlnkWebsite,
                GroupLayout.DEFAULT_SIZE, 464,
                Short.MAX_VALUE)
                .addComponent(hprlnkForum,
                GroupLayout.DEFAULT_SIZE, 464,
                Short.MAX_VALUE))
                .addContainerGap()));
        gl_contentPanel
                .setVerticalGroup(gl_contentPanel
                .createParallelGroup(Alignment.TRAILING)
                .addGroup(
                gl_contentPanel
                .createSequentialGroup()
                .addContainerGap()
                .addGroup(
                gl_contentPanel
                .createParallelGroup(
                Alignment.LEADING)
                .addGroup(
                gl_contentPanel
                .createSequentialGroup()
                .addComponent(
                lblProgramName)
                .addPreferredGap(
                ComponentPlacement.RELATED)
                .addComponent(
                lblVersion)
                .addGap(18)
                .addComponent(
                hprlnkWebsite,
                GroupLayout.PREFERRED_SIZE,
                GroupLayout.DEFAULT_SIZE,
                GroupLayout.PREFERRED_SIZE)
                .addPreferredGap(
                ComponentPlacement.RELATED)
                .addComponent(
                hprlnkAnleitung,
                GroupLayout.PREFERRED_SIZE,
                GroupLayout.DEFAULT_SIZE,
                GroupLayout.PREFERRED_SIZE)
                .addPreferredGap(
                ComponentPlacement.RELATED)
                .addComponent(
                hprlnkForum,
                GroupLayout.PREFERRED_SIZE,
                GroupLayout.DEFAULT_SIZE,
                GroupLayout.PREFERRED_SIZE)
                .addGap(18)
                .addComponent(
                pnlProgramPaths,
                GroupLayout.PREFERRED_SIZE,
                78,
                GroupLayout.PREFERRED_SIZE))
                .addComponent(
                lblProgramIcon))
                .addContainerGap(124, Short.MAX_VALUE)));

        JLabel lblFilmliste = new JLabel("Filmliste:");
        lblFilmliste.setHorizontalAlignment(SwingConstants.RIGHT);
        lblFilmliste.setForeground(new Color(159, 159, 159));

        //JLabel lblFilmlistPath = new JLabel("f");
        lblFilmlistPath.setForeground(new Color(159, 159, 159));

        JLabel lblEinstellungen = new JLabel("Einstellungen:");
        lblEinstellungen.setForeground(new Color(159, 159, 159));

        //JLabel lblSettingsFilePath = new JLabel("s");
        lblSettingsFilePath.setForeground(new Color(159, 159, 159));
        GroupLayout gl_panel = new GroupLayout(pnlProgramPaths);
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
    }

    private class WebsiteHyperlinkAction extends HyperlinkAction {

        private final static String WEBSITE_URL = "http://zdfmediathk.sourceforge.net";

        public WebsiteHyperlinkAction() throws URISyntaxException {
            super(new URI(WEBSITE_URL), Action.BROWSE);
            putValue(SHORT_DESCRIPTION, WEBSITE_URL);
            putValue(LONG_DESCRIPTION, WEBSITE_URL);
        }
    }

    private class ForumHyperlinkAction extends HyperlinkAction {

        private final static String FORUM_URL = "http://sourceforge.net/apps/phpbb/zdfmediathk/";

        public ForumHyperlinkAction() throws URISyntaxException {
            super(new URI(FORUM_URL), Action.BROWSE);
            putValue(SHORT_DESCRIPTION, FORUM_URL);
            putValue(LONG_DESCRIPTION, FORUM_URL);
        }
    }

    private class AnleitungHyperlinkAction extends HyperlinkAction {

        private final static String ANLEITUNG_URL = "https://sourceforge.net/p/zdfmediathk/wiki/Home/";

        public AnleitungHyperlinkAction() throws URISyntaxException {
            super(new URI(ANLEITUNG_URL), Action.BROWSE);
            putValue(SHORT_DESCRIPTION, ANLEITUNG_URL);
            putValue(LONG_DESCRIPTION, ANLEITUNG_URL);
        }
    }

    private class CloseDialogAction extends AbstractAction {

        private final MVAboutDialog dlg;

        public CloseDialogAction(MVAboutDialog dlg) {
            super();
            putValue(NAME, "Schlie\u00DFen");
            putValue(SHORT_DESCRIPTION, "Dialog schließen");
            this.dlg = dlg;
        }

        public void actionPerformed(ActionEvent e) {
            dlg.dispose();
        }
    }
}
