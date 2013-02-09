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

import com.jidesoft.swing.MarqueePane;
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

    private final JLabel lblVersion = new JLabel();
    private final JPanel buttonPane = new JPanel();
    private final JLabel lblFilmlistPath = new JLabel();
    private final JLabel lblSettingsFilePath = new JLabel();
    private final Boolean isRunningOnMac;
    private final JLabel lblJavaVersion = new JLabel();
    private final JLabel lblVmType = new JLabel();
    private MarqueePane marqueePane;

    private void setupVersionString() {
        String strVersion = "Version ";
        strVersion += Konstanten.VERSION;
        strVersion += " (";
        strVersion += Funktionen.getBuildNr();
        strVersion += ")";

        lblVersion.setText(strVersion);
    }

    private void initMarqueePane() {
        final JEditorPane messagePane = new JEditorPane();
        messagePane.setEditable(false);
        messagePane.setFocusable(false);
        messagePane.setContentType("text/html");
        //TODO Message austauschen!
        //Hier kann eine HTML-Datei geladen werden
        messagePane.setText("<html><b>Lorem ipsum</b> dolor sit amet, consetetur sadipscing elitr, sed diam nonumy eirmod tempor invidunt ut labore et dolore magna aliquyam erat, sed diam voluptua. At vero eos et accusam et justo duo dolores et ea rebum. Stet clita kasd gubergren, no sea takimata sanctus est Lorem ipsum dolor sit amet. Lorem ipsum dolor sit amet, consetetur sadipscing elitr, sed diam nonumy eirmod tempor invidunt ut labore et dolore magna aliquyam erat, sed diam voluptua. At vero eos et accusam et justo duo dolores et ea rebum. Stet clita kasd gubergren, no sea takimata sanctus est Lorem ipsum dolor sit amet. Lorem ipsum dolor sit amet, consetetur sadipscing elitr, sed diam nonumy eirmod tempor invidunt ut labore et dolore magna aliquyam erat, sed diam voluptua. At vero eos et accusam et justo duo dolores et ea rebum. Stet clita kasd gubergren, no sea takimata sanctus est Lorem ipsum dolor sit amet.   \n"
                + "\n"
                + "Duis autem vel eum iriure dolor in hendrerit in vulputate velit esse molestie consequat, vel illum dolore eu feugiat nulla facilisis at vero eros et accumsan et iusto odio dignissim qui blandit praesent luptatum zzril delenit augue duis dolore te feugait nulla facilisi. Lorem ipsum dolor sit amet, consectetuer adipiscing elit, sed diam nonummy nibh euismod tincidunt ut laoreet dolore magna aliquam erat volutpat.   \n"
                + "\n"
                + "Ut wisi enim ad minim veniam, quis nostrud exerci tation ullamcorper suscipit lobortis nisl ut aliquip ex ea commodo consequat. Duis autem vel eum iriure dolor in hendrerit in vulputate velit esse molestie consequat, vel illum dolore eu feugiat nulla facilisis at vero eros et accumsan et iusto odio dignissim qui blandit praesent luptatum zzril delenit augue duis dolore te feugait nulla facilisi.   \n"
                + "\n"
                + "Nam liber tempor cum soluta nobis eleifend option congue nihil imperdiet doming id quod mazim placerat facer possim assum. Lorem ipsum dolor sit amet, consectetuer adipiscing elit, sed diam nonummy nibh euismod tincidunt ut laoreet dolore magna aliquam erat volutpat. Ut wisi enim ad minim veniam, quis nostrud exerci tation ullamcorper suscipit lobortis nisl ut aliquip ex ea commodo consequat.   \n"
                + "\n"
                + "Duis autem vel eum iriure dolor in hendrerit in vulputate velit esse molestie consequat, vel illum dolore eu feugiat nulla facilisis.   \n"
                + "\n"
                + "At vero eos et accusam et justo duo dolores et ea rebum. Stet clita kasd gubergren, no sea takimata sanctus est Lorem ipsum dolor sit amet. Lorem ipsum dolor sit amet, consetetur sadipscing elitr, sed diam nonumy eirmod tempor invidunt ut labore et dolore magna aliquyam erat, sed diam voluptua. At vero eos et accusam et justo duo dolores et ea rebum. Stet clita kasd gubergren, no sea takimata sanctus est Lorem ipsum dolor sit amet. Lorem ipsum dolor sit amet, consetetur sadipscing elitr, At accusam aliquyam diam diam dolore dolores duo eirmod eos erat, et nonumy sed tempor et et invidunt justo labore Stet clita ea et gubergren, kasd magna no rebum. sanctus sea sed takimata ut vero voluptua. est Lorem ipsum dolor sit amet. Lorem ipsum dolor sit amet, consetetur</html>");

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

        initMarqueePane();

        setResizable(false);
        setModalityType(ModalityType.APPLICATION_MODAL);
        setBounds(100, 100, 748, 476);
        getContentPane().setLayout(new BorderLayout());
        JPanel contentPanel = new JPanel();
        contentPanel.setBackground(Color.WHITE);
        contentPanel.setBorder(new EmptyBorder(5, 5, 5, 5));
        getContentPane().add(contentPanel, BorderLayout.CENTER);

        JLabel lblProgramIcon = new JLabel();
        lblProgramIcon.setIcon(new ImageIcon(MVAboutDialog.class
                .getResource("/mediathek/res/MediathekView.png")));

        JLabel lblProgramName = new JLabel("Mediathek View");
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

        JXHyperlink hprlnkAnleitung = new JXHyperlink();
        hprlnkAnleitung.setHorizontalAlignment(SwingConstants.LEFT);
        try {
            hprlnkAnleitung.setAction(new AnleitungHyperlinkAction());
        } catch (URISyntaxException e1) {
            e1.printStackTrace();
        }
        hprlnkAnleitung.setText("Anleitung");

        JXHyperlink hprlnkForum = new JXHyperlink();
        hprlnkForum.setHorizontalAlignment(SwingConstants.LEFT);
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

        JPanel pnlJavaInformation = new JPanel();
        border = new TitledBorder("Java Information");
        pnlJavaInformation.setBorder(border);
        pnlJavaInformation.setBackground(Color.WHITE);

        GroupLayout gl_contentPanel = new GroupLayout(contentPanel);


        gl_contentPanel.setHorizontalGroup(
                gl_contentPanel.createParallelGroup(Alignment.LEADING)
                .addGroup(gl_contentPanel.createSequentialGroup()
                .addContainerGap()
                .addGroup(gl_contentPanel.createParallelGroup(Alignment.LEADING)
                .addComponent(lblProgramIcon)
                .addComponent(hprlnkWebsite, GroupLayout.PREFERRED_SIZE, GroupLayout.DEFAULT_SIZE, GroupLayout.PREFERRED_SIZE)
                .addComponent(hprlnkForum, GroupLayout.PREFERRED_SIZE, GroupLayout.DEFAULT_SIZE, GroupLayout.PREFERRED_SIZE)
                .addComponent(hprlnkAnleitung, GroupLayout.PREFERRED_SIZE, GroupLayout.DEFAULT_SIZE, GroupLayout.PREFERRED_SIZE))
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
            putValue(NAME, "Schließen");
            putValue(SHORT_DESCRIPTION, "Dialog schließen");
            this.dlg = dlg;
        }

        public void actionPerformed(ActionEvent e) {
            dlg.dispose();
        }
    }
}
