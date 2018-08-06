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
import com.jidesoft.utils.SystemInfo;
import mediathek.config.Daten;
import mediathek.config.Konstanten;
import mediathek.gui.HyperlinkButton;
import mediathek.gui.actions.DisposeDialogAction;
import mediathek.gui.actions.UrlHyperlinkAction;
import mediathek.tool.EscapeKeyHandler;

import javax.swing.*;
import javax.swing.GroupLayout.Alignment;
import javax.swing.LayoutStyle.ComponentPlacement;
import javax.swing.border.EmptyBorder;
import javax.swing.border.TitledBorder;
import java.awt.*;
import java.awt.event.ActionEvent;
import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.net.URISyntaxException;
import java.net.URL;
import java.nio.file.Path;

@SuppressWarnings("serial")
public class AboutDialog extends JDialog {

    private final JLabel lblVersion = new JLabel();
    private final JPanel buttonPane = new JPanel();
    private final JLabel lblFilmlistPath = new JLabel();
    private final JLabel lblSettingsFilePath = new JLabel();
    private final JLabel lblJavaVersion = new JLabel();
    private final JLabel lblVmType = new JLabel();
    private MarqueePane marqueePane;

    private void setupVersionString() {
        String strVersion = "Version ";
        strVersion += Konstanten.MVVERSION;

        lblVersion.setText(strVersion);
    }

    /**
     * Read the credits HTML file from resources
     * @return the HTML content as String
     */
    private String loadCredits()
    {
        String content;

        final StringBuilder contentBuilder = new StringBuilder();
        URL url = this.getClass().getResource("/mediathek/res/programm/about/credits.html");
        try (InputStreamReader isr = new InputStreamReader(url.openStream());
             BufferedReader in = new BufferedReader(isr))
        {
            String str;
            while ((str = in.readLine()) != null) {
                contentBuilder.append(str);
            }
        } catch (IOException ignored) {
            ignored.printStackTrace();
        }
        content = contentBuilder.toString();

        return content;
    }

    private void initMarqueePane() {
        final JEditorPane messagePane = new JEditorPane();
        messagePane.setEditable(false);
        messagePane.setFocusable(false);
        messagePane.setContentType("text/html");
        messagePane.setText(loadCredits());

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
            if (SystemInfo.isMacOSX()) {
                this.remove(buttonPane);
            }

        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    public AboutDialog(JFrame parent) {
        super(parent);

        setModal(true);
        setDefaultCloseOperation(JDialog.DISPOSE_ON_CLOSE);

        EscapeKeyHandler.installHandler(this, this::dispose);

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
        lblProgramIcon.setIcon(new ImageIcon(AboutDialog.class.getResource("/mediathek/res/MediathekView.png")));

        JLabel lblProgramName = new JLabel("MediathekView");
        lblProgramName.setFont(new Font("Lucida Grande", Font.BOLD, 24));

        Color greyColor = new Color(159, 159, 159);
        lblVersion.setForeground(greyColor);
        lblVersion.setFont(new Font("Lucida Grande", Font.BOLD, 13));

        HyperlinkButton hprlnkWebsite = new HyperlinkButton();
        hprlnkWebsite.setAction(new HyperlinkAction(parent, Konstanten.ADRESSE_WEBSITE));
        hprlnkWebsite.setText("Website");

        HyperlinkButton hprlnkDonation = new HyperlinkButton();
        hprlnkDonation.setAction(new HyperlinkAction(parent, Konstanten.ADRESSE_DONATION));
        hprlnkDonation.setText("Spende");

        HyperlinkButton hprlnkAnleitung = new HyperlinkButton();
        hprlnkAnleitung.setAction(new HyperlinkAction(parent, Konstanten.ADRESSE_ANLEITUNG));
        hprlnkAnleitung.setText("Anleitung");

        HyperlinkButton hprlnkForum = new HyperlinkButton();
        hprlnkForum.setAction(new HyperlinkAction(parent, Konstanten.ADRESSE_FORUM));
        hprlnkForum.setText("Forum");

        JPanel pnlProgramPaths = new JPanel();
        TitledBorder border = new TitledBorder("Programmpfade");
        pnlProgramPaths.setBorder(border);
        pnlProgramPaths.setBackground(Color.WHITE);

        JPanel pnlJavaInformation = new JPanel();
        border = new TitledBorder("Java Information");
        pnlJavaInformation.setBorder(border);
        pnlJavaInformation.setBackground(Color.WHITE);

        JLabel lblDevelopmentSupportedBy = new JLabel("Development supported by JetBrains IntelliJ");
        lblDevelopmentSupportedBy.setFont(UIManager.getFont("ToolTip.font"));

        GroupLayout gl_contentPanel = new GroupLayout(contentPanel);
        gl_contentPanel.setHorizontalGroup(
                gl_contentPanel.createParallelGroup(Alignment.LEADING)
                        .addGroup(gl_contentPanel.createSequentialGroup()
                                .addContainerGap()
                                .addGroup(gl_contentPanel.createParallelGroup(Alignment.LEADING)
                                        .addGroup(gl_contentPanel.createSequentialGroup()
                                                .addComponent(lblDevelopmentSupportedBy, GroupLayout.DEFAULT_SIZE, GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                                                .addGap(573))
                                        .addGroup(gl_contentPanel.createSequentialGroup()
                                                .addGroup(gl_contentPanel.createParallelGroup(Alignment.LEADING)
                                                        .addComponent(lblProgramIcon)
                                                        .addGroup(gl_contentPanel.createSequentialGroup()
                                                                .addComponent(hprlnkWebsite, GroupLayout.DEFAULT_SIZE, 53, Short.MAX_VALUE)
                                                                .addGap(206))
                                                        .addGroup(gl_contentPanel.createSequentialGroup()
                                                                .addComponent(hprlnkDonation, GroupLayout.DEFAULT_SIZE, GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                                                                .addGap(213))
                                                        .addGroup(gl_contentPanel.createSequentialGroup()
                                                                .addComponent(hprlnkForum, GroupLayout.DEFAULT_SIZE, GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                                                                .addGap(218))
                                                        .addGroup(gl_contentPanel.createSequentialGroup()
                                                                .addComponent(hprlnkAnleitung, GroupLayout.DEFAULT_SIZE, GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                                                                .addGap(197)))
                                                .addGap(0)
                                                .addGroup(gl_contentPanel.createParallelGroup(Alignment.LEADING)
                                                        .addComponent(marqueePane, GroupLayout.DEFAULT_SIZE, 543, Short.MAX_VALUE)
                                                        .addComponent(pnlJavaInformation, Alignment.TRAILING, GroupLayout.DEFAULT_SIZE, 543, Short.MAX_VALUE)
                                                        .addComponent(pnlProgramPaths, GroupLayout.DEFAULT_SIZE, GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                                                        .addComponent(lblProgramName)
                                                        .addComponent(lblVersion, Alignment.TRAILING, GroupLayout.DEFAULT_SIZE, 543, Short.MAX_VALUE))))
                                .addContainerGap())
        );
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
                                .addPreferredGap(ComponentPlacement.RELATED, 14, Short.MAX_VALUE)
                                .addComponent(lblDevelopmentSupportedBy))
        );

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
                okButton.setAction(new DisposeDialogAction(this, "Schlie\u00DFen", "Dialog schlie\u00DFen"));
                buttonPane.add(okButton);
                getRootPane().setDefaultButton(okButton);
            }
        }
        initialize();

        pack();
    }

    /**
     * Opens a browser window
     */
    private class HyperlinkAction extends AbstractAction {
        private final String url;
        private final JFrame parent;

        public HyperlinkAction(JFrame parent, final String url) {
            this.url = url;
            this.parent = parent;
            putValue(SHORT_DESCRIPTION, url);
            putValue(LONG_DESCRIPTION, url);
        }

        @Override
        public void actionPerformed(ActionEvent e) {
            try {
                UrlHyperlinkAction.openURL(parent, url);
            } catch (URISyntaxException ignored) {
            }
        }
    }
}
