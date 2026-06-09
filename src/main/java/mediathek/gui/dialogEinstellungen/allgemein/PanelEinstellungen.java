package mediathek.gui.dialogEinstellungen.allgemein;

import mediathek.config.application.ApplicationConfiguration;
import mediathek.gui.messages.*;
import mediathek.mainwindow.MediathekGui;
import mediathek.tool.GuiFunktionen;
import mediathek.tool.MessageBus;
import mediathek.tool.http.MVHttpClient;
import mediathek.x11.DesktopEnvDetector;
import net.engio.mbassy.listener.Handler;
import net.miginfocom.layout.AC;
import net.miginfocom.layout.CC;
import net.miginfocom.layout.LC;
import net.miginfocom.swing.MigLayout;
import org.apache.commons.lang3.SystemUtils;
import org.jdesktop.swingx.JXTitledPanel;

import javax.swing.*;
import javax.swing.border.TitledBorder;
import java.awt.*;
import java.awt.event.ActionEvent;

public class PanelEinstellungen extends JPanel {
    private void setupProxySettings() {
        var applicationConfiguration = ApplicationConfiguration.getInstance();

        jtfProxyHost.setText(applicationConfiguration.getHttpProxyHost());
        var listener = new TextFieldConfigWriter(jtfProxyHost, applicationConfiguration::setHttpProxyHost);
        jtfProxyHost.getDocument().addDocumentListener(new TimedDocumentListener(listener));

        jtfProxyPort.setText(applicationConfiguration.getHttpProxyPort());
        listener = new TextFieldConfigWriter(jtfProxyPort, applicationConfiguration::setHttpProxyPort);
        jtfProxyPort.getDocument().addDocumentListener(new TimedDocumentListener(listener));

        jtfProxyUser.setText(applicationConfiguration.getHttpProxyUser());
        listener = new TextFieldConfigWriter(jtfProxyUser, applicationConfiguration::setHttpProxyUser);
        jtfProxyUser.getDocument().addDocumentListener(new TimedDocumentListener(listener));

        jpfProxyPassword.setText(applicationConfiguration.getHttpProxyPassword());
        listener = new TextFieldConfigWriter(jpfProxyPassword, applicationConfiguration::setHttpProxyPassword);
        jpfProxyPassword.getDocument().addDocumentListener(new TimedDocumentListener(listener));

        jButtonApplyProxySettings.addActionListener(_ -> applyProxySettings());
    }

    private void applyProxySettings() {
        ApplicationConfiguration.getInstance().setHttpProxy(
                jtfProxyHost.getText(),
                jtfProxyPort.getText(),
                jtfProxyUser.getText(),
                new String(jpfProxyPassword.getPassword()));
        MVHttpClient.INSTANCE.reloadProxySettings();
    }

    private void setupUserAgentSettings() {
        var applicationConfiguration = ApplicationConfiguration.getInstance();
        jtfUserAgent.setText(applicationConfiguration.getUserAgent());
        var listener = new TextFieldConfigWriter(jtfUserAgent, applicationConfiguration::setUserAgent);
        jtfUserAgent.getDocument().addDocumentListener(new TimedDocumentListener(listener));
    }

    private void cbUseWikipediaSenderLogosActionPerformed(ActionEvent evt) {
        ApplicationConfiguration.getInstance().setLocalSenderIcons(!cbUseWikipediaSenderLogos.isSelected());
        MessageBus.getMessageBus().publish(new SenderIconStyleChangedEvent());
        MediathekGui.ui().repaint();
    }
    
    private void cbAutomaticUpdateChecksActionPerformed(ActionEvent evt) {
        ApplicationConfiguration.getInstance().setAutomaticUpdateCheck(cbAutomaticUpdateChecks.isSelected());
        MessageBus.getMessageBus().publishAsync(new UpdateStateChangedEvent(cbAutomaticUpdateChecks.isSelected()));
    }

    private void setupTabUI() {
        var applicationConfiguration = ApplicationConfiguration.getInstance();
        final boolean tabPositionTop = applicationConfiguration.getTabPositionTop();
        jCheckBoxTabsTop.setSelected(tabPositionTop);
        jCheckBoxTabsTop.addActionListener(_ -> {
            applicationConfiguration.setTabPositionTop(jCheckBoxTabsTop.isSelected());
            MessageBus.getMessageBus().publishAsync(new TabVisualSettingsChangedEvent());
        });
        if (SystemUtils.IS_OS_MAC_OSX) {
            jCheckBoxTabsTop.setEnabled(false);
            jCheckBoxTabsTop.setToolTipText(NO_INFLUENCE_TEXT);
        }

        jCheckBoxTabIcon.setSelected(applicationConfiguration.getMainWindowTabIcons());
        jCheckBoxTabIcon.addActionListener(_ -> {
            applicationConfiguration.setMainWindowTabIcons(jCheckBoxTabIcon.isSelected());
            MessageBus.getMessageBus().publishAsync(new TabVisualSettingsChangedEvent());
        });
    }

    @Handler
    private void handleTrayIconEvent(TrayIconEvent e) {
        SwingUtilities.invokeLater(() -> jCheckBoxTray.setSelected(ApplicationConfiguration.getInstance().getUseTray()));
    }

    private void setupTray() {
        if (SystemUtils.IS_OS_MAC_OSX || !DesktopEnvDetector.trayIconSupported()) {
            jCheckBoxTray.setSelected(false);
            jCheckBoxTray.setEnabled(false);
        } else {
            MessageBus.getMessageBus().subscribe(this);

            jCheckBoxTray.setSelected(ApplicationConfiguration.getInstance().getUseTray());
            jCheckBoxTray.addActionListener(_ -> {
                ApplicationConfiguration.getInstance().setUseTray(jCheckBoxTray.isSelected());
                MediathekGui.ui().initializeSystemTray();
            });
        }
    }

    private void setupModernSearch() {
        var applicationConfiguration = ApplicationConfiguration.getInstance();
        var useModernSearch = applicationConfiguration.getUseModernSearch();

        var searchPanel = new ModernSearchConfigPanel();
        searchPanel.getCbActivateModernSearch().setSelected(useModernSearch);
        searchPanel.getCbActivateModernSearch().addActionListener(_ -> {
            var selected = searchPanel.getCbActivateModernSearch().isSelected();
            applicationConfiguration.setUseModernSearch(selected);
        });
        modernSearchTitlePanel.setContentContainer(searchPanel);
    }

    public PanelEinstellungen() {
        super();
        initComponents();

        setupModernSearch();
        setupUserAgentSettings();

        setupProxySettings();

        setupTabUI();

        setupTray();

        setupTabSwitchListener();

        cbUseWikipediaSenderLogos.addActionListener(this::cbUseWikipediaSenderLogosActionPerformed);
        final boolean useLocalSenderLogos = ApplicationConfiguration.getInstance().getLocalSenderIcons();
        cbUseWikipediaSenderLogos.setSelected(!useLocalSenderLogos);
        
        cbAutomaticUpdateChecks.addActionListener(this::cbAutomaticUpdateChecksActionPerformed);
        cbAutomaticUpdateChecks.setSelected(ApplicationConfiguration.getInstance().getAutomaticUpdateCheck());
        if (GuiFunktionen.isUsingExternalUpdater()) {
            cbAutomaticUpdateChecks.setEnabled(false);
            cbAutomaticUpdateChecks.setToolTipText("Diese Option ist deaktiviert, da ein externer Updater verwendet wird.");
        }

        var restore = ApplicationConfiguration.getInstance().getRestoreSelectedTab();
        cbRestoreSelectedTab.setSelected(restore);
        cbRestoreSelectedTab.addActionListener(_ -> ApplicationConfiguration.getInstance()
                .setRestoreSelectedTab(cbRestoreSelectedTab.isSelected()));

        var drawIconsRight = ApplicationConfiguration.getInstance().getListIconPositionRight();
        cbDrawListIconsRight.setSelected(drawIconsRight);
        cbDrawListIconsRight.addActionListener(_ -> {
            ApplicationConfiguration.getInstance().setListIconPositionRight(cbDrawListIconsRight.isSelected());
            MediathekGui.ui().repaint();
        });

        boolean useIconWithText = ApplicationConfiguration.getInstance().getToolbarBlacklistIconWithText();
        cbShowBlacklistIconWithText.setSelected(useIconWithText);
        cbShowBlacklistIconWithText.addActionListener(_ -> {
            var useText = cbShowBlacklistIconWithText.isSelected();
            ApplicationConfiguration.getInstance().setToolbarBlacklistIconWithText(useText);
        });

        boolean useSystemDarkMode = ApplicationConfiguration.getInstance().getUseSystemDarkMode();
        cbUseSystemDarkMode.setSelected(useSystemDarkMode);
        cbUseSystemDarkMode.addActionListener(_ -> ApplicationConfiguration.getInstance()
                .setUseSystemDarkMode(cbUseSystemDarkMode.isSelected()));

        var useLongTimeFormat = ApplicationConfiguration.getInstance().getFilmTimeUseLongFormat();
        cbTabFilmeTimeUseLongFormat.setSelected(useLongTimeFormat);
        cbTabFilmeTimeUseLongFormat.addActionListener(_ -> {
            ApplicationConfiguration.getInstance().setFilmTimeUseLongFormat(cbTabFilmeTimeUseLongFormat.isSelected());
            MediathekGui.ui().repaint();
        });
    }

    private static final String NO_INFLUENCE_TEXT = "Einstellung hat unter macOS keine Auswirkung";

    private void setupTabSwitchListener() {
        if (!MediathekGui.ui().supportsAutomaticMenuTabSwitching()) {
            cbAutomaticMenuTabSwitching.setEnabled(false);
            cbAutomaticMenuTabSwitching.setToolTipText(NO_INFLUENCE_TEXT);
            ApplicationConfiguration.getInstance().setInstallTabSwitchListener(false);
        } else {
            boolean installed = ApplicationConfiguration.getInstance().getInstallTabSwitchListener();
            cbAutomaticMenuTabSwitching.setSelected(installed);

            cbAutomaticMenuTabSwitching.addActionListener(_ -> {
                final boolean isOn = cbAutomaticMenuTabSwitching.isSelected();
                ApplicationConfiguration.getInstance().setInstallTabSwitchListener(isOn);
                final var eventType = isOn
                        ? InstallTabSwitchListenerEvent.INSTALL_TYPE.INSTALL
                        : InstallTabSwitchListenerEvent.INSTALL_TYPE.REMOVE;
                final InstallTabSwitchListenerEvent evt = new InstallTabSwitchListenerEvent(eventType);
                MessageBus.getMessageBus().publishAsync(evt);
            });
        }
    }

    // <editor-fold defaultstate="collapsed" desc="Generated Code">//GEN-BEGIN:initComponents
    // Generated using JFormDesigner non-commercial license
    private void initComponents() {
        var jPanel5 = new JPanel();
        jCheckBoxTabsTop = new JCheckBox();
        jCheckBoxTabIcon = new JCheckBox();
        cbAutomaticMenuTabSwitching = new JCheckBox();
        cbRestoreSelectedTab = new JCheckBox();
        var jPanel3 = new JPanel();
        var jLabel3 = new JLabel();
        jtfUserAgent = new JTextField();
        var jPanel4 = new JPanel();
        var jLabel4 = new JLabel();
        jtfProxyHost = new JTextField();
        var jLabel5 = new JLabel();
        jtfProxyPort = new JTextField();
        var jLabel7 = new JLabel();
        jtfProxyUser = new JTextField();
        var jLabel8 = new JLabel();
        jpfProxyPassword = new JPasswordField();
        jButtonApplyProxySettings = new JButton();
        var panel1 = new JPanel();
        jCheckBoxTray = new JCheckBox();
        cbShowBlacklistIconWithText = new JCheckBox();
        cbUseWikipediaSenderLogos = new JCheckBox();
        cbAutomaticUpdateChecks = new JCheckBox();
        cbDrawListIconsRight = new JCheckBox();
        cbUseSystemDarkMode = new JCheckBox();
        cbTabFilmeTimeUseLongFormat = new JCheckBox();
        modernSearchTitlePanel = new JXTitledPanel();

        //======== this ========
        setMaximumSize(new Dimension(10, 10));

        //======== jPanel5 ========
        {
            jPanel5.setBorder(new TitledBorder("Tab-Verhalten"));
            jPanel5.setLayout(new MigLayout(
                new LC().insets("0").hideMode(3).gridGap("5", "5"),
                // columns
                new AC()
                    .fill().gap()
                    .fill().gap()
                    .fill(),
                // rows
                new AC()
                    .gap()
                    .fill()));

            //---- jCheckBoxTabsTop ----
            jCheckBoxTabsTop.setText("Tabs oben anzeigen");
            jPanel5.add(jCheckBoxTabsTop, new CC().cell(0, 0));

            //---- jCheckBoxTabIcon ----
            jCheckBoxTabIcon.setText("Icons anzeigen");
            jCheckBoxTabIcon.setToolTipText("Im Tab keine Icons anzeigen");
            jPanel5.add(jCheckBoxTabIcon, new CC().cell(1, 0));

            //---- cbAutomaticMenuTabSwitching ----
            cbAutomaticMenuTabSwitching.setText("Tabs schalten automatisch bei Men\u00fcnutzung um");
            jPanel5.add(cbAutomaticMenuTabSwitching, new CC().cell(2, 0));

            //---- cbRestoreSelectedTab ----
            cbRestoreSelectedTab.setText("Letzte Auswahl beim Start wiederherstellen");
            cbRestoreSelectedTab.setToolTipText("Wenn gew\u00e4hlt wird beim Start des Programms automatisch das zuletzt genutzte Tab aktiviert.");
            jPanel5.add(cbRestoreSelectedTab, new CC().cell(0, 1, 3, 1));
        }

        //======== jPanel3 ========
        {
            jPanel3.setBorder(new TitledBorder("Download"));

            //---- jLabel3 ----
            jLabel3.setText("User-Agent:");

            //---- jtfUserAgent ----
            jtfUserAgent.setMinimumSize(new Dimension(200, 26));
            jtfUserAgent.setPreferredSize(new Dimension(520, 26));

            GroupLayout jPanel3Layout = new GroupLayout(jPanel3);
            jPanel3.setLayout(jPanel3Layout);
            jPanel3Layout.setHorizontalGroup(
                jPanel3Layout.createParallelGroup()
                    .addGroup(jPanel3Layout.createSequentialGroup()
                        .addGap(5, 5, 5)
                        .addComponent(jLabel3)
                        .addGap(5, 5, 5)
                        .addComponent(jtfUserAgent, GroupLayout.PREFERRED_SIZE, GroupLayout.DEFAULT_SIZE, GroupLayout.PREFERRED_SIZE)
                        .addContainerGap(GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
            );
            jPanel3Layout.setVerticalGroup(
                jPanel3Layout.createParallelGroup()
                    .addGroup(jPanel3Layout.createSequentialGroup()
                        .addGap(10, 10, 10)
                        .addComponent(jLabel3))
                    .addGroup(jPanel3Layout.createSequentialGroup()
                        .addGap(5, 5, 5)
                        .addComponent(jtfUserAgent, GroupLayout.PREFERRED_SIZE, GroupLayout.DEFAULT_SIZE, GroupLayout.PREFERRED_SIZE))
            );
        }

        //======== jPanel4 ========
        {
            jPanel4.setBorder(new TitledBorder("HTTP-Proxy"));
            jPanel4.setToolTipText("");

            //---- jLabel4 ----
            jLabel4.setText("Host:");

            //---- jLabel5 ----
            jLabel5.setText("Port:");

            //---- jLabel7 ----
            jLabel7.setText("User:");

            //---- jLabel8 ----
            jLabel8.setText("Passwort:");

            //---- jButtonApplyProxySettings ----
            jButtonApplyProxySettings.setText("Proxy übernehmen");

            GroupLayout jPanel4Layout = new GroupLayout(jPanel4);
            jPanel4.setLayout(jPanel4Layout);
            jPanel4Layout.setHorizontalGroup(
                jPanel4Layout.createParallelGroup()
                    .addGroup(jPanel4Layout.createSequentialGroup()
                        .addContainerGap()
                        .addGroup(jPanel4Layout.createParallelGroup(GroupLayout.Alignment.LEADING, false)
                            .addGroup(jPanel4Layout.createSequentialGroup()
                                .addComponent(jLabel4)
                                .addPreferredGap(LayoutStyle.ComponentPlacement.RELATED)
                                .addComponent(jtfProxyHost, GroupLayout.PREFERRED_SIZE, 250, GroupLayout.PREFERRED_SIZE))
                            .addGroup(jPanel4Layout.createSequentialGroup()
                                .addComponent(jLabel7)
                                .addPreferredGap(LayoutStyle.ComponentPlacement.RELATED)
                                .addComponent(jtfProxyUser)))
                        .addPreferredGap(LayoutStyle.ComponentPlacement.UNRELATED)
                        .addGroup(jPanel4Layout.createParallelGroup(GroupLayout.Alignment.TRAILING)
                            .addComponent(jLabel8)
                            .addComponent(jLabel5))
                        .addPreferredGap(LayoutStyle.ComponentPlacement.RELATED)
                        .addGroup(jPanel4Layout.createParallelGroup()
                            .addGroup(jPanel4Layout.createSequentialGroup()
                                .addComponent(jtfProxyPort, GroupLayout.PREFERRED_SIZE, 72, GroupLayout.PREFERRED_SIZE)
                                .addGap(0, 187, Short.MAX_VALUE))
                            .addComponent(jpfProxyPassword))
                        .addContainerGap())
                    .addGroup(GroupLayout.Alignment.TRAILING, jPanel4Layout.createSequentialGroup()
                        .addContainerGap(GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                        .addComponent(jButtonApplyProxySettings)
                        .addContainerGap())
            );
            jPanel4Layout.setVerticalGroup(
                jPanel4Layout.createParallelGroup()
                    .addGroup(jPanel4Layout.createSequentialGroup()
                        .addContainerGap()
                        .addGroup(jPanel4Layout.createParallelGroup(GroupLayout.Alignment.BASELINE)
                            .addComponent(jLabel4)
                            .addComponent(jtfProxyHost, GroupLayout.PREFERRED_SIZE, GroupLayout.DEFAULT_SIZE, GroupLayout.PREFERRED_SIZE)
                            .addComponent(jLabel5)
                            .addComponent(jtfProxyPort, GroupLayout.PREFERRED_SIZE, GroupLayout.DEFAULT_SIZE, GroupLayout.PREFERRED_SIZE))
                        .addPreferredGap(LayoutStyle.ComponentPlacement.RELATED)
                        .addGroup(jPanel4Layout.createParallelGroup(GroupLayout.Alignment.BASELINE)
                            .addComponent(jLabel7)
                            .addComponent(jtfProxyUser, GroupLayout.PREFERRED_SIZE, GroupLayout.DEFAULT_SIZE, GroupLayout.PREFERRED_SIZE)
                            .addComponent(jLabel8)
                            .addComponent(jpfProxyPassword, GroupLayout.PREFERRED_SIZE, GroupLayout.DEFAULT_SIZE, GroupLayout.PREFERRED_SIZE))
                        .addPreferredGap(LayoutStyle.ComponentPlacement.RELATED)
                        .addComponent(jButtonApplyProxySettings)
                        .addContainerGap(GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
            );
        }

        //======== panel1 ========
        {
            panel1.setLayout(new MigLayout(
                new LC().insets("0").hideMode(3).gridGap("5", "0"),
                // columns
                new AC()
                    .grow().align("left").gap()
                    .fill(),
                // rows
                new AC()
                    .fill().gap()
                    .fill().gap()
                    .fill().gap()
                    .gap()
                    .fill().gap()
                    ));

            //---- jCheckBoxTray ----
            jCheckBoxTray.setText("Programm ins Tray minimieren");
            panel1.add(jCheckBoxTray, new CC().cell(0, 0));

            //---- cbShowBlacklistIconWithText ----
            cbShowBlacklistIconWithText.setText("Blacklist-Filter-Icon mit Text anzeigen");
            cbShowBlacklistIconWithText.setToolTipText("Neustart erforderlich");
            panel1.add(cbShowBlacklistIconWithText, new CC().cell(1, 0));

            //---- cbUseWikipediaSenderLogos ----
            cbUseWikipediaSenderLogos.setText("Senderlogos von Wikipedia verwenden");
            panel1.add(cbUseWikipediaSenderLogos, new CC().cell(0, 1));

            //---- cbAutomaticUpdateChecks ----
            cbAutomaticUpdateChecks.setText("Programmupdates t\u00e4glich suchen");
            panel1.add(cbAutomaticUpdateChecks, new CC().cell(0, 2));

            //---- cbDrawListIconsRight ----
            cbDrawListIconsRight.setText("Info-Icons der Listen rechts darstellen");
            panel1.add(cbDrawListIconsRight, new CC().cell(0, 3));

            //---- cbUseSystemDarkMode ----
            cbUseSystemDarkMode.setText("Erscheinungsbild des Betriebssystems verwenden");
            cbUseSystemDarkMode.setToolTipText("Stellt den Hell-/Dunkelmodus der App beim Programmstart nach den aktuellen Einstellungen des Betriebssystem ein.");
            panel1.add(cbUseSystemDarkMode, new CC().cell(0, 4, 2, 1));

            //---- cbTabFilmeTimeUseLongFormat ----
            cbTabFilmeTimeUseLongFormat.setText("Langes Zeitformat (HH:mm:ss) im Tab Filme f\u00fcr Zeit verwenden");
            panel1.add(cbTabFilmeTimeUseLongFormat, new CC().cell(0, 5));
        }

        //---- modernSearchTitlePanel ----
        modernSearchTitlePanel.setTitle("Moderne Suche");

        GroupLayout layout = new GroupLayout(this);
        setLayout(layout);
        layout.setHorizontalGroup(
            layout.createParallelGroup()
                .addGroup(layout.createSequentialGroup()
                    .addContainerGap()
                    .addGroup(layout.createParallelGroup()
                        .addComponent(jPanel3, GroupLayout.DEFAULT_SIZE, GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                        .addComponent(jPanel5, GroupLayout.Alignment.TRAILING, GroupLayout.DEFAULT_SIZE, GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                        .addGroup(layout.createSequentialGroup()
                            .addGroup(layout.createParallelGroup()
                                .addComponent(jPanel4, GroupLayout.PREFERRED_SIZE, GroupLayout.DEFAULT_SIZE, GroupLayout.PREFERRED_SIZE)
                                .addComponent(panel1, GroupLayout.PREFERRED_SIZE, GroupLayout.DEFAULT_SIZE, GroupLayout.PREFERRED_SIZE))
                            .addGap(0, 0, Short.MAX_VALUE))
                        .addComponent(modernSearchTitlePanel, GroupLayout.DEFAULT_SIZE, GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
                    .addContainerGap())
        );
        layout.setVerticalGroup(
            layout.createParallelGroup()
                .addGroup(layout.createSequentialGroup()
                    .addContainerGap()
                    .addComponent(jPanel5, GroupLayout.PREFERRED_SIZE, GroupLayout.DEFAULT_SIZE, GroupLayout.PREFERRED_SIZE)
                    .addPreferredGap(LayoutStyle.ComponentPlacement.RELATED)
                    .addComponent(jPanel3, GroupLayout.PREFERRED_SIZE, GroupLayout.DEFAULT_SIZE, GroupLayout.PREFERRED_SIZE)
                    .addPreferredGap(LayoutStyle.ComponentPlacement.RELATED)
                    .addComponent(jPanel4, GroupLayout.PREFERRED_SIZE, GroupLayout.DEFAULT_SIZE, GroupLayout.PREFERRED_SIZE)
                    .addPreferredGap(LayoutStyle.ComponentPlacement.RELATED)
                    .addComponent(panel1, GroupLayout.PREFERRED_SIZE, GroupLayout.DEFAULT_SIZE, GroupLayout.PREFERRED_SIZE)
                    .addPreferredGap(LayoutStyle.ComponentPlacement.RELATED)
                    .addComponent(modernSearchTitlePanel, GroupLayout.DEFAULT_SIZE, GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                    .addContainerGap())
        );
    }// </editor-fold>//GEN-END:initComponents
    // Variables declaration - do not modify//GEN-BEGIN:variables
    // Generated using JFormDesigner non-commercial license
    private JCheckBox jCheckBoxTabsTop;
    private JCheckBox jCheckBoxTabIcon;
    private JCheckBox cbAutomaticMenuTabSwitching;
    private JCheckBox cbRestoreSelectedTab;
    private JTextField jtfUserAgent;
    private JTextField jtfProxyHost;
    private JTextField jtfProxyPort;
    private JTextField jtfProxyUser;
    private JPasswordField jpfProxyPassword;
    private JButton jButtonApplyProxySettings;
    private JCheckBox jCheckBoxTray;
    private JCheckBox cbShowBlacklistIconWithText;
    private JCheckBox cbUseWikipediaSenderLogos;
    private JCheckBox cbAutomaticUpdateChecks;
    private JCheckBox cbDrawListIconsRight;
    private JCheckBox cbUseSystemDarkMode;
    private JCheckBox cbTabFilmeTimeUseLongFormat;
    private JXTitledPanel modernSearchTitlePanel;
    // End of variables declaration//GEN-END:variables
}
