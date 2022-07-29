package mediathek.gui.dialogEinstellungen.allgemein;

import mediathek.gui.messages.*;
import mediathek.mainwindow.MediathekGui;
import mediathek.tool.ApplicationConfiguration;
import mediathek.tool.GuiFunktionen;
import mediathek.tool.MessageBus;
import mediathek.tool.sender_icon_cache.MVSenderIconCache;
import net.engio.mbassy.listener.Handler;
import org.apache.commons.configuration2.Configuration;
import org.apache.commons.lang3.SystemUtils;
import org.jdesktop.swingx.VerticalLayout;

import javax.swing.*;
import javax.swing.border.TitledBorder;
import java.awt.*;
import java.awt.event.ActionEvent;
import java.util.NoSuchElementException;

public class PanelEinstellungen extends JPanel {
    private final Configuration config = ApplicationConfiguration.getConfiguration();

    private void setupProxySettings() {

        jtfProxyHost.setText(config.getString(ApplicationConfiguration.HttpProxy.HOST, ""));
        var listener = new TextFieldConfigWriter(jtfProxyHost,ApplicationConfiguration.HttpProxy.HOST);
        jtfProxyHost.getDocument().addDocumentListener(new TimedDocumentListener(listener));

        jtfProxyPort.setText(config.getString(ApplicationConfiguration.HttpProxy.PORT, ""));
        listener = new TextFieldConfigWriter(jtfProxyPort,ApplicationConfiguration.HttpProxy.PORT);
        jtfProxyPort.getDocument().addDocumentListener(new TimedDocumentListener(listener));

        jtfProxyUser.setText(config.getString(ApplicationConfiguration.HttpProxy.USER, ""));
        listener = new TextFieldConfigWriter(jtfProxyUser,ApplicationConfiguration.HttpProxy.USER);
        jtfProxyUser.getDocument().addDocumentListener(new TimedDocumentListener(listener));

        jpfProxyPassword.setText(config.getString(ApplicationConfiguration.HttpProxy.PASSWORD, ""));
        listener = new TextFieldConfigWriter(jpfProxyPassword,ApplicationConfiguration.HttpProxy.PASSWORD);
        jpfProxyPassword.getDocument().addDocumentListener(new TimedDocumentListener(listener));
    }

    private void setupUserAgentSettings() {
        jtfUserAgent.setText(ApplicationConfiguration.getConfiguration().getString(ApplicationConfiguration.APPLICATION_USER_AGENT));
        var listener = new TextFieldConfigWriter(jtfUserAgent,ApplicationConfiguration.APPLICATION_USER_AGENT);
        jtfUserAgent.getDocument().addDocumentListener(new TimedDocumentListener(listener));
    }

    private void cbUseWikipediaSenderLogosActionPerformed(ActionEvent evt) {
        ApplicationConfiguration.getConfiguration().setProperty(MVSenderIconCache.CONFIG_USE_LOCAL_SENDER_ICONS,!cbUseWikipediaSenderLogos.isSelected());
        MessageBus.getMessageBus().publishAsync(new SenderIconStyleChangedEvent());
    }
    
    private void cbAutomaticUpdateChecksActionPerformed(ActionEvent evt) {
        ApplicationConfiguration.getConfiguration().setProperty(ApplicationConfiguration.CONFIG_AUTOMATIC_UPDATE_CHECK, cbAutomaticUpdateChecks.isSelected());
        MessageBus.getMessageBus().publishAsync(new UpdateStateChangedEvent(cbAutomaticUpdateChecks.isSelected()));
    }

    private void setupTabUI() {
        final boolean tabPositionTop = config.getBoolean(ApplicationConfiguration.APPLICATION_UI_TAB_POSITION_TOP, true);
        jCheckBoxTabsTop.setSelected(tabPositionTop);
        jCheckBoxTabsTop.addActionListener(ae -> {
            config.setProperty(ApplicationConfiguration.APPLICATION_UI_TAB_POSITION_TOP, jCheckBoxTabsTop.isSelected());
            MessageBus.getMessageBus().publishAsync(new TabVisualSettingsChangedEvent());
        });

        var config = ApplicationConfiguration.getConfiguration();
        jCheckBoxTabIcon.setSelected(config.getBoolean(ApplicationConfiguration.APPLICATION_UI_MAINWINDOW_TAB_ICONS,false));
        jCheckBoxTabIcon.addActionListener(ae -> {
            config.setProperty(ApplicationConfiguration.APPLICATION_UI_MAINWINDOW_TAB_ICONS, jCheckBoxTabIcon.isSelected());
            MessageBus.getMessageBus().publishAsync(new TabVisualSettingsChangedEvent());
        });
    }

    @Handler
    private void handleTrayIconEvent(TrayIconEvent e) {
        SwingUtilities.invokeLater(() -> jCheckBoxTray.setSelected(config.getBoolean(ApplicationConfiguration.APPLICATION_UI_USE_TRAY,false)));
    }

    private void setupTray() {
        if (SystemUtils.IS_OS_MAC_OSX) {
            jCheckBoxTray.setSelected(false);
            jCheckBoxTray.setEnabled(false);
        } else {
            MessageBus.getMessageBus().subscribe(this);

            jCheckBoxTray.setSelected(config.getBoolean(ApplicationConfiguration.APPLICATION_UI_USE_TRAY,false));
            jCheckBoxTray.addActionListener(ae -> {
                config.setProperty(ApplicationConfiguration.APPLICATION_UI_USE_TRAY,jCheckBoxTray.isSelected());
                MediathekGui.ui().initializeSystemTray();
            });
        }
    }

    public PanelEinstellungen() {
        super();
        initComponents();

        setupUserAgentSettings();

        setupProxySettings();

        setupTabUI();

        setupTray();

        setupTabSwitchListener();

        cbUseWikipediaSenderLogos.addActionListener(this::cbUseWikipediaSenderLogosActionPerformed);
        final boolean useLocalSenderLogos = ApplicationConfiguration.getConfiguration().getBoolean(MVSenderIconCache.CONFIG_USE_LOCAL_SENDER_ICONS,false);
        cbUseWikipediaSenderLogos.setSelected(!useLocalSenderLogos);
        
        cbAutomaticUpdateChecks.addActionListener(this::cbAutomaticUpdateChecksActionPerformed);
        cbAutomaticUpdateChecks.setSelected(ApplicationConfiguration.getConfiguration().getBoolean(ApplicationConfiguration.CONFIG_AUTOMATIC_UPDATE_CHECK,true));
        if (GuiFunktionen.isUsingExternalUpdater()) {
            cbAutomaticUpdateChecks.setEnabled(false);
            cbAutomaticUpdateChecks.setToolTipText("Diese Option ist deaktiviert, da ein externer Updater verwendet wird.");
        }
    }

    private void setupTabSwitchListener() {
        if (SystemUtils.IS_OS_MAC_OSX) {
            //deactivated on OS X
            cbAutomaticMenuTabSwitching.setEnabled(false);
            config.setProperty(ApplicationConfiguration.APPLICATION_INSTALL_TAB_SWITCH_LISTENER, false);
        } else {
            boolean installed;
            try {
                installed = config.getBoolean(ApplicationConfiguration.APPLICATION_INSTALL_TAB_SWITCH_LISTENER);
            } catch (NoSuchElementException ex) {
                installed = true;
                config.setProperty(ApplicationConfiguration.APPLICATION_INSTALL_TAB_SWITCH_LISTENER, true);
            }
            cbAutomaticMenuTabSwitching.setSelected(installed);

            cbAutomaticMenuTabSwitching.addActionListener(e -> {
                final boolean isOn = cbAutomaticMenuTabSwitching.isSelected();
                config.setProperty(ApplicationConfiguration.APPLICATION_INSTALL_TAB_SWITCH_LISTENER, isOn);
                final InstallTabSwitchListenerEvent evt = new InstallTabSwitchListenerEvent();
                if (isOn) {
                    evt.event = InstallTabSwitchListenerEvent.INSTALL_TYPE.INSTALL;
                } else {
                    evt.event = InstallTabSwitchListenerEvent.INSTALL_TYPE.REMOVE;
                }
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
        var panel1 = new JPanel();
        jCheckBoxTray = new JCheckBox();
        cbUseWikipediaSenderLogos = new JCheckBox();
        cbAutomaticUpdateChecks = new JCheckBox();

        //======== this ========
        setMaximumSize(new Dimension(10, 10));

        //======== jPanel5 ========
        {
            jPanel5.setBorder(new TitledBorder("Tab-Verhalten")); //NON-NLS

            //---- jCheckBoxTabsTop ----
            jCheckBoxTabsTop.setText("Tabs oben anzeigen"); //NON-NLS

            //---- jCheckBoxTabIcon ----
            jCheckBoxTabIcon.setText("Icons anzeigen"); //NON-NLS
            jCheckBoxTabIcon.setToolTipText("Im Tab keine Icons anzeigen"); //NON-NLS

            //---- cbAutomaticMenuTabSwitching ----
            cbAutomaticMenuTabSwitching.setText("Tabs schalten automatisch bei Men\u00fcnutzung um"); //NON-NLS

            GroupLayout jPanel5Layout = new GroupLayout(jPanel5);
            jPanel5.setLayout(jPanel5Layout);
            jPanel5Layout.setHorizontalGroup(
                jPanel5Layout.createParallelGroup()
                    .addGroup(jPanel5Layout.createSequentialGroup()
                        .addGap(5, 5, 5)
                        .addComponent(jCheckBoxTabsTop)
                        .addGap(5, 5, 5)
                        .addComponent(jCheckBoxTabIcon)
                        .addGap(5, 5, 5)
                        .addComponent(cbAutomaticMenuTabSwitching)
                        .addContainerGap(GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
            );
            jPanel5Layout.setVerticalGroup(
                jPanel5Layout.createParallelGroup()
                    .addGroup(jPanel5Layout.createSequentialGroup()
                        .addGap(5, 5, 5)
                        .addGroup(jPanel5Layout.createParallelGroup()
                            .addComponent(jCheckBoxTabsTop)
                            .addComponent(jCheckBoxTabIcon)
                            .addComponent(cbAutomaticMenuTabSwitching)))
            );
        }

        //======== jPanel3 ========
        {
            jPanel3.setBorder(new TitledBorder("Download")); //NON-NLS

            //---- jLabel3 ----
            jLabel3.setText("User-Agent:"); //NON-NLS

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
            jPanel4.setBorder(new TitledBorder("HTTP-Proxy (Neustart erforderlich!)")); //NON-NLS
            jPanel4.setToolTipText(""); //NON-NLS

            //---- jLabel4 ----
            jLabel4.setText("Host:"); //NON-NLS

            //---- jLabel5 ----
            jLabel5.setText("Port:"); //NON-NLS

            //---- jLabel7 ----
            jLabel7.setText("User:"); //NON-NLS

            //---- jLabel8 ----
            jLabel8.setText("Passwort:"); //NON-NLS

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
                        .addContainerGap(GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
            );
        }

        //======== panel1 ========
        {
            panel1.setLayout(new VerticalLayout());

            //---- jCheckBoxTray ----
            jCheckBoxTray.setText("Programm ins Tray minimieren"); //NON-NLS
            panel1.add(jCheckBoxTray);

            //---- cbUseWikipediaSenderLogos ----
            cbUseWikipediaSenderLogos.setText("Senderlogos von Wikipedia verwenden"); //NON-NLS
            panel1.add(cbUseWikipediaSenderLogos);

            //---- cbAutomaticUpdateChecks ----
            cbAutomaticUpdateChecks.setText("Programmupdates t\u00e4glich suchen"); //NON-NLS
            panel1.add(cbAutomaticUpdateChecks);
        }

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
                            .addGap(0, 17, Short.MAX_VALUE)))
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
                    .addContainerGap(5, Short.MAX_VALUE))
        );
    }// </editor-fold>//GEN-END:initComponents
    // Variables declaration - do not modify//GEN-BEGIN:variables
    // Generated using JFormDesigner non-commercial license
    private JCheckBox jCheckBoxTabsTop;
    private JCheckBox jCheckBoxTabIcon;
    private JCheckBox cbAutomaticMenuTabSwitching;
    private JTextField jtfUserAgent;
    private JTextField jtfProxyHost;
    private JTextField jtfProxyPort;
    private JTextField jtfProxyUser;
    private JPasswordField jpfProxyPassword;
    private JCheckBox jCheckBoxTray;
    private JCheckBox cbUseWikipediaSenderLogos;
    private JCheckBox cbAutomaticUpdateChecks;
    // End of variables declaration//GEN-END:variables
}
