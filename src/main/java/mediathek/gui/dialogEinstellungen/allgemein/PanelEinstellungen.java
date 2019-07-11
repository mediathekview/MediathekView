package mediathek.gui.dialogEinstellungen.allgemein;

import mSearch.tool.ApplicationConfiguration;
import mediathek.MediathekGui;
import mediathek.config.Daten;
import mediathek.config.Icons;
import mediathek.config.MVConfig;
import mediathek.gui.PanelVorlage;
import mediathek.gui.dialog.DialogHilfe;
import mediathek.gui.messages.*;
import mediathek.tool.MVSenderIconCache;
import net.engio.mbassy.listener.Handler;
import org.apache.commons.configuration2.Configuration;
import org.apache.commons.lang3.SystemUtils;

import javax.swing.*;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;
import java.util.NoSuchElementException;

@SuppressWarnings("serial")
public class PanelEinstellungen extends PanelVorlage {
    private final static String ALLE = " Alle ";
    private final Configuration config = ApplicationConfiguration.getConfiguration();

    private void setupProxySettings() {

        jtfProxyHost.setText(config.getString(ApplicationConfiguration.HTTP_PROXY_HOSTNAME, ""));
        var listener = new TextFieldConfigWriter(jtfProxyHost,ApplicationConfiguration.HTTP_PROXY_HOSTNAME);
        jtfProxyHost.getDocument().addDocumentListener(new TimedDocumentListener(listener));

        jtfProxyPort.setText(config.getString(ApplicationConfiguration.HTTP_PROXY_PORT, ""));
        listener = new TextFieldConfigWriter(jtfProxyPort,ApplicationConfiguration.HTTP_PROXY_PORT);
        jtfProxyPort.getDocument().addDocumentListener(new TimedDocumentListener(listener));

        jtfProxyUser.setText(config.getString(ApplicationConfiguration.HTTP_PROXY_USERNAME, ""));
        listener = new TextFieldConfigWriter(jtfProxyUser,ApplicationConfiguration.HTTP_PROXY_USERNAME);
        jtfProxyUser.getDocument().addDocumentListener(new TimedDocumentListener(listener));

        jpfProxyPassword.setText(config.getString(ApplicationConfiguration.HTTP_PROXY_PASSWORD, ""));
        listener = new TextFieldConfigWriter(jpfProxyPassword,ApplicationConfiguration.HTTP_PROXY_PASSWORD);
        jpfProxyPassword.getDocument().addDocumentListener(new TimedDocumentListener(listener));
    }

    private void setupUserAgentSettings() {
        jtfUserAgent.setText(ApplicationConfiguration.getConfiguration().getString(ApplicationConfiguration.APPLICATION_USER_AGENT));
        var listener = new TextFieldConfigWriter(jtfUserAgent,ApplicationConfiguration.APPLICATION_USER_AGENT);
        jtfUserAgent.getDocument().addDocumentListener(new TimedDocumentListener(listener));
    }

    private void setupDays() {
        jButtonHelpDays.setIcon(Icons.ICON_BUTTON_HELP);
        jButtonHelpDays.addActionListener(e -> new DialogHilfe(parentComponent, true, '\n'
                + "Es werden nur Filme der letzten\n"
                + "xx Tage geladen."
                + '\n'
                + "Bei \"Alle\" werden alle Filme geladen.\n"
                + '\n'
                + "(Eine kleinere Filmliste\n"
                + "kann bei Rechnern mit wenig\n"
                + "Speicher hilfreich sein.)"
                + "\n\n"
                + "Auswirkung hat das erst nach dem\n"
                + "Neuladen der kompletten Filmliste.").setVisible(true));

        SpinnerListModel lm = new SpinnerListModel(new Object[]{ALLE, "1", "2", "3", "4", "5", "6", "7", "8", "9", "10",
                "12", "14", "16", "18", "20", "25", "30"});
        jSpinnerDays.setModel(lm);
        ((JSpinner.DefaultEditor) jSpinnerDays.getEditor()).getTextField().setEditable(false);
        initSpinner();
        jSpinnerDays.addChangeListener(new BeobSpinnerDays());
    }

    private void setupTabUI() {
        jCheckBoxTabsTop.setSelected(Boolean.parseBoolean(MVConfig.get(MVConfig.Configs.SYSTEM_TABS_TOP)));
        jCheckBoxTabsTop.addActionListener(ae -> {
            MVConfig.add(MVConfig.Configs.SYSTEM_TABS_TOP, Boolean.toString(jCheckBoxTabsTop.isSelected()));
            Daten.getInstance().getMessageBus().publishAsync(new TabVisualSettingsChangedEvent());
        });
        jCheckBoxTabIcon.setSelected(Boolean.parseBoolean(MVConfig.get(MVConfig.Configs.SYSTEM_TABS_ICON)));
        jCheckBoxTabIcon.addActionListener(ae -> {
            MVConfig.add(MVConfig.Configs.SYSTEM_TABS_ICON, Boolean.toString(jCheckBoxTabIcon.isSelected()));
            Daten.getInstance().getMessageBus().publishAsync(new TabVisualSettingsChangedEvent());
        });
    }

    @Handler
    private void handleTrayIconEvent(TrayIconEvent e) {
        SwingUtilities.invokeLater(() -> jCheckBoxTray.setSelected(Boolean.parseBoolean(MVConfig.get(MVConfig.Configs.SYSTEM_USE_TRAY))));
    }

    private void setupTray() {
        if (SystemUtils.IS_OS_MAC_OSX) {
            jCheckBoxTray.setSelected(false);
            jCheckBoxTray.setEnabled(false);
        } else {
            daten.getMessageBus().subscribe(this);

            jCheckBoxTray.setSelected(Boolean.parseBoolean(MVConfig.get(MVConfig.Configs.SYSTEM_USE_TRAY)));
            jCheckBoxTray.addActionListener(ae -> {
                MVConfig.add(MVConfig.Configs.SYSTEM_USE_TRAY, Boolean.toString(jCheckBoxTray.isSelected()));
                MediathekGui.ui().setTray();
            });
        }
    }

    private void setupDatabaseCleanerCheckbox() {
        final Configuration config = ApplicationConfiguration.getConfiguration();
        cbUseDatabaseCleaner.setSelected(config.getBoolean(ApplicationConfiguration.DATABASE_USE_CLEANER_INTERFACE, false));
        cbUseDatabaseCleaner.addActionListener(l -> config.setProperty(ApplicationConfiguration.DATABASE_USE_CLEANER_INTERFACE, cbUseDatabaseCleaner.isSelected()));
    }

    private void setupSaveHumanReadableFilmlistCheckbox() {
        final Configuration config = ApplicationConfiguration.getConfiguration();
        cbSaveHumanReadableFilmlist.setSelected(config.getBoolean(ApplicationConfiguration.FILMLISTE_SAVE_HUMAN_READABLE, false));
        cbSaveHumanReadableFilmlist.addActionListener(l -> config.setProperty(ApplicationConfiguration.FILMLISTE_SAVE_HUMAN_READABLE, cbSaveHumanReadableFilmlist.isSelected()));
    }

    public PanelEinstellungen(Daten d, JFrame parent) {
        super(d, parent);
        daten = d;

        initComponents();

        setupUserAgentSettings();

        setupProxySettings();

        setupDatabaseCleanerCheckbox();

        setupSaveHumanReadableFilmlistCheckbox();

        jButtonLoad.addActionListener(ae -> {
            daten.getListeFilme().clear(); // sonst wird evtl. nur eine Diff geladen
            daten.getFilmeLaden().loadFilmlist("");
        });

        setupDays();

        setupTabUI();

        setupTray();

        setupTabSwitchListener();

        final boolean useLocalSenderLogos = ApplicationConfiguration.getConfiguration().getBoolean(MVSenderIconCache.CONFIG_USE_LOCAL_SENDER_ICONS,false);
        cbUseWikipediaSenderLogos.setSelected(!useLocalSenderLogos);
    }

    @Handler
    private void handleParallelDownloadNumberChanged(ParallelDownloadNumberChangedEvent e) {
        SwingUtilities.invokeLater(this::initSpinner);
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
                daten.getMessageBus().publishAsync(evt);
            });
        }
    }

    private void initSpinner() {
        if (MVConfig.get(MVConfig.Configs.SYSTEM_ANZ_TAGE_FILMLISTE).isEmpty()) {
            MVConfig.add(MVConfig.Configs.SYSTEM_ANZ_TAGE_FILMLISTE, "0");
        }
        String s = MVConfig.get(MVConfig.Configs.SYSTEM_ANZ_TAGE_FILMLISTE);
        if (s.equals("0")) {
            s = ALLE;
        }
        jSpinnerDays.setValue(s);
    }

    private class BeobSpinnerDays implements ChangeListener {

        @Override
        public void stateChanged(ChangeEvent arg0) {
            String s = jSpinnerDays.getModel().getValue().toString();
            if (s.equals(ALLE)) {
                s = "0";
            }
            MVConfig.add(MVConfig.Configs.SYSTEM_ANZ_TAGE_FILMLISTE, s);
        }
    }

    // <editor-fold defaultstate="collapsed" desc="Generated Code">//GEN-BEGIN:initComponents
    private void initComponents() {

        javax.swing.JPanel jPanel5 = new javax.swing.JPanel();
        jCheckBoxTabsTop = new javax.swing.JCheckBox();
        jCheckBoxTabIcon = new javax.swing.JCheckBox();
        cbAutomaticMenuTabSwitching = new javax.swing.JCheckBox();
        javax.swing.JPanel jPanel3 = new javax.swing.JPanel();
        javax.swing.JLabel jLabel3 = new javax.swing.JLabel();
        jtfUserAgent = new javax.swing.JTextField();
        javax.swing.JPanel jPanel4 = new javax.swing.JPanel();
        javax.swing.JLabel jLabel4 = new javax.swing.JLabel();
        jtfProxyHost = new javax.swing.JTextField();
        javax.swing.JLabel jLabel5 = new javax.swing.JLabel();
        jtfProxyPort = new javax.swing.JTextField();
        javax.swing.JLabel jLabel7 = new javax.swing.JLabel();
        jtfProxyUser = new javax.swing.JTextField();
        javax.swing.JLabel jLabel8 = new javax.swing.JLabel();
        jpfProxyPassword = new javax.swing.JPasswordField();
        javax.swing.JPanel jPanel2 = new javax.swing.JPanel();
        javax.swing.JPanel jPanel6 = new javax.swing.JPanel();
        javax.swing.JLabel jLabel6 = new javax.swing.JLabel();
        jSpinnerDays = new javax.swing.JSpinner();
        jButtonLoad = new javax.swing.JButton();
        jButtonHelpDays = new javax.swing.JButton();
        javax.swing.JPanel jPanel7 = new javax.swing.JPanel();
        cbUseDatabaseCleaner = new javax.swing.JCheckBox();
        javax.swing.JPanel jPanel8 = new javax.swing.JPanel();
        cbSaveHumanReadableFilmlist = new javax.swing.JCheckBox();
        jCheckBoxTray = new javax.swing.JCheckBox();
        cbUseWikipediaSenderLogos = new javax.swing.JCheckBox();

        setMaximumSize(new java.awt.Dimension(10, 10));
        setMinimumSize(getMaximumSize());

        jPanel5.setBorder(javax.swing.BorderFactory.createTitledBorder("Tab-Verhalten"));

        jCheckBoxTabsTop.setText("Tabs oben anzeigen");

        jCheckBoxTabIcon.setText("Icons anzeigen");
        jCheckBoxTabIcon.setToolTipText("Im Tab keine Icons anzeigen");

        cbAutomaticMenuTabSwitching.setText("Tabs schalten automatisch bei Menünutzung um");

        javax.swing.GroupLayout jPanel5Layout = new javax.swing.GroupLayout(jPanel5);
        jPanel5.setLayout(jPanel5Layout);
        jPanel5Layout.setHorizontalGroup(
            jPanel5Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel5Layout.createSequentialGroup()
                .addGap(5, 5, 5)
                .addComponent(jCheckBoxTabsTop)
                .addGap(5, 5, 5)
                .addComponent(jCheckBoxTabIcon)
                .addGap(5, 5, 5)
                .addComponent(cbAutomaticMenuTabSwitching))
        );
        jPanel5Layout.setVerticalGroup(
            jPanel5Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel5Layout.createSequentialGroup()
                .addGap(5, 5, 5)
                .addGroup(jPanel5Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addComponent(jCheckBoxTabsTop)
                    .addComponent(jCheckBoxTabIcon)
                    .addComponent(cbAutomaticMenuTabSwitching)))
        );

        jPanel3.setBorder(javax.swing.BorderFactory.createTitledBorder("Download"));

        jLabel3.setText("User-Agent:");

        jtfUserAgent.setMinimumSize(new java.awt.Dimension(200, 26));
        jtfUserAgent.setPreferredSize(new java.awt.Dimension(520, 26));

        javax.swing.GroupLayout jPanel3Layout = new javax.swing.GroupLayout(jPanel3);
        jPanel3.setLayout(jPanel3Layout);
        jPanel3Layout.setHorizontalGroup(
            jPanel3Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel3Layout.createSequentialGroup()
                .addGap(5, 5, 5)
                .addComponent(jLabel3)
                .addGap(5, 5, 5)
                .addComponent(jtfUserAgent, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE))
        );
        jPanel3Layout.setVerticalGroup(
            jPanel3Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel3Layout.createSequentialGroup()
                .addGap(10, 10, 10)
                .addComponent(jLabel3))
            .addGroup(jPanel3Layout.createSequentialGroup()
                .addGap(5, 5, 5)
                .addComponent(jtfUserAgent, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE))
        );

        jPanel4.setBorder(javax.swing.BorderFactory.createTitledBorder("HTTP-Proxy (Neustart erforderlich!)"));
        jPanel4.setToolTipText("");

        jLabel4.setText("Host:");

        jLabel5.setText("Port:");

        jLabel7.setText("User:");

        jLabel8.setText("Passwort:");

        javax.swing.GroupLayout jPanel4Layout = new javax.swing.GroupLayout(jPanel4);
        jPanel4.setLayout(jPanel4Layout);
        jPanel4Layout.setHorizontalGroup(
            jPanel4Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel4Layout.createSequentialGroup()
                .addContainerGap()
                .addGroup(jPanel4Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING, false)
                    .addGroup(jPanel4Layout.createSequentialGroup()
                        .addComponent(jLabel4)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                        .addComponent(jtfProxyHost, javax.swing.GroupLayout.PREFERRED_SIZE, 250, javax.swing.GroupLayout.PREFERRED_SIZE))
                    .addGroup(jPanel4Layout.createSequentialGroup()
                        .addComponent(jLabel7)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                        .addComponent(jtfProxyUser)))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                .addGroup(jPanel4Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.TRAILING)
                    .addComponent(jLabel8)
                    .addComponent(jLabel5))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addGroup(jPanel4Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addGroup(jPanel4Layout.createSequentialGroup()
                        .addComponent(jtfProxyPort, javax.swing.GroupLayout.PREFERRED_SIZE, 72, javax.swing.GroupLayout.PREFERRED_SIZE)
                        .addGap(0, 160, Short.MAX_VALUE))
                    .addComponent(jpfProxyPassword))
                .addContainerGap())
        );
        jPanel4Layout.setVerticalGroup(
            jPanel4Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel4Layout.createSequentialGroup()
                .addContainerGap()
                .addGroup(jPanel4Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(jLabel4)
                    .addComponent(jtfProxyHost, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                    .addComponent(jLabel5)
                    .addComponent(jtfProxyPort, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addGroup(jPanel4Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(jLabel7)
                    .addComponent(jtfProxyUser, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                    .addComponent(jLabel8)
                    .addComponent(jpfProxyPassword, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE))
                .addContainerGap(javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
        );

        jPanel2.setBorder(javax.swing.BorderFactory.createTitledBorder(""));

        jLabel6.setText("nur die Filme der letzten Tage laden:");

        jSpinnerDays.setModel(new javax.swing.SpinnerListModel(new String[] {"Alles", "1", "2", "10", "15"}));

        jButtonLoad.setText("Filmliste jetzt neu laden");

        jButtonHelpDays.setIcon(new javax.swing.ImageIcon(getClass().getResource("/mediathek/res/muster/button-help.png"))); // NOI18N
        jButtonHelpDays.setToolTipText("Hilfe anzeigen");

        javax.swing.GroupLayout jPanel6Layout = new javax.swing.GroupLayout(jPanel6);
        jPanel6.setLayout(jPanel6Layout);
        jPanel6Layout.setHorizontalGroup(
            jPanel6Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel6Layout.createSequentialGroup()
                .addGap(5, 5, 5)
                .addComponent(jLabel6)
                .addGap(5, 5, 5)
                .addComponent(jSpinnerDays, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(jButtonLoad)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                .addComponent(jButtonHelpDays)
                .addContainerGap(javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
        );
        jPanel6Layout.setVerticalGroup(
            jPanel6Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel6Layout.createSequentialGroup()
                .addGroup(jPanel6Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addGroup(jPanel6Layout.createSequentialGroup()
                        .addGap(11, 11, 11)
                        .addComponent(jLabel6))
                    .addGroup(jPanel6Layout.createSequentialGroup()
                        .addGap(6, 6, 6)
                        .addGroup(jPanel6Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                            .addComponent(jButtonHelpDays)
                            .addGroup(jPanel6Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                                .addComponent(jSpinnerDays, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                                .addComponent(jButtonLoad)))))
                .addGap(2, 2, 2))
        );

        javax.swing.GroupLayout jPanel2Layout = new javax.swing.GroupLayout(jPanel2);
        jPanel2.setLayout(jPanel2Layout);
        jPanel2Layout.setHorizontalGroup(
            jPanel2Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel2Layout.createSequentialGroup()
                .addContainerGap()
                .addComponent(jPanel6, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addContainerGap(javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
        );
        jPanel2Layout.setVerticalGroup(
            jPanel2Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.TRAILING)
            .addGroup(jPanel2Layout.createSequentialGroup()
                .addGap(5, 5, 5)
                .addComponent(jPanel6, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE))
        );

        jPanel7.setBorder(javax.swing.BorderFactory.createTitledBorder("Datenbank (Neustart erforderlich!)"));

        cbUseDatabaseCleaner.setText("Bereinigung während Laufzeit");

        javax.swing.GroupLayout jPanel7Layout = new javax.swing.GroupLayout(jPanel7);
        jPanel7.setLayout(jPanel7Layout);
        jPanel7Layout.setHorizontalGroup(
            jPanel7Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel7Layout.createSequentialGroup()
                .addContainerGap()
                .addComponent(cbUseDatabaseCleaner)
                .addContainerGap(381, Short.MAX_VALUE))
        );
        jPanel7Layout.setVerticalGroup(
            jPanel7Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel7Layout.createSequentialGroup()
                .addContainerGap()
                .addComponent(cbUseDatabaseCleaner)
                .addContainerGap(javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
        );

        jPanel8.setBorder(javax.swing.BorderFactory.createTitledBorder("Speicherung der Filmliste"));

        cbSaveHumanReadableFilmlist.setText("in les- und editierbarem Format speichern");

        javax.swing.GroupLayout jPanel8Layout = new javax.swing.GroupLayout(jPanel8);
        jPanel8.setLayout(jPanel8Layout);
        jPanel8Layout.setHorizontalGroup(
            jPanel8Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel8Layout.createSequentialGroup()
                .addContainerGap()
                .addComponent(cbSaveHumanReadableFilmlist)
                .addContainerGap(300, Short.MAX_VALUE))
        );
        jPanel8Layout.setVerticalGroup(
            jPanel8Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanel8Layout.createSequentialGroup()
                .addContainerGap()
                .addComponent(cbSaveHumanReadableFilmlist)
                .addContainerGap(javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
        );

        jCheckBoxTray.setText("Programm ins Tray minimieren");

        cbUseWikipediaSenderLogos.setText("Senderlogos von Wikipedia verwenden");
        cbUseWikipediaSenderLogos.addActionListener(this::cbUseWikipediaSenderLogosActionPerformed);

        javax.swing.GroupLayout layout = new javax.swing.GroupLayout(this);
        this.setLayout(layout);
        layout.setHorizontalGroup(
            layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(layout.createSequentialGroup()
                .addContainerGap()
                .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addComponent(jPanel8, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                    .addComponent(jPanel7, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                    .addComponent(jPanel3, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                    .addComponent(jPanel5, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                    .addComponent(jCheckBoxTray)
                    .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.TRAILING, false)
                        .addComponent(jPanel2, javax.swing.GroupLayout.Alignment.LEADING, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                        .addComponent(jPanel4, javax.swing.GroupLayout.Alignment.LEADING, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
                    .addComponent(cbUseWikipediaSenderLogos))
                .addContainerGap(javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
        );
        layout.setVerticalGroup(
            layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(layout.createSequentialGroup()
                .addContainerGap()
                .addComponent(jPanel5, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(jPanel3, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(jPanel4, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(jPanel2, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(jPanel7, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(jPanel8, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(jCheckBoxTray)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(cbUseWikipediaSenderLogos)
                .addContainerGap(javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
        );
    }// </editor-fold>//GEN-END:initComponents

    private void cbUseWikipediaSenderLogosActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_cbUseWikipediaSenderLogosActionPerformed
        ApplicationConfiguration.getConfiguration().setProperty(MVSenderIconCache.CONFIG_USE_LOCAL_SENDER_ICONS,!cbUseWikipediaSenderLogos.isSelected());
        daten.getMessageBus().publishAsync(new SenderIconStyleChangedEvent());
    }//GEN-LAST:event_cbUseWikipediaSenderLogosActionPerformed


    // Variables declaration - do not modify//GEN-BEGIN:variables
    private javax.swing.JCheckBox cbAutomaticMenuTabSwitching;
    private javax.swing.JCheckBox cbSaveHumanReadableFilmlist;
    private javax.swing.JCheckBox cbUseDatabaseCleaner;
    private javax.swing.JCheckBox cbUseWikipediaSenderLogos;
    private javax.swing.JButton jButtonHelpDays;
    private javax.swing.JButton jButtonLoad;
    private javax.swing.JCheckBox jCheckBoxTabIcon;
    private javax.swing.JCheckBox jCheckBoxTabsTop;
    private javax.swing.JCheckBox jCheckBoxTray;
    private javax.swing.JSpinner jSpinnerDays;
    private javax.swing.JPasswordField jpfProxyPassword;
    private javax.swing.JTextField jtfProxyHost;
    private javax.swing.JTextField jtfProxyPort;
    private javax.swing.JTextField jtfProxyUser;
    private javax.swing.JTextField jtfUserAgent;
    // End of variables declaration//GEN-END:variables
}
