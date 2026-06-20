package mediathek.gui.dialogEinstellungen.allgemein

import mediathek.config.application.ApplicationConfiguration
import mediathek.gui.messages.*
import mediathek.mainwindow.SettingsDialogHost
import mediathek.tool.GuiFunktionen
import mediathek.tool.MessageBus
import mediathek.tool.http.MVHttpClient
import mediathek.x11.DesktopEnvDetector
import net.engio.mbassy.listener.Handler
import org.apache.commons.lang3.SystemUtils
import javax.swing.SwingUtilities

class PanelEinstellungen(
    private val host: SettingsDialogHost,
) : PanelEinstellungenBase() {
    private var trayMessageBusEnabled = false
    private var subscribedToMessageBus = false

    init {
        setupModernSearchPanel()
        setupUserAgentSettings()
        setupProxySettings()
        setupTabUI()
        setupTray()
        setupTabSwitchListener()
        setupSenderLogoSettings()
        setupUpdateCheckSettings()
        setupRestoreSelectedTab()
        setupListIconSettings()
        setupToolbarSettings()
        setupSystemThemeSettings()
        setupFilmTimeFormatSettings()
    }

    private fun setupProxySettings() {
        val applicationConfiguration = ApplicationConfiguration.getInstance()

        jtfProxyHost.text = applicationConfiguration.httpProxyHost
        var listener = TextFieldConfigWriter(jtfProxyHost) { applicationConfiguration.httpProxyHost = it }
        jtfProxyHost.document.addDocumentListener(TimedDocumentListener(listener))

        jtfProxyPort.text = applicationConfiguration.httpProxyPort
        listener = TextFieldConfigWriter(jtfProxyPort) { applicationConfiguration.httpProxyPort = it }
        jtfProxyPort.document.addDocumentListener(TimedDocumentListener(listener))

        jtfProxyUser.text = applicationConfiguration.httpProxyUser
        listener = TextFieldConfigWriter(jtfProxyUser) { applicationConfiguration.httpProxyUser = it }
        jtfProxyUser.document.addDocumentListener(TimedDocumentListener(listener))

        jpfProxyPassword.text = applicationConfiguration.httpProxyPassword
        listener = TextFieldConfigWriter(jpfProxyPassword) { applicationConfiguration.httpProxyPassword = it }
        jpfProxyPassword.document.addDocumentListener(TimedDocumentListener(listener))

        jButtonApplyProxySettings.addActionListener { applyProxySettings() }
    }

    private fun applyProxySettings() {
        ApplicationConfiguration.getInstance().setHttpProxy(
            jtfProxyHost.text,
            jtfProxyPort.text,
            jtfProxyUser.text,
            String(jpfProxyPassword.password),
        )
        MVHttpClient.reloadProxySettings()
    }

    private fun setupUserAgentSettings() {
        val applicationConfiguration = ApplicationConfiguration.getInstance()
        jtfUserAgent.text = applicationConfiguration.userAgent
        val listener = TextFieldConfigWriter(jtfUserAgent, applicationConfiguration::setUserAgent)
        jtfUserAgent.document.addDocumentListener(TimedDocumentListener(listener))
    }

    private fun setupTabUI() {
        val applicationConfiguration = ApplicationConfiguration.getInstance()
        jCheckBoxTabsTop.isSelected = applicationConfiguration.tabPositionTop
        jCheckBoxTabsTop.addActionListener {
            applicationConfiguration.tabPositionTop = jCheckBoxTabsTop.isSelected
            MessageBus.messageBus.publishAsync(TabVisualSettingsChangedEvent())
        }
        if (SystemUtils.IS_OS_MAC_OSX) {
            jCheckBoxTabsTop.isEnabled = false
            jCheckBoxTabsTop.toolTipText = NO_INFLUENCE_TEXT
        }

        jCheckBoxTabIcon.isSelected = applicationConfiguration.mainWindowTabIcons
        jCheckBoxTabIcon.addActionListener {
            applicationConfiguration.mainWindowTabIcons = jCheckBoxTabIcon.isSelected
            MessageBus.messageBus.publishAsync(TabVisualSettingsChangedEvent())
        }
    }

    @Suppress("UNUSED_PARAMETER")
    @Handler
    private fun handleTrayIconEvent(event: TrayIconEvent) {
        SwingUtilities.invokeLater {
            jCheckBoxTray.isSelected = ApplicationConfiguration.getInstance().useTray
        }
    }

    private fun setupTray() {
        val applicationConfiguration = ApplicationConfiguration.getInstance()
        if (SystemUtils.IS_OS_MAC_OSX || !DesktopEnvDetector.trayIconSupported()) {
            trayMessageBusEnabled = false
            jCheckBoxTray.isSelected = false
            jCheckBoxTray.isEnabled = false
        } else {
            trayMessageBusEnabled = true
            jCheckBoxTray.isSelected = applicationConfiguration.useTray
            jCheckBoxTray.addActionListener {
                applicationConfiguration.useTray = jCheckBoxTray.isSelected
                host.refreshSystemTray()
            }
        }
    }

    override fun addNotify() {
        super.addNotify()
        subscribeToMessageBus()
    }

    private fun setupModernSearchPanel() {
        modernSearchTitlePanel.setContentContainer(ModernSearchConfigPanel())
    }

    override fun removeNotify() {
        unsubscribeFromMessageBus()
        super.removeNotify()
    }

    private fun subscribeToMessageBus() {
        if (trayMessageBusEnabled && !subscribedToMessageBus) {
            MessageBus.messageBus.subscribe(this)
            subscribedToMessageBus = true
        }
    }

    private fun unsubscribeFromMessageBus() {
        if (subscribedToMessageBus) {
            MessageBus.messageBus.unsubscribe(this)
            subscribedToMessageBus = false
        }
    }

    private fun setupSenderLogoSettings() {
        val applicationConfiguration = ApplicationConfiguration.getInstance()
        cbUseWikipediaSenderLogos.addActionListener {
            applicationConfiguration.localSenderIcons = !cbUseWikipediaSenderLogos.isSelected
            MessageBus.messageBus.publish(SenderIconStyleChangedEvent())
            host.repaintMainWindow()
        }
        cbUseWikipediaSenderLogos.isSelected = !applicationConfiguration.localSenderIcons
    }

    private fun setupUpdateCheckSettings() {
        val applicationConfiguration = ApplicationConfiguration.getInstance()
        cbAutomaticUpdateChecks.addActionListener {
            val selected = cbAutomaticUpdateChecks.isSelected
            applicationConfiguration.automaticUpdateCheck = selected
            MessageBus.messageBus.publishAsync(UpdateStateChangedEvent(selected))
        }
        cbAutomaticUpdateChecks.isSelected = applicationConfiguration.automaticUpdateCheck
        if (GuiFunktionen.isUsingExternalUpdater()) {
            cbAutomaticUpdateChecks.isEnabled = false
            cbAutomaticUpdateChecks.toolTipText = "Diese Option ist deaktiviert, da ein externer Updater verwendet wird."
        }
    }

    private fun setupRestoreSelectedTab() {
        val applicationConfiguration = ApplicationConfiguration.getInstance()
        cbRestoreSelectedTab.isSelected = applicationConfiguration.restoreSelectedTab
        cbRestoreSelectedTab.addActionListener {
            applicationConfiguration.restoreSelectedTab = cbRestoreSelectedTab.isSelected
        }
    }

    private fun setupListIconSettings() {
        val applicationConfiguration = ApplicationConfiguration.getInstance()
        cbDrawListIconsRight.isSelected = applicationConfiguration.listIconPositionRight
        cbDrawListIconsRight.addActionListener {
            applicationConfiguration.listIconPositionRight = cbDrawListIconsRight.isSelected
            host.repaintMainWindow()
        }
    }

    private fun setupToolbarSettings() {
        val applicationConfiguration = ApplicationConfiguration.getInstance()
        cbShowBlacklistIconWithText.isSelected = applicationConfiguration.toolbarBlacklistIconWithText
        cbShowBlacklistIconWithText.addActionListener {
            applicationConfiguration.toolbarBlacklistIconWithText = cbShowBlacklistIconWithText.isSelected
        }
    }

    private fun setupSystemThemeSettings() {
        val applicationConfiguration = ApplicationConfiguration.getInstance()
        cbUseSystemDarkMode.isSelected = applicationConfiguration.useSystemDarkMode
        cbUseSystemDarkMode.addActionListener {
            applicationConfiguration.useSystemDarkMode = cbUseSystemDarkMode.isSelected
        }
    }

    private fun setupFilmTimeFormatSettings() {
        val applicationConfiguration = ApplicationConfiguration.getInstance()
        cbTabFilmeTimeUseLongFormat.isSelected = applicationConfiguration.filmTimeUseLongFormat
        cbTabFilmeTimeUseLongFormat.addActionListener {
            applicationConfiguration.filmTimeUseLongFormat = cbTabFilmeTimeUseLongFormat.isSelected
            host.repaintMainWindow()
        }
    }

    private fun setupTabSwitchListener() {
        val applicationConfiguration = ApplicationConfiguration.getInstance()
        if (!host.supportsAutomaticMenuTabSwitching()) {
            cbAutomaticMenuTabSwitching.isEnabled = false
            cbAutomaticMenuTabSwitching.toolTipText = NO_INFLUENCE_TEXT
            applicationConfiguration.installTabSwitchListener = false
        } else {
            cbAutomaticMenuTabSwitching.isSelected = applicationConfiguration.installTabSwitchListener
            cbAutomaticMenuTabSwitching.addActionListener {
                val isOn = cbAutomaticMenuTabSwitching.isSelected
                applicationConfiguration.installTabSwitchListener = isOn
                val eventType = if (isOn) {
                    InstallTabSwitchListenerEvent.INSTALL_TYPE.INSTALL
                } else {
                    InstallTabSwitchListenerEvent.INSTALL_TYPE.REMOVE
                }
                MessageBus.messageBus.publishAsync(InstallTabSwitchListenerEvent(eventType))
            }
        }
    }

    private companion object {
        private const val NO_INFLUENCE_TEXT = "Einstellung hat unter macOS keine Auswirkung"
    }
}
