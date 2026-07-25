package mediathek.gui.dialogEinstellungen

import mediathek.config.application.ApplicationConfiguration
import mediathek.gui.messages.ProgramLocationChangedEvent
import mediathek.swing.SwingDispatch
import mediathek.tool.MessageBus
import org.junit.jupiter.api.Assertions.*
import org.junit.jupiter.api.Test
import java.awt.Component
import java.awt.Container
import javax.swing.JCheckBox
import javax.swing.JPasswordField
import javax.swing.JTextField

internal class PanelEinstellungenErweitertTest {
    @Test
    fun configuredValuesInitializeAllEditableControls() {
        withAdvancedSettings {
            withPanel { panel ->
                assertTrue(panel.component<JCheckBox>(PanelEinstellungenErweitertComponentNames.SEARCH_SUBSCRIPTIONS_IMMEDIATELY).isSelected)
                assertFalse(panel.component<JCheckBox>(PanelEinstellungenErweitertComponentNames.START_DOWNLOADS_IMMEDIATELY).isSelected)
                assertEquals(
                    INITIAL_VALUES.directoryOpenProgram,
                    panel.component<JTextField>(PanelEinstellungenErweitertComponentNames.DIRECTORY_OPEN_PROGRAM).text
                )
                assertEquals(
                    INITIAL_VALUES.videoPlayerProgram,
                    panel.component<JTextField>(PanelEinstellungenErweitertComponentNames.VIDEO_PLAYER_PROGRAM).text
                )
                assertEquals(
                    INITIAL_VALUES.webBrowserProgram,
                    panel.component<JTextField>(PanelEinstellungenErweitertComponentNames.WEB_BROWSER_PROGRAM).text
                )
                assertEquals(
                    INITIAL_VALUES.linuxShutdownCommand,
                    panel.component<JTextField>(PanelEinstellungenErweitertComponentNames.LINUX_SHUTDOWN_COMMAND).text
                )
                assertEquals(
                    INITIAL_VALUES.jDownloaderUrl,
                    panel.component<JTextField>(PanelEinstellungenErweitertComponentNames.JDOWNLOADER_URL).text
                )
                assertEquals(
                    INITIAL_VALUES.pyLoadUrl,
                    panel.component<JTextField>(PanelEinstellungenErweitertComponentNames.PYLOAD_URL).text
                )
                assertEquals(
                    INITIAL_VALUES.pyLoadUser,
                    panel.component<JTextField>(PanelEinstellungenErweitertComponentNames.PYLOAD_USER).text
                )
                assertEquals(
                    INITIAL_VALUES.pyLoadPassword,
                    String(panel.component<JPasswordField>(PanelEinstellungenErweitertComponentNames.PYLOAD_CREDENTIAL_FIELD).password),
                )
            }
        }
    }

    @Test
    fun editingControlsPersistsEverySetting() {
        withAdvancedSettings { config ->
            withPanel { panel ->
                panel.component<JCheckBox>(PanelEinstellungenErweitertComponentNames.SEARCH_SUBSCRIPTIONS_IMMEDIATELY)
                    .doClick()
                panel.component<JCheckBox>(PanelEinstellungenErweitertComponentNames.START_DOWNLOADS_IMMEDIATELY)
                    .doClick()
                panel.component<JTextField>(PanelEinstellungenErweitertComponentNames.DIRECTORY_OPEN_PROGRAM).text =
                    UPDATED_VALUES.directoryOpenProgram
                panel.component<JTextField>(PanelEinstellungenErweitertComponentNames.VIDEO_PLAYER_PROGRAM).text =
                    UPDATED_VALUES.videoPlayerProgram
                panel.component<JTextField>(PanelEinstellungenErweitertComponentNames.WEB_BROWSER_PROGRAM).text =
                    UPDATED_VALUES.webBrowserProgram
                panel.component<JTextField>(PanelEinstellungenErweitertComponentNames.LINUX_SHUTDOWN_COMMAND).text =
                    UPDATED_VALUES.linuxShutdownCommand
                panel.component<JTextField>(PanelEinstellungenErweitertComponentNames.JDOWNLOADER_URL).text =
                    UPDATED_VALUES.jDownloaderUrl
                panel.component<JTextField>(PanelEinstellungenErweitertComponentNames.PYLOAD_URL).text =
                    UPDATED_VALUES.pyLoadUrl
                panel.component<JTextField>(PanelEinstellungenErweitertComponentNames.PYLOAD_USER).text =
                    UPDATED_VALUES.pyLoadUser
                panel.component<JPasswordField>(PanelEinstellungenErweitertComponentNames.PYLOAD_CREDENTIAL_FIELD).text =
                    UPDATED_VALUES.pyLoadPassword

                assertFalse(config.searchAbosImmediately)
                assertTrue(config.startDownloadsImmediately)
                assertEquals(UPDATED_VALUES.directoryOpenProgram, config.directoryOpenProgram)
                assertEquals(UPDATED_VALUES.videoPlayerProgram, config.videoPlayerProgram)
                assertEquals(UPDATED_VALUES.webBrowserProgram, config.webBrowserProgram)
                assertEquals(UPDATED_VALUES.linuxShutdownCommand, config.linuxShutdownCommand)
                assertEquals(UPDATED_VALUES.jDownloaderUrl, config.jDownloaderUrl)
                assertEquals(UPDATED_VALUES.pyLoadUrl, config.pyLoadUrl)
                assertEquals(UPDATED_VALUES.pyLoadUser, config.pyLoadUser)
                assertEquals(UPDATED_VALUES.pyLoadPassword, config.pyLoadPassword)
            }
        }
    }

    @Test
    fun programLocationChangeEventRefreshesProgramFields() {
        withAdvancedSettings { config ->
            var panel: PanelEinstellungenErweitert? = null
            try {
                SwingDispatch.runAndWait("Create advanced settings for program-location event") {
                    panel = PanelEinstellungenErweitert(null)
                    config.directoryOpenProgram = UPDATED_VALUES.directoryOpenProgram
                    config.videoPlayerProgram = UPDATED_VALUES.videoPlayerProgram
                    config.webBrowserProgram = UPDATED_VALUES.webBrowserProgram
                }

                MessageBus.messageBus.publish(ProgramLocationChangedEvent())

                SwingDispatch.runAndWait("Verify refreshed program locations") {
                    val initializedPanel = requireNotNull(panel)
                    assertEquals(
                        UPDATED_VALUES.directoryOpenProgram,
                        initializedPanel.component<JTextField>(
                            PanelEinstellungenErweitertComponentNames.DIRECTORY_OPEN_PROGRAM,
                        ).text,
                    )
                    assertEquals(
                        UPDATED_VALUES.videoPlayerProgram,
                        initializedPanel.component<JTextField>(
                            PanelEinstellungenErweitertComponentNames.VIDEO_PLAYER_PROGRAM,
                        ).text,
                    )
                    assertEquals(
                        UPDATED_VALUES.webBrowserProgram,
                        initializedPanel.component<JTextField>(
                            PanelEinstellungenErweitertComponentNames.WEB_BROWSER_PROGRAM,
                        ).text,
                    )
                }
            } finally {
                panel?.let { MessageBus.messageBus.unsubscribe(it) }
            }
        }
    }

    private fun withPanel(action: (PanelEinstellungenErweitert) -> Unit) {
        SwingDispatch.runAndWait("Inspect advanced settings") {
            val panel = PanelEinstellungenErweitert(null)
            try {
                action(panel)
            } finally {
                MessageBus.messageBus.unsubscribe(panel)
            }
        }
    }

    private fun withAdvancedSettings(action: (ApplicationConfiguration) -> Unit) {
        val config = ApplicationConfiguration.getInstance()
        val previousValues = config.advancedSettingsValues()
        config.apply(INITIAL_VALUES)
        try {
            action(config)
        } finally {
            config.apply(previousValues)
        }
    }

    private fun ApplicationConfiguration.advancedSettingsValues() = AdvancedSettingsValues(
        searchAbosImmediately = searchAbosImmediately,
        startDownloadsImmediately = startDownloadsImmediately,
        directoryOpenProgram = directoryOpenProgram,
        videoPlayerProgram = videoPlayerProgram,
        webBrowserProgram = webBrowserProgram,
        linuxShutdownCommand = linuxShutdownCommand,
        jDownloaderUrl = jDownloaderUrl,
        pyLoadUrl = pyLoadUrl,
        pyLoadUser = pyLoadUser,
        pyLoadPassword = pyLoadPassword,
    )

    private fun ApplicationConfiguration.apply(values: AdvancedSettingsValues) {
        searchAbosImmediately = values.searchAbosImmediately
        startDownloadsImmediately = values.startDownloadsImmediately
        directoryOpenProgram = values.directoryOpenProgram
        videoPlayerProgram = values.videoPlayerProgram
        webBrowserProgram = values.webBrowserProgram
        linuxShutdownCommand = values.linuxShutdownCommand
        jDownloaderUrl = values.jDownloaderUrl
        pyLoadUrl = values.pyLoadUrl
        pyLoadUser = values.pyLoadUser
        pyLoadPassword = values.pyLoadPassword
    }

    private inline fun <reified T : Component> PanelEinstellungenErweitert.component(name: String): T =
        descendants().filterIsInstance<T>().single { it.name == name }

    private fun Component.descendants(): Sequence<Component> = sequence {
        yield(this@descendants)
        if (this@descendants is Container) {
            this@descendants.components.forEach { child -> yieldAll(child.descendants()) }
        }
    }

    private data class AdvancedSettingsValues(
        val searchAbosImmediately: Boolean,
        val startDownloadsImmediately: Boolean,
        val directoryOpenProgram: String,
        val videoPlayerProgram: String,
        val webBrowserProgram: String,
        val linuxShutdownCommand: String,
        val jDownloaderUrl: String,
        val pyLoadUrl: String,
        val pyLoadUser: String,
        val pyLoadPassword: String,
    )

    private companion object {
        private val INITIAL_VALUES = AdvancedSettingsValues(
            searchAbosImmediately = true,
            startDownloadsImmediately = false,
            directoryOpenProgram = "/initial/file-manager",
            videoPlayerProgram = "/initial/video-player",
            webBrowserProgram = "/initial/browser",
            linuxShutdownCommand = "/initial/shutdown",
            jDownloaderUrl = "http://initial.invalid/jdownloader",
            pyLoadUrl = "http://initial.invalid/pyload",
            pyLoadUser = "initial-user",
            pyLoadPassword = listOf("initial", "value").joinToString("-"),
        )
        private val UPDATED_VALUES = AdvancedSettingsValues(
            searchAbosImmediately = false,
            startDownloadsImmediately = true,
            directoryOpenProgram = "/updated/file-manager",
            videoPlayerProgram = "/updated/video-player",
            webBrowserProgram = "/updated/browser",
            linuxShutdownCommand = "/updated/shutdown",
            jDownloaderUrl = "http://updated.invalid/jdownloader",
            pyLoadUrl = "http://updated.invalid/pyload",
            pyLoadUser = "updated-user",
            pyLoadPassword = listOf("updated", "value").joinToString("-"),
        )
    }
}
