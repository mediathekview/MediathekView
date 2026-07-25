package mediathek.gui.dialogEinstellungen

import mediathek.config.Konstanten
import mediathek.config.application.ApplicationConfiguration
import mediathek.swing.SwingDispatch
import org.junit.jupiter.api.Assertions.*
import org.junit.jupiter.api.Test
import java.awt.Component
import java.awt.Container
import javax.swing.JCheckBox
import javax.swing.JSpinner

internal class PanelDownloadTest {
    @Test
    fun checkboxesReflectDownloadSettings() {
        withDownloadSettings(
            showDownloadErrorMessage = true,
            fetchMissingDownloadFileSize = true,
        ) {
            SwingDispatch.runAndWait("Create PanelDownload and inspect checkboxes") {
                val panel = PanelDownload()

                assertTrue(panel.checkBox(PanelDownloadComponentNames.SHOW_DOWNLOAD_ERROR_MESSAGE).isSelected)
                assertFalse(panel.checkBox(PanelDownloadComponentNames.PLAY_SOUND_AFTER_DOWNLOAD).isSelected)
                assertTrue(panel.checkBox(PanelDownloadComponentNames.FETCH_MISSING_FILE_SIZE).isSelected)
            }
        }
    }

    @Test
    fun checkboxClicksUpdateDownloadSettings() {
        withDownloadSettings(
            showDownloadErrorMessage = false,
            fetchMissingDownloadFileSize = false,
        ) { config ->
            SwingDispatch.runAndWait("Click PanelDownload checkboxes") {
                val panel = PanelDownload()

                panel.checkBox(PanelDownloadComponentNames.SHOW_DOWNLOAD_ERROR_MESSAGE).doClick()
                panel.checkBox(PanelDownloadComponentNames.PLAY_SOUND_AFTER_DOWNLOAD).doClick()
                panel.checkBox(PanelDownloadComponentNames.FETCH_MISSING_FILE_SIZE).doClick()
            }

            assertTrue(config.showDownloadErrorMessage)
            assertTrue(config.playSoundAfterDownload)
            assertTrue(config.fetchMissingDownloadFileSize)
        }
    }

    @Test
    fun continuationSpinnerUsesDefaultForInvalidSettingAndPersistsChanges() {
        val config = ApplicationConfiguration.getInstance()
        val previousValue = config.downloadContinuationTime
        config.downloadContinuationTime = 0
        try {
            SwingDispatch.runAndWait("Update PanelDownload continuation spinner") {
                val panel = PanelDownload()
                val spinner = panel.component<JSpinner>(PanelDownloadComponentNames.DOWNLOAD_CONTINUATION_TIME)

                assertEquals(Konstanten.DOWNLOAD_CONTINUATION_DEFAULT_TIME, spinner.value)
                spinner.value = 7
            }

            assertEquals(7, config.downloadContinuationTime)
        } finally {
            config.downloadContinuationTime = previousValue
        }
    }

    private fun withDownloadSettings(
        showDownloadErrorMessage: Boolean,
        fetchMissingDownloadFileSize: Boolean,
        action: (ApplicationConfiguration) -> Unit,
    ) {
        val config = ApplicationConfiguration.getInstance()
        val previousShowDownloadErrorMessage = config.showDownloadErrorMessage
        val previousPlaySoundAfterDownload = config.playSoundAfterDownload
        val previousFetchMissingDownloadFileSize = config.fetchMissingDownloadFileSize
        config.showDownloadErrorMessage = showDownloadErrorMessage
        config.playSoundAfterDownload = false
        config.fetchMissingDownloadFileSize = fetchMissingDownloadFileSize
        try {
            action(config)
        } finally {
            config.showDownloadErrorMessage = previousShowDownloadErrorMessage
            config.playSoundAfterDownload = previousPlaySoundAfterDownload
            config.fetchMissingDownloadFileSize = previousFetchMissingDownloadFileSize
        }
    }

    private fun PanelDownload.checkBox(name: String): JCheckBox = component(name)

    private inline fun <reified T : Component> PanelDownload.component(name: String): T =
        descendants().filterIsInstance<T>().single { it.name == name }

    private fun Component.descendants(): Sequence<Component> = sequence {
        yield(this@descendants)
        if (this@descendants is Container) {
            this@descendants.components.forEach { child -> yieldAll(child.descendants()) }
        }
    }

}
