package mediathek.gui.dialogEinstellungen

import mediathek.config.application.ApplicationConfiguration
import mediathek.swing.SwingDispatch
import org.junit.jupiter.api.Assertions.*
import org.junit.jupiter.api.Test
import org.junit.jupiter.api.io.TempDir
import java.awt.Color
import java.awt.Component
import java.awt.Container
import java.nio.file.Files
import java.nio.file.Path
import javax.swing.JPanel
import javax.swing.JTextField
import javax.swing.UIManager

internal class PanelProgrammPfadeTest {
    @TempDir
    lateinit var tempDir: Path

    @Test
    fun visibilityFlagsAndConfiguredPathsInitializeThePanels() {
        val vlcPath = tempDir.resolve("vlc").toString()
        val ffmpegPath = tempDir.resolve("ffmpeg").toString()

        withProgramPaths(vlcPath, ffmpegPath) {
            SwingDispatch.runAndWait("Inspect program path settings") {
                val panel = PanelProgrammPfade(
                    parentComponent = null,
                    showVlcSettings = true,
                    showFFmpegSettings = false,
                )
                val vlcPanel = panel.component<JPanel>(PanelProgrammPfadeComponentNames.VLC_PANEL)
                val ffmpegPanel = panel.component<JPanel>(PanelProgrammPfadeComponentNames.FFMPEG_PANEL)

                assertTrue(vlcPanel.isVisible)
                assertFalse(ffmpegPanel.isVisible)
                assertEquals(vlcPath, panel.component<JTextField>(PanelProgrammPfadeComponentNames.VLC_PATH).text)
                assertEquals(ffmpegPath, panel.component<JTextField>(PanelProgrammPfadeComponentNames.FFMPEG_PATH).text)
            }
        }
    }

    @Test
    fun pathEditsPersistAndUpdateValidationColors() {
        val initialPath = Files.createFile(tempDir.resolve("initial")).toString()
        val existingPath = Files.createFile(tempDir.resolve("existing")).toString()
        val missingPath = tempDir.resolve("missing").toString()

        withProgramPaths(initialPath, initialPath) { config ->
            SwingDispatch.runAndWait("Edit program path settings") {
                val panel = PanelProgrammPfade(
                    parentComponent = null,
                    showVlcSettings = true,
                    showFFmpegSettings = true,
                )
                val vlcField = panel.component<JTextField>(PanelProgrammPfadeComponentNames.VLC_PATH)
                val ffmpegField = panel.component<JTextField>(PanelProgrammPfadeComponentNames.FFMPEG_PATH)

                vlcField.text = existingPath
                ffmpegField.text = missingPath

                assertEquals(existingPath, config.standardVlcPath)
                assertEquals(missingPath, config.standardFFmpegPath)
                assertEquals(UIManager.getDefaults().getColor(TEXT_FIELD_BACKGROUND_KEY), vlcField.background)
                assertEquals(INVALID_PATH_COLOR, ffmpegField.background)
            }
        }
    }

    private fun withProgramPaths(
        vlcPath: String,
        ffmpegPath: String,
        action: (ApplicationConfiguration) -> Unit,
    ) {
        val config = ApplicationConfiguration.getInstance()
        val previousVlcPath = config.standardVlcPath
        val previousFFmpegPath = config.standardFFmpegPath
        config.standardVlcPath = vlcPath
        config.standardFFmpegPath = ffmpegPath
        try {
            action(config)
        } finally {
            config.standardVlcPath = previousVlcPath
            config.standardFFmpegPath = previousFFmpegPath
        }
    }

    private inline fun <reified T : Component> PanelProgrammPfade.component(name: String): T =
        descendants().filterIsInstance<T>().single { it.name == name }

    private fun Component.descendants(): Sequence<Component> = sequence {
        yield(this@descendants)
        if (this@descendants is Container) {
            this@descendants.components.forEach { child -> yieldAll(child.descendants()) }
        }
    }

    private companion object {
        private const val TEXT_FIELD_BACKGROUND_KEY = "TextField.background"
        private val INVALID_PATH_COLOR = Color(255, 200, 200)
    }
}
