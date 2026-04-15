package mediathek.tool

import mediathek.config.MVConfig
import mediathek.gui.dialog.DialogProgrammOrdnerOeffnen
import mediathek.gui.messages.ProgramLocationChangedEvent
import org.apache.logging.log4j.LogManager
import java.awt.Desktop
import java.awt.Frame
import java.io.File
import javax.swing.JOptionPane

object DirOpenAction {
    private val logger = LogManager.getLogger()

    @JvmStatic
    fun zielordnerOeffnen(parent: Frame, ordner: String) {
        if (ordner.isEmpty()) {
            return
        }

        var success = false
        var directory: File? = null
        val programCall = arrayOf("", "")
        val normalizedOrdner = if (ordner.endsWith(File.separator)) ordner else ordner + File.separator

        try {
            directory = File(normalizedOrdner)
            if (!directory.exists()) {
                directory = directory.parentFile
            }

            val configuredProgram = configuredDirectoryOpener()
            if (configuredProgram.isNotEmpty()) {
                programCall[0] = configuredProgram
                programCall[1] = directory.absolutePath
                Runtime.getRuntime().exec(programCall)
                success = true
            } else if (Desktop.isDesktopSupported()) {
                val desktop = Desktop.getDesktop()
                if (desktop.isSupported(Desktop.Action.OPEN)) {
                    desktop.open(directory)
                    success = true
                }
            }
        } catch (openException: Exception) {
            try {
                val program = resolveDirectoryOpener(parent)
                if (directory != null) {
                    programCall[0] = program
                    programCall[1] = directory.absolutePath
                    Runtime.getRuntime().exec(programCall)

                    MVConfig.add(MVConfig.Configs.SYSTEM_ORDNER_OEFFNEN, program)
                    MessageBus.messageBus.publishAsync(ProgramLocationChangedEvent())
                    success = true
                }
            } catch (_: Exception) {
                logger.error("Ordner öffnen: {}", ordner)
                logger.error(openException)
            }
        } finally {
            if (!success) {
                MVConfig.add(MVConfig.Configs.SYSTEM_ORDNER_OEFFNEN, "")
                MessageBus.messageBus.publishAsync(ProgramLocationChangedEvent())
                MVMessageDialog.showMessageDialog(
                    parent,
                    "Kann den Dateimanager nicht öffnen!",
                    "Fehler",
                    JOptionPane.ERROR_MESSAGE,
                )
            }
        }
    }

    private fun resolveDirectoryOpener(parent: Frame): String {
        val configuredProgram = configuredDirectoryOpener()
        if (configuredProgram.isNotEmpty()) {
            return configuredProgram
        }

        val text = "\n Der Dateimanager zum Anzeigen des Speicherordners wird nicht gefunden.\n Dateimanager selbst auswählen."
        val dialog = DialogProgrammOrdnerOeffnen(parent, true, "", "Dateimanager suchen", text)
        dialog.isVisible = true
        return if (dialog.ok) dialog.ziel else ""
    }

    private fun configuredDirectoryOpener(): String =
        MVConfig.get(MVConfig.Configs.SYSTEM_ORDNER_OEFFNEN)
}
