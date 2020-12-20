package mediathek.gui.actions

import mediathek.config.Konstanten
import mediathek.mainwindow.MediathekGui
import mediathek.tool.GuiFunktionen
import mediathek.tool.MVFunctionSys
import mediathek.tool.javafx.FXErrorDialog
import okio.buffer
import okio.sink
import org.apache.commons.lang3.SystemUtils
import org.apache.logging.log4j.LogManager
import java.awt.Desktop
import java.awt.event.ActionEvent
import java.io.File
import java.io.IOException
import java.nio.file.Files
import java.nio.file.Path
import java.nio.file.Paths
import javax.swing.AbstractAction
import javax.swing.JOptionPane

class SetAppMemoryAction : AbstractAction() {
    override fun actionPerformed(e: ActionEvent) {
        val fileName: String
        val optionsPath: Path
        if (SystemUtils.IS_OS_MAC_OSX) {
            //on macOS we cannot write into app bundle, we have to use a file in our config dir..
            fileName = MAC_VMOPTIONS_FILE
            optionsPath = Paths.get(fileName)
            if (Files.notExists(optionsPath)) {
                //create dummy file
                try {
                    optionsPath.sink().buffer().use { sink ->
                        sink.writeUtf8("### SEIEN SIE VORSICHTIG! FEHLERHAFTE EINTRÄGE VERHINDERN DEN START DER ANWENDUNG!\n")
                        sink.writeUtf8("-Xmx2G")
                    }
                } catch (ex: IOException) {
                    logger.error(ex)
                    FXErrorDialog.showErrorDialogWithoutParent(Konstanten.PROGRAMMNAME, "Schreibvorgang fehlgeschlagen",
                            """
                                Die Datei '${optionsPath.normalize().toAbsolutePath()}' konnte nicht geschrieben werden.
                                Bitte wenden Sie sich bei Fragen an das Forum!
                                """.trimIndent(), ex)
                }
            }
        } else {
            fileName = WIN_VMOPTIONS_FILE
            optionsPath = Paths.get(MVFunctionSys.getPathToApplicationJar(), fileName)
        }
        val fileStr = optionsPath.normalize().toAbsolutePath().toString()
        if (Desktop.isDesktopSupported()) {
            try {
                Desktop.getDesktop().open(optionsPath.normalize().toAbsolutePath().toFile())
            } catch (ex: Exception) {
                logger.error("Failed to open vm options file", ex)
                FXErrorDialog.showErrorDialogWithoutParent(Konstanten.PROGRAMMNAME, "Datei konnte nicht geöffnet werden",
                        """
                            Es trat ein Fehler beim Öffnen der Datei auf.
                            Diese Funktion wird nur durch die offiziellen MediathekView-Apps unterstützt.
                            Bitte wenden Sie sich mit dem unten sichtbaren Fehler an das Forum.
                            """.trimIndent(), ex)
            }
        } else {
            logger.error("Desktop is not supported")
            JOptionPane.showMessageDialog(MediathekGui.ui(), "<html>Datei konnte nicht geöffnet werden, da Java auf Ihrem System das Öffnen nicht unterstützt.<br>" +
                    "Bitte bearbeiten Sie manuell die Datei '" + fileStr + "'</html>")
        }
    }

    companion object {
        private const val WIN_VMOPTIONS_FILE = "MediathekView.vmoptions"
        private val MAC_VMOPTIONS_FILE = GuiFunktionen.getHomePath() + File.separator + ".mediathek3" + File.separator + "MediathekView_vmoptions.txt"
        private val logger = LogManager.getLogger()
    }

    init {
        putValue(NAME, "Speicherzuweisung ändern...")
    }
}