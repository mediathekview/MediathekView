package mediathek.javafx

import javafx.application.Platform
import javafx.event.EventHandler
import javafx.scene.control.Alert
import javafx.scene.control.Hyperlink
import javafx.scene.control.Label
import javafx.scene.layout.FlowPane
import mediathek.config.Konstanten
import mediathek.daten.GeoblockingField
import mediathek.gui.actions.UrlHyperlinkAction
import mediathek.mainwindow.MediathekGui
import mediathek.tool.ApplicationConfiguration
import org.apache.logging.log4j.LogManager
import java.net.URISyntaxException

class AustrianVlcCheck {
    private lateinit var alert: Alert
    private val config = ApplicationConfiguration.getConfiguration()

    private fun createLayout(): FlowPane {
        val fp = FlowPane()
        val lbl = Label(
            """
    Um den ORF erfolgreich zu nutzen sind zusätzlich Einstellungen erforderlich.
    Bitte lesen Sie sich hierzu folgenden Link durch: 
    """.trimIndent())
        val link = Hyperlink("Link zum Tutorial")
        fp.children.addAll(lbl, link)
        link.onAction = EventHandler {
            //disable display of dialog when user has clicked on the hyperlink
            config.setProperty(ApplicationConfiguration.APPLICATION_SHOW_ORF_CONFIG_HELP, false)
            try {
                UrlHyperlinkAction.openURL(MediathekGui.ui(), Konstanten.ORF_TUTORIAL_LINK)
            } catch (ignored: URISyntaxException) {
            }
            alert.close()
        }
        return fp
    }

    fun perform() {
        logger.trace("ORF setup tutorial display check started")
        if (config.getBoolean(ApplicationConfiguration.APPLICATION_SHOW_ORF_CONFIG_HELP, true)) {
            //we haven´t shown the config help dialog before
            val location = config.getString(ApplicationConfiguration.GEO_LOCATION, "")
            if (location == GeoblockingField.GEO_AT) {
                Platform.runLater {
                    val alert = Alert(Alert.AlertType.INFORMATION)
                    alert.title = Konstanten.PROGRAMMNAME
                    alert.headerText = "Wichtige Information für ORF"
                    alert.dialogPane.contentProperty().set(createLayout())
                    alert.show()
                }
            }
        }
        logger.trace("ORF setup tutorial display check finished")
    }

    companion object {
        private val logger = LogManager.getLogger()
    }
}