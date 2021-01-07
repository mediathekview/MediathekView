package mediathek.javafx

import javafx.event.ActionEvent
import javafx.fxml.FXML
import javafx.fxml.FXMLLoader
import javafx.scene.control.Hyperlink
import javafx.scene.control.Label
import javafx.scene.layout.AnchorPane
import mediathek.config.Konstanten
import mediathek.gui.actions.UrlHyperlinkAction
import mediathek.mainwindow.MediathekGui
import org.apache.logging.log4j.LogManager
import java.io.IOException
import java.net.URI
import java.net.URISyntaxException
import javax.swing.SwingUtilities

class AboutController : AnchorPane() {
    @FXML
    private lateinit var homepage: Hyperlink

    @FXML
    private lateinit var serverDonationHyperlink: Hyperlink

    @FXML
    private lateinit var forum: Hyperlink

    @FXML
    private lateinit var anleitung: Hyperlink

    @FXML
    private lateinit var developerDonationHyperlink: Hyperlink

    @FXML
    private lateinit var version: Label

    @FXML
    private fun homepageLinkClicked(e: ActionEvent) {
        SwingUtilities.invokeLater {
            try {
                val uri = URI(Konstanten.ADRESSE_WEBSITE)
                UrlHyperlinkAction.openURL(MediathekGui.ui(), uri.toString())
            } catch (uriSyntaxException: URISyntaxException) {
                uriSyntaxException.printStackTrace()
            }
        }
    }

    @FXML
    private fun developerDonationLinkClicked(e: ActionEvent) {
        SwingUtilities.invokeLater {
            try {
                val uri = URI("https://paypal.me/ChristianFranzke")
                UrlHyperlinkAction.openURL(MediathekGui.ui(), uri.toString())
            } catch (uriSyntaxException: URISyntaxException) {
                uriSyntaxException.printStackTrace()
            }
        }
    }

    @FXML
    private fun serverDonationLinkClicked(e: ActionEvent) {
        SwingUtilities.invokeLater {
            try {
                val uri = URI(Konstanten.ADRESSE_DONATION)
                UrlHyperlinkAction.openURL(MediathekGui.ui(), uri.toString())
            } catch (uriSyntaxException: URISyntaxException) {
                uriSyntaxException.printStackTrace()
            }
        }
    }

    @FXML
    private fun forumLinkClicked(e: ActionEvent) {
        SwingUtilities.invokeLater {
            try {
                val uri = URI(Konstanten.ADRESSE_FORUM)
                UrlHyperlinkAction.openURL(MediathekGui.ui(), uri.toString())
            } catch (uriSyntaxException: URISyntaxException) {
                uriSyntaxException.printStackTrace()
            }
        }
    }

    @FXML
    private fun anleitungLinkClicked(e: ActionEvent) {
        SwingUtilities.invokeLater {
            try {
                val uri = URI(Konstanten.ADRESSE_ANLEITUNG)
                UrlHyperlinkAction.openURL(MediathekGui.ui(), uri.toString())
            } catch (uriSyntaxException: URISyntaxException) {
                uriSyntaxException.printStackTrace()
            }
        }
    }

    @FXML
    private fun jetbrainsLinkClicked(e: ActionEvent) {
        SwingUtilities.invokeLater {
            try {
                val uri = URI("https://www.jetbrains.com")
                UrlHyperlinkAction.openURL(MediathekGui.ui(), uri.toString())
            } catch (uriSyntaxException: URISyntaxException) {
                uriSyntaxException.printStackTrace()
            }
        }
    }

    @FXML
    private fun ejLinkClicked(e: ActionEvent) {
        SwingUtilities.invokeLater {
            try {
                val uri = URI("https://www.ej-technologies.com")
                UrlHyperlinkAction.openURL(MediathekGui.ui(), uri.toString())
            } catch (uriSyntaxException: URISyntaxException) {
                uriSyntaxException.printStackTrace()
            }
        }
    }

    init {
        try {
            val url = javaClass.getResource("/mediathek/res/programm/fxml/about.fxml")
            val fxmlLoader = FXMLLoader(url)
            fxmlLoader.setRoot(this)
            fxmlLoader.setController(this)
            fxmlLoader.load<Any>()
        } catch (e: IOException) {
            val logger = LogManager.getLogger()
            logger.error("Failed to load FXML!")
        }

        version.text = "Version ${Konstanten.VERSION_STR}"
    }
}