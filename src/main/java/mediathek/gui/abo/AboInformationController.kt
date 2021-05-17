package mediathek.gui.abo

import javafx.fxml.FXML
import javafx.fxml.Initializable
import javafx.scene.control.Label
import mediathek.config.Daten
import mediathek.gui.messages.AboListChangedEvent
import mediathek.javafx.tool.JavaFxUtils
import mediathek.tool.MessageBus
import net.engio.mbassy.listener.Handler
import java.net.URL
import java.util.*

class AboInformationController : Initializable {
    @FXML
    private lateinit var totalAbos: Label

    @FXML
    private lateinit var activeAbos: Label

    @FXML
    private lateinit var inactiveAbos: Label

    private fun updateDisplayText() {
        val listeAbo = Daten.getInstance().listeAbo
        val numAbos = listeAbo.size

        if (numAbos == 1) {
            totalAbos.text = "Gesamt: 1 Abo"
        } else {
            totalAbos.text = "Gesamt: $numAbos Abos"
        }
        activeAbos.text = "${listeAbo.activeAbos()} eingeschaltet"
        inactiveAbos.text = "${listeAbo.inactiveAbos()} ausgeschaltet"
    }

    @Handler
    @Suppress("UNUSED_PARAMETER")
    private fun handleAboChangedEvent(e: AboListChangedEvent) {
        JavaFxUtils.invokeInFxThreadAndWait { updateDisplayText() }
    }

    override fun initialize(location: URL?, resources: ResourceBundle?) {
        MessageBus.messageBus.subscribe(this)
        updateDisplayText()
    }
}