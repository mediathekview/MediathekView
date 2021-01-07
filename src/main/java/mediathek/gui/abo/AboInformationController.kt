package mediathek.gui.abo

import javafx.fxml.FXML
import javafx.fxml.Initializable
import javafx.scene.control.Label
import mediathek.config.Daten
import mediathek.gui.messages.AboListChangedEvent
import mediathek.javafx.tool.JavaFxUtils
import net.engio.mbassy.listener.Handler
import java.net.URL
import java.util.*

class AboInformationController: Initializable {
    @FXML
    private lateinit var totalAbos: Label

    @FXML
    private lateinit var activeAbos: Label

    @FXML
    private lateinit var inactiveAbos: Label

    private var oldSize = -1
    private var oldActive: Long = -1
    private var oldInactive: Long = -1

    private fun updateTotalDisplay(gesamt: Int) {
        if (gesamt == 1) {
            totalAbos.text = "Gesamt: 1 Abo"
        } else {
            totalAbos.text = String.format("Gesamt: %d Abos", gesamt)
        }
    }

    private fun updateDisplayText() {
        val listeAbo = Daten.getInstance().listeAbo
        val _activeAbos = listeAbo.activeAbos()
        val _inactiveAbos = listeAbo.inactiveAbos()
        val gesamt = listeAbo.size
        if (gesamt != oldSize) {
            updateTotalDisplay(gesamt)
            oldSize = gesamt
        }
        if (_activeAbos != oldActive) {
            activeAbos.text = String.format("%d eingeschaltet", _activeAbos)
            oldActive = _activeAbos
        }
        if (_inactiveAbos != oldInactive) {
            inactiveAbos.text = String.format("%d ausgeschaltet", _inactiveAbos)
            oldInactive = _inactiveAbos
        }
    }

    @Handler
    private fun handleAboChangedEvent(e: AboListChangedEvent) {
        JavaFxUtils.invokeInFxThreadAndWait { updateDisplayText() }
    }

    override fun initialize(location: URL?, resources: ResourceBundle?) {
        Daten.getInstance().messageBus.subscribe(this)
        updateDisplayText()
    }
}