package mediathek.gui.abo;

import javafx.fxml.FXML;
import javafx.scene.control.Label;
import mediathek.config.Daten;
import mediathek.gui.messages.TimerEvent;
import mediathek.javafx.tool.JavaFxUtils;
import net.engio.mbassy.listener.Handler;

public class AboInformationController {
    @FXML
    private Label totalAbos;

    @FXML
    private Label activeAbos;

    @FXML
    private Label inactiveAbos;

    private int oldSize = -1;
    private long oldActive = -1;
    private long oldInactive = -1;
    private Daten daten;

    public void startListener() {
        this.daten = Daten.getInstance();
        daten.getMessageBus().subscribe(this);
    }

    private void updateTotalDisplay(final int gesamt) {
        if (gesamt == 1) {
            totalAbos.setText("Gesamt: 1 Abo");
        } else {
            totalAbos.setText(String.format("Gesamt: %d Abos", gesamt));
        }
    }

    private void updateDisplayText() {
        final var listeAbo = daten.getListeAbo();
        final long _activeAbos = listeAbo.activeAbos();
        final long _inactiveAbos = listeAbo.inactiveAbos();
        final int gesamt = listeAbo.size();

        if (gesamt != oldSize) {
            updateTotalDisplay(gesamt);

            oldSize = gesamt;
        }

        if (_activeAbos != oldActive) {
            activeAbos.setText(String.format("%d eingeschaltet", _activeAbos));
            oldActive = _activeAbos;
        }

        if (_inactiveAbos != oldInactive) {
            inactiveAbos.setText(String.format("%d ausgeschaltet", _inactiveAbos));
            oldInactive = _inactiveAbos;
        }
    }

    @Handler
    private void handleTimerEvent(TimerEvent e) {
        JavaFxUtils.invokeInFxThreadAndWait(this::updateDisplayText);
    }
}
