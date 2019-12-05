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
    private Label activitySummary;
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
            totalAbos.setText("1 Abo");
        } else {
            totalAbos.setText(String.format("%d Abos", gesamt));
        }
    }

    private void updateDisplayText() {
        final var listeAbo = daten.getListeAbo();
        final long activeAbos = listeAbo.activeAbos();
        final long inactiveAbos = listeAbo.inactiveAbos();
        final int gesamt = listeAbo.size();

        if (gesamt != oldSize) {
            updateTotalDisplay(gesamt);

            oldSize = gesamt;
        }

        if ((activeAbos != oldActive) || (inactiveAbos != oldInactive)) {
            activitySummary.setText(String.format("%d eingeschaltet, %d ausgeschaltet", activeAbos, inactiveAbos));

            oldActive = activeAbos;
            oldInactive = inactiveAbos;
        }
    }

    @Handler
    private void handleTimerEvent(TimerEvent e) {
        JavaFxUtils.invokeInFxThreadAndWait(this::updateDisplayText);
    }
}
