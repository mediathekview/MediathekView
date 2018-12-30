package mediathek.javafx;

import javafx.application.Platform;
import javafx.scene.control.Label;
import javafx.scene.control.Tooltip;
import javafx.scene.layout.HBox;
import mediathek.config.Daten;
import mediathek.gui.messages.TimerEvent;
import net.engio.mbassy.listener.Handler;


public class AboInformationPanel extends HBox {
    private final Daten daten;
    private final Label gesamtLabel = new Label();
    private final Label onOffLabel = new Label();
    private int oldSize = -1;
    private long oldActive = -1;
    private long oldInactive = -1;

    public AboInformationPanel(Daten daten) {
        super();
        this.daten = daten;

        getChildren().addAll(gesamtLabel,
                new VerticalSeparator(),
                onOffLabel);

        gesamtLabel.setTooltip(new Tooltip("Anzahl aller vorhandenen Abos"));
        daten.getMessageBus().subscribe(this);
    }

    private void updateTotalDisplay(final int gesamt) {
        if (gesamt == 1) {
            gesamtLabel.setText("1 Abo");
        } else {
            gesamtLabel.setText(String.format("%d Abos", gesamt));
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
            onOffLabel.setText(String.format("%d eingeschaltet, %d ausgeschaltet", activeAbos, inactiveAbos));

            oldActive = activeAbos;
            oldInactive = inactiveAbos;
        }
    }

    @Handler
    private void handleTimerEvent(TimerEvent e) {
        Platform.runLater(this::updateDisplayText);
    }
}
